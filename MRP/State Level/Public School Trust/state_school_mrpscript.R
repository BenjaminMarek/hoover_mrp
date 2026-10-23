# =============================================================================
# MRP Estimation of State-Level Trust in Public Schools
# Data: YouGov Survey (2024), ACS 2023, CCES 2021-2024, FEC 2024 Results
# Adapted from Shiro Kuriwaki's MRP worklow (https://www.shirokuriwaki.com/ccesMRPprep/)
# Author: Ben Marek
# Last Updated: 2025-12-22
# =============================================================================

# Packages

library(here)
library(tidycensus)
library(tidyverse)
library(ccesMRPprep)
library(ccesMRPrun)
library(brms)
library(cmdstanr)
library(synthjoint)
library(scales)
library(furrr)
library(readxl)

# Data Prep

## Survey data
### Loads YouGov data
yougov_raw_df <- here("Data", "YouGov Surveys", "say-24-baseline-2024-11-21-baseline-6.csv") |> 
  read_csv()
### Recodes to match census demographics
yougov_clean_df <- yougov_raw_df |> 
  filter(gender4 %in% c(1,2)) |> # drops non-binary gender
  mutate(
    age_mrp = case_when(
      age4 == 1 ~ "Under 30",
      age4 == 2 ~ "30-44", 
      age4 == 3 ~ "45-64",
      age4 == 4 ~ "65+"),
    educ_mrp = case_when(
      educ4 == 1 ~ "HS or less",
      educ4 == 2 ~ "Some college",
      educ4 == 3 ~ "College grad",
      educ4 == 4 ~ "Postgrad"),
    race_mrp = case_when(
      hispanic == 1 ~ "Hispanic",
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 4 ~ "Asian",
      TRUE ~ "Other"),
    sex_mrp = case_when(
      gender4 == 1 ~ "Man",
      gender4 == 2 ~ "Woman"),
    pid3_mrp = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      pid3 %in% c(4, 5) ~ NA_character_  # drops others
  ))
### Collapses trust
yougov_cleaner_df <- yougov_clean_df |> 
  mutate(STATE = sprintf("%02d", inputstate),
    trust_schools_binary = case_when(
      institutional_trust_public_schools %in% c(1, 2) ~ 1,  # "A great deal" or "Quite a lot" = Trust
      institutional_trust_public_schools %in% c(3, 4) ~ 0,  # "Some" or "Very little" = Distrust
      TRUE ~ NA_real_  # Drop "Not sure", "skipped", "not asked"
    )
  ) |> 
  filter(complete.cases(age_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp, STATE))

## Base postrat frame
### Loads pums data from tidycensus. 
# pums_df <- get_pums(
#  c("STATE","RAC1P", "SCHL", "HISP", "AGEP", "SEX"),
#  state = "all",
#  year = 2023,
# survey = "acs5") # Redundant due to cached version

pums_df <- here("MRP", "State Level", "pums_2023_acs5_allstates.rds") |> 
  read_rds()
### Summarizes pums data to form celled postrat frame
poststrat_df <- pums_df |> 
  filter(complete.cases(STATE), AGEP >= 18) |> 
  mutate(
    age_mrp = case_when(
      AGEP < 30 ~ "Under 30",
      AGEP < 45 ~ "30-44", 
      AGEP < 65 ~ "45-64",
      AGEP >= 65 ~ "65+"),
    educ_mrp = case_when(
      SCHL %in% c("18", "19", "20") ~ "Some college",
      SCHL == "21" ~ "College grad",
      SCHL %in% c("22", "23", "24") ~ "Postgrad",
      TRUE ~ "HS or less"),
    race_mrp = case_when(
      HISP != "01" ~ "Hispanic",
      RAC1P == "1" ~ "White",
      RAC1P == "2" ~ "Black",
      RAC1P == "6" ~ "Asian",
      TRUE ~ "Other"),
    sex_mrp = case_when(
      SEX == "1" ~ "Man",
      SEX == "2" ~ "Woman")
  ) |> 
  group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
  summarize(count = sum(PWGTP)) |> 
  ungroup() 


## Synthetic Joint distribution for Partisanship: two methods compared
### - smoothfix: Raking to CCES marginals
### - bmlogit: Multinomial model with balancing constraints
### r = 0.99  at cell level, <1pp difference in state estimates

### CCES marginals
#### Loads CCES data
cces_cum_df <- here("Data", "CCES Surveys", "cumulative_2006-2024.rds") |> 
  read_rds()
#### Cleans CCES data
cces_partisan_df <- cces_cum_df |> 
  filter(year > 2020) |> 
  mutate(STATE = case_when(
    state == "Alabama" ~ "01",
    state == "Alaska" ~ "02",
    state == "Arizona" ~ "04",
    state == "Arkansas" ~ "05",
    state == "California" ~ "06",
    state == "Colorado" ~ "08",
    state == "Connecticut" ~ "09",
    state == "Delaware" ~ "10",
    state == "District of Columbia" ~ "11",
    state == "Florida" ~ "12",
    state == "Georgia" ~ "13",
    state == "Hawaii" ~ "15",
    state == "Idaho" ~ "16",
    state == "Illinois" ~ "17",
    state == "Indiana" ~ "18",
    state == "Iowa" ~ "19",
    state == "Kansas" ~ "20",
    state == "Kentucky" ~ "21",
    state == "Louisiana" ~ "22",
    state == "Maine" ~ "23",
    state == "Maryland" ~ "24",
    state == "Massachusetts" ~ "25",
    state == "Michigan" ~ "26",
    state == "Minnesota" ~ "27",
    state == "Mississippi" ~ "28",
    state == "Missouri" ~ "29",
    state == "Montana" ~ "30",
    state == "Nebraska" ~ "31",
    state == "Nevada" ~ "32",
    state == "New Hampshire" ~ "33",
    state == "New Jersey" ~ "34",
    state == "New Mexico" ~ "35",
    state == "New York" ~ "36",
    state == "North Carolina" ~ "37",
    state == "North Dakota" ~ "38",
    state == "Ohio" ~ "39",
    state == "Oklahoma" ~ "40",
    state == "Oregon" ~ "41",
    state == "Pennsylvania" ~ "42",
    state == "Rhode Island" ~ "44",
    state == "South Carolina" ~ "45",
    state == "South Dakota" ~ "46",
    state == "Tennessee" ~ "47",
    state == "Texas" ~ "48",
    state == "Utah" ~ "49",
    state == "Vermont" ~ "50",
    state == "Virginia" ~ "51",
    state == "Washington" ~ "53",
    state == "West Virginia" ~ "54",
    state == "Wisconsin" ~ "55",
    state == "Wyoming" ~ "56",
    TRUE ~ NA_character_
  ), pid3_mrp = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      TRUE ~ NA_character_  # drops others
    )) |> 
  filter(complete.cases(pid3_mrp), complete.cases(state))
#### Finds ACS voter state pop count
state_pops <- poststrat_df |> 
  group_by(STATE) |> 
  summarise(state_pop = sum(count)) |> 
  ungroup()
#### Counts partisans by state and scales these counts to ACS pop counts per state
cces_state_pid <-  cces_partisan_df |> 
  group_by(STATE, pid3_mrp) |> 
  summarise(s_count = sum(weight_cumulative)) |> 
  ungroup() |> 
  group_by(STATE) |> 
  mutate(proportion = s_count / sum(s_count)) |> 
  ungroup() |> 
  left_join(state_pops, by = "STATE") |> 
  mutate(count = proportion * state_pop) |> 
  select(STATE, pid3_mrp, count)

### Cleaning survey for synth
yougov_cleaner_synth <- yougov_cleaner_df |> 
  mutate(pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican"))) |> 
  filter(complete.cases(pid3_mrp)) |> 
  mutate(age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

### Trump vote totals
#### Loads state results from
state_results <- here("Data", "Election Results", "2024presgeresults.xlsx") |> 
  read_excel()
#### Computes z-value for Trump vote share (Claude-generated code on 12/18/25)
state_results <- state_results |> 
  filter(complete.cases(STATE)) |> 
  mutate(
    trump_share = TRUMP / (TRUMP + HARRIS), 
    trump_z = (trump_share - mean(trump_share)) / (2 * sd(trump_share)), 
    STATE_FIPS = case_match(STATE,
        "AL" ~ "01", "AK" ~ "02", "AZ" ~ "04", "AR" ~ "05", "CA" ~ "06", 
        "CO" ~ "08", "CT" ~ "09", "DE" ~ "10", "DC" ~ "11", "FL" ~ "12", 
        "GA" ~ "13", "HI" ~ "15", "ID" ~ "16", "IL" ~ "17", "IN" ~ "18", 
        "IA" ~ "19", "KS" ~ "20", "KY" ~ "21", "LA" ~ "22", "ME" ~ "23", 
        "MD" ~ "24", "MA" ~ "25", "MI" ~ "26", "MN" ~ "27", "MS" ~ "28", 
        "MO" ~ "29", "MT" ~ "30", "NE" ~ "31", "NV" ~ "32", "NH" ~ "33", 
        "NJ" ~ "34", "NM" ~ "35", "NY" ~ "36", "NC" ~ "37", "ND" ~ "38", 
        "OH" ~ "39", "OK" ~ "40", "OR" ~ "41", "PA" ~ "42", "RI" ~ "44", 
        "SC" ~ "45", "SD" ~ "46", "TN" ~ "47", "TX" ~ "48", "UT" ~ "49", 
        "VT" ~ "50", "VA" ~ "51", "WA" ~ "53", "WV" ~ "54", "WI" ~ "55", "WY" ~ "56",
        .default = NA
      )
  )
#### Joins z-score to YouGov data
yougov_cleaner_synth <- yougov_cleaner_synth |>
  left_join(state_results |> select(STATE_FIPS, trump_z), by = c("STATE" = "STATE_FIPS"))

# smoothfix treats CCES as truth and rakes
pidsynth <- synth_smoothfix(
  pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
  yougov_cleaner_synth,
  poststrat_df,
  fix_to = cces_state_pid,
  area_var = "STATE")
  
# bmlogit uses multinomial model with CCES as balancing constraint
plan(sequential)

states <- unique(poststrat_df$STATE)

pidsynth_bm <- map_dfr(states, function(st) {
  synth_bmlogit(
    pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
    yougov_cleaner_synth,
    poststrat_df |> filter(STATE == st),
    fix_to = cces_state_pid |> filter(STATE == st),
    area_var = "STATE"
  )
}, .progress = TRUE)

plan(default)

# MRP

## With partisanship
### Set formula w/ pid3
formula_partisan <- trust_schools_binary ~  sex_mrp + pid3_mrp + 
  (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|STATE) +
  (1 | STATE:educ_mrp) + (1 | STATE:race_mrp) + trump_z

### Multilevel regression w/ pid3
fit_mix_partisan <- brm(
  formula_partisan, 
  data = yougov_cleaner_synth,
  family = bernoulli(),
  prior = c(prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd"),   
    prior(normal(0, 1), class = "Intercept")),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12), 
  seed = 123
)

### Join Trump z-score (poststrat frame needs to match regression)
pidsynth <- pidsynth |>
  left_join(state_results |> select(STATE_FIPS, trump_z), by = c("STATE" = "STATE_FIPS"))
pidsynth_bm <- pidsynth_bm |>
  left_join(state_results |> select(STATE_FIPS, trump_z), by = c("STATE" = "STATE_FIPS"))

### smoothfix poststrat + summary
draws_pid_smfx <- poststrat_draws(fit_mix_partisan, poststrat_tgt = pidsynth, area_var = "STATE", new_levels = TRUE)
mrp_pid_smfx <- summ_sims(draws_pid_smfx, area_var = "STATE")

### bmlogit poststrat + summary
draws_pid_bm <- poststrat_draws(fit_mix_partisan, poststrat_tgt = pidsynth_bm, area_var = "STATE", new_levels = TRUE)
mrp_pid_bm <- summ_sims(draws_pid_bm, area_var = "STATE")


## Baseline (no partisanship)
### Set formula
formula_baseline <- trust_schools_binary ~  sex_mrp + 
  (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|STATE) +
  (1 | STATE:educ_mrp) + (1 | STATE:race_mrp)

### Multilevel Regression
fit_mix_baseline <- brm(
  formula_baseline, 
  data = yougov_cleaner_df,
  family = bernoulli(),
  prior = c(prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd"),   
    prior(normal(0, 1), class = "Intercept")),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12), 
  seed = 123
)

### Baseline poststrat + summary
final_baseline <- poststrat_draws(fit_mix_baseline, poststrat_tgt = poststrat_df, area_var = "STATE", new_levels = TRUE)
mrp_base <- summ_sims(final_baseline, area_var = "STATE")


# Final mrp values
mrp_base # baseline (no pid, just demographics)
mrp_pid_smfx # pid synthetic joint from smoothfix
mrp_pid_bm # pid synthetic joint from bmlogit

# Saving to wd
## Models
saveRDS(fit_mix_partisan, "state_pid_mr.rds")
saveRDS(fit_mix_baseline, "state_base_mr.rds")
## Poststrat frames
saveRDS(pidsynth_bm, "bmlogit_poststrat.rds")
saveRDS(pidsynth, "smoothfix_poststrat.rds")
saveRDS(poststrat_df, "baseline_poststrat.rds")
## MRP results
saveRDS(mrp_pid_bm, "mrp_pid_bm.rds")
saveRDS(mrp_pid_smfx, "mrp_pid_smfx.rds")
saveRDS(mrp_base, "mrp_base.rds")


# Validation
## Compare to direct weighted mean from YouGov survey

weighted_means <- yougov_cleaner_synth |>
  group_by(STATE) |>
  summarise(weighted_trust = weighted.mean(trust_schools_binary, weight, na.rm = TRUE), n = n())

# Correlation test: MRP vs weighted survey
cor.test(mrp_pid_bm$p_mrp_est, weighted_means$weighted_trust)

# Correlation test: MRP vs baseline
cor.test(mrp_pid_bm$p_mrp_est, mrp_base$p_mrp_est)

# Correlation test: smoothfix vs bmlogit
cor.test(mrp_pid_smfx$p_mrp_est, mrp_pid_bm$p_mrp_est)

# Shrinkage test (generated by Claude on 12/21/25)
validation <- data.frame(
  STATE = weighted_means$STATE,
  n = weighted_means$n,
  diff = mrp_pid_bm$p_mrp_est - weighted_means$weighted_trust)

cor(validation$n, abs(validation$diff))

# Vizualization
# Plot: MRP (bmlogit) vs weighted (generated by Claude on 12/21/25)
ggplot(mapping = aes(x = weighted_means$weighted_trust, y = mrp_pid_bm$p_mrp_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Weighted Survey Mean", y = "MRP Estimate (bmlogit)", 
       title = "MRP vs Weighted Survey Estimates") +
  theme_minimal()

# Plot: Partisan adjusted MRP (bmlogit) vs baseline (no pid) (generated by Claude on 12/21/25)
ggplot(mapping = aes(x = mrp_base$p_mrp_est, y = mrp_pid_bm$p_mrp_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Baseline (Demographics Only) MRP", y = "Partisan-Adjusted MRP", 
       title = "Effect of Partisan Adjustment on State Estimates") +
  theme_minimal()