# MRP: State-Level Trust in Congress (2024 Pooled YouGov)
# Ben Marek | January 2025

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

## Loading
yougov_raw_df <- bind_rows(
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20240320.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241025.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241121.rds"))) |> 
  group_by(person_id) |> 
  slice_tail(n = 1) |> 
  ungroup()

## MRP format
yougov_raw_df <- yougov_raw_df |> 
  filter(gender4 %in% c("Man", "Woman")) |>
  mutate(
    age_mrp = as.character(age4),
    educ_mrp = as.character(educ4),
    race_mrp = case_when(
      hispanic == "Yes" ~ "Hispanic",
      race == "White" ~ "White",
      race == "Black" ~ "Black",
      race == "Asian" ~ "Asian",
      TRUE ~ "Other"),
    sex_mrp = case_when(
      gender4 == "Man" ~ "Man",
      gender4 == "Woman" ~ "Woman"),
    pid3_mrp = case_when(
      pid3 == "Democrat" ~ "Democrat",
      pid3 == "Republican" ~ "Republican",
      pid3 == "Independent" ~ "Independent",
      TRUE ~ NA_character_)
  )

## Trust collapsing
yougov_cleaner_df <- yougov_raw_df |> 
  mutate(
    STATE = case_when(
      inputstate == "Alabama" ~ "01",
      inputstate == "Alaska" ~ "02",
      inputstate == "Arizona" ~ "04",
      inputstate == "Arkansas" ~ "05",
      inputstate == "California" ~ "06",
      inputstate == "Colorado" ~ "08",
      inputstate == "Connecticut" ~ "09",
      inputstate == "Delaware" ~ "10",
      inputstate == "District of Columbia" ~ "11",
      inputstate == "Florida" ~ "12",
      inputstate == "Georgia" ~ "13",
      inputstate == "Hawaii" ~ "15",
      inputstate == "Idaho" ~ "16",
      inputstate == "Illinois" ~ "17",
      inputstate == "Indiana" ~ "18",
      inputstate == "Iowa" ~ "19",
      inputstate == "Kansas" ~ "20",
      inputstate == "Kentucky" ~ "21",
      inputstate == "Louisiana" ~ "22",
      inputstate == "Maine" ~ "23",
      inputstate == "Maryland" ~ "24",
      inputstate == "Massachusetts" ~ "25",
      inputstate == "Michigan" ~ "26",
      inputstate == "Minnesota" ~ "27",
      inputstate == "Mississippi" ~ "28",
      inputstate == "Missouri" ~ "29",
      inputstate == "Montana" ~ "30",
      inputstate == "Nebraska" ~ "31",
      inputstate == "Nevada" ~ "32",
      inputstate == "New Hampshire" ~ "33",
      inputstate == "New Jersey" ~ "34",
      inputstate == "New Mexico" ~ "35",
      inputstate == "New York" ~ "36",
      inputstate == "North Carolina" ~ "37",
      inputstate == "North Dakota" ~ "38",
      inputstate == "Ohio" ~ "39",
      inputstate == "Oklahoma" ~ "40",
      inputstate == "Oregon" ~ "41",
      inputstate == "Pennsylvania" ~ "42",
      inputstate == "Rhode Island" ~ "44",
      inputstate == "South Carolina" ~ "45",
      inputstate == "South Dakota" ~ "46",
      inputstate == "Tennessee" ~ "47",
      inputstate == "Texas" ~ "48",
      inputstate == "Utah" ~ "49",
      inputstate == "Vermont" ~ "50",
      inputstate == "Virginia" ~ "51",
      inputstate == "Washington" ~ "53",
      inputstate == "West Virginia" ~ "54",
      inputstate == "Wisconsin" ~ "55",
      inputstate == "Wyoming" ~ "56",
      TRUE ~ NA_character_),
    trust_congress_binary = case_when(
      institutional_trust_congress %in% c("A great deal", "Quite a lot") ~ 1,
      institutional_trust_congress %in% c("Some", "Very little") ~ 0,
      TRUE ~ NA_real_)) |> 
  filter(complete.cases(age_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp, STATE, trust_congress_binary))

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
yougov_cleaner_synth <- yougov_cleaner_df |> 
  mutate(pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican"))) |> 
  filter(complete.cases(pid3_mrp)) |> 
  mutate(age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman"))) |> 
  left_join(state_results |> select(STATE_FIPS, trump_z), by = c("STATE" = "STATE_FIPS"))

# MRP Workflow

### Load in poststrat_df
poststrat <- here("MRP", "State Level", "Comprehensive Institutional Trust 2024", "2024_pooled_poststrat.rds") |> 
  read_rds()

# MRP
### Set formula
formula_congress <- trust_congress_binary ~  sex_mrp + pid3_mrp + 
  (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|STATE) +
  (1 | STATE:educ_mrp) + (1 | STATE:race_mrp) + trump_z

### Multilevel regression
fit_congress <- brm(
  formula_congress, 
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

### poststrat + summary
draws <- poststrat_draws(fit_congress, poststrat_tgt = poststrat, area_var = "STATE", new_levels = TRUE)
mrp_congress <- summ_sims(draws, area_var = "STATE")


# Final mrp values
mrp_congress # pid synthetic joint from smoothfix

# Saving to wd
## Models
saveRDS(fit_congress, "2024_pooled_congress_mr.rds")
## MRP results
saveRDS(mrp_congress, "mrp_pooled_congress.rds")





map_dfr(names(fits), function(inst) {
  vc <- VarCorr(fits[[inst]], summary = TRUE)
  
  tibble(
    institution = inst,
    sd_state = vc$STATE$sd[1, "Estimate"],
    sd_state_educ = vc$`STATE:educ_mrp`$sd[1, "Estimate"],
    sd_state_race = vc$`STATE:race_mrp`$sd[1, "Estimate"]
  )
})

# Merge trump_z to MRP estimates
mrp_with_trump <- mrp_estimates |> 
  left_join(state_results |> select(STATE_FIPS, trump_z), 
            by = c("STATE" = "STATE_FIPS"))

# Correlation by institution
mrp_with_trump |> 
  group_by(institution) |> 
  summarise(
    cor_with_trump = cor(p_mrp_est, trump_z),
    .groups = "drop"
  ) |> 
  arrange(cor_with_trump)

# You already have this
trump_posteriors |> 
  pivot_longer(everything(), names_to = "institution", values_to = "coef") |> 
  ggplot(aes(x = coef, fill = institution)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~institution, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "trump_z coefficient", title = "Posterior distributions")