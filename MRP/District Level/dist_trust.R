# MRP: District-Level Trust in Public Schools (2024 Pooled YouGov)
# Ben Marek | February 2025

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
library(haven)
library(educationdata)

## Loading
yougov_raw_df <- bind_rows(
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20240320.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241025.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241121.rds"))) |> 
  group_by(YouGovID) |> 
  slice_tail(n = 1) |> 
  ungroup()

dist_id <- here("Data", "YouGov Surveys", "say_districts.csv") |> 
  read.csv() |> 
  mutate(leaid = str_pad(as.character(leaid), width = 7, pad = "0"))

## MRP format
yougov_mrp_df <- yougov_raw_df |> 
  filter(gender4 %in% c("Man", "Woman")) |>
  mutate(
    age_mrp = as.character(age4),
    educ_mrp = as.character(educ4),
    race_mrp = case_when(
      hispanic == "Yes" ~ "Hispanic",
      race == "White" ~ "White",
      race == "Black" ~ "Black",
      TRUE ~ "Other"),
    sex_mrp = case_when(
      gender4 == "Man" ~ "Man",
      gender4 == "Woman" ~ "Woman"),
    pid3_mrp = case_when(
      pid3 == "Democrat" ~ "Democrat",
      pid3 == "Republican" ~ "Republican",
      pid3 == "Independent" ~ "Independent",
      TRUE ~ NA_character_)) |> 
  left_join(dist_id, by = "YouGovID") |> 
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
    trust_schools_binary = case_when(
      institutional_trust_public_schools %in% c("A great deal", "Quite a lot") ~ 1,
      institutional_trust_public_schools %in% c("Some", "Very little") ~ 0,
      TRUE ~ NA_real_)) |> 
  filter(complete.cases(age_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp, STATE, trust_schools_binary, leaid))




## District Level Covariates
dist_24_vote <- here("Data", "Election Results", "school_district_vote_shares_2016_2020_2024.dta") |> 
  read_dta()
dist_dem <- dist_24_vote |> # builds from 2020 if 2024 unavailable
  mutate(
    leaid = str_pad(as.character(sd_id), width = 7, pad = "0"),
    dem_plurality = coalesce(dem_plurality_allprecincts_2024, dem_plurality_allprecincts_2020)) |>
  select(leaid, dem_plurality)

dist_ppexp <- read_delim(here("Data", "NCES", "sdf22_1a.txt"), delim = "\t") |>
  mutate(
    leaid = str_pad(as.character(LEAID), width = 7, pad = "0"),
    ppexp = TCURELSC / MEMBERSCH) |>
  filter(ppexp > 0 & is.finite(ppexp)) |>
  mutate(log_ppexp = log(ppexp)) |>
  select(leaid, log_ppexp)

dist_directory <- get_education_data(
  level = "school-districts",
  source = "ccd",
  topic = "directory",
  filters = list(year = 2022),
  add_labels = TRUE) |>
  filter(enrollment > 0) |> 
  mutate(
    leaid = str_pad(as.character(leaid), width = 7, pad = "0"),
    log_enrollment = log(enrollment)) |>
  select(leaid, enrollment, log_enrollment)

dist_poverty <- get_education_data(
  level = "school-districts",
  source = "saipe",
  filters = list(year = 2021)) |> 
  mutate(leaid = str_pad(as.character(leaid), width = 7, pad = "0")) |>
  select(leaid, child_poverty_pct = est_population_5_17_poverty_pct)


dist_covariates <- dist_directory |>
  left_join(dist_dem, by = "leaid") |>
  left_join(dist_poverty, by = "leaid") |>
  left_join(dist_ppexp, by = "leaid") |>
  mutate(across(
    c(dem_plurality, child_poverty_pct, log_enrollment, log_ppexp),
    \(x) (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE)),
    .names = "{.col}_z"))


## State Level Trump Results
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




# Multilevel Regression

## Baseline (no district level covariates)
yougov_baseline_df <- yougov_mrp_df |> 
  mutate(pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican"))) |> 
  filter(complete.cases(pid3_mrp)) |> 
  mutate(age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman"))) |>
  left_join(state_results |> select(STATE_FIPS, trump_z), by = c("STATE" = "STATE_FIPS"))


formula_baseline <- trust_schools_binary ~ sex_mrp + pid3_mrp + 
  (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + (1 | STATE) + (1 | leaid) +
  (1 | STATE:pid3_mrp) + (1 | STATE:educ_mrp) + (1 | STATE:race_mrp) +
  pid3_mrp:trump_z + trump_z

fit_baseline <- brm(
  formula_baseline, 
  data = yougov_baseline_df,
  family = bernoulli(),
  prior = c(prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd"),   
    prior(normal(0, 1), class = "Intercept")),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12), 
  seed = 123)

saveRDS(fit_baseline, "baseline_dist.rds")


## District Level Covariates

yougov_covariates_df <- yougov_baseline_df |>
  left_join(dist_covariates, by = "leaid") |>
  filter(complete.cases(dem_plurality_z, child_poverty_pct_z, log_enrollment_z, log_ppexp_z))

formula_covariates <- trust_schools_binary ~ sex_mrp + pid3_mrp + (1 | age_mrp) + 
  (1 | educ_mrp) + (1 | race_mrp) + (1 | STATE) + (1 | leaid) +
  (1 | STATE:pid3_mrp) + (1 | STATE:educ_mrp) + (1 | STATE:race_mrp) +
  pid3_mrp:dem_plurality_z + dem_plurality_z +
  child_poverty_pct_z + log_enrollment_z + log_ppexp_z

fit_covariates <- brm(
  formula_covariates,
  data = yougov_covariates_df,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(normal(0, 1), class = "Intercept")
  ),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 123)

saveRDS(fit_covariates, "covariate_dist.rds")

### Loading in
fit_covariates <- here("MRP", "District Level", "covariate_dist.rds") |> 
  readRDS()



# Poststratification

## No PID Frame
poststrat_nopid <- here("MRP", "District Level", "poststrat_frame_nopid.rds") |> 
  readRDS()

## Modelling PID
pid_formula <- pid3_mrp ~ sex_mrp + (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + 
  (1 | STATE) + (1 | STATE:race_mrp) + (1 | STATE:educ_mrp) + dem_plurality_z

pid_model <- brm(pid_formula,
  family = categorical(link = "logit", refcat = "Independent"),
  data = yougov_covariates_df,
  backend = "cmdstanr",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12))

saveRDS(pid_model, "pid_model.rds")

## Predicting district level PID

district_ids <- unique(poststrat_nopid$leaid)
chunks <- split(district_ids, ceiling(seq_along(district_ids) / 100))

pid_results <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  chunk_data <- poststrat_nopid |> filter(leaid %in% chunks[[i]])

  pred_chunk <- fitted(
    pid_model,
    newdata = chunk_data,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    scale = "response",
    summary = TRUE
  )

  pid_probs <- pred_chunk[, "Estimate", ]
  colnames(pid_probs) <- c("Democrat", "Independent", "Republican")

  pid_results[[i]] <- chunk_data |>
    bind_cols(as_tibble(pid_probs))

  rm(pred_chunk, pid_probs, chunk_data)
  if (i %% 10 == 0) gc()

  message("PID chunk ", i, " of ", length(chunks))
}

poststrat_pid <- bind_rows(pid_results) |>
  pivot_longer(
    cols = c("Democrat", "Independent", "Republican"),
    names_to = "pid3_mrp",
    values_to = "pid_prob"
  ) |>
  mutate(N_cpj = count * pid_prob)

rm(pid_results, poststrat_nopid, pid_model)
gc()

saveRDS(poststrat_pid, "poststrat_pid.rds")


## Trust Predictions
district_ids <- unique(poststrat_pid$leaid)
chunks <- split(district_ids, ceiling(seq_along(district_ids) / 100))

results_list <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  chunk_data <- poststrat_pid |> filter(leaid %in% chunks[[i]])

  pred_trust <- fitted(
    fit_covariates,
    newdata = chunk_data,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    scale = "response",
    summary = TRUE
  )[, "Estimate"]

  chunk_data$pred_trust <- pred_trust

  results_list[[i]] <- chunk_data |>
    group_by(leaid) |>
    summarise(trust_hat = sum(N_cpj * pred_trust) / sum(N_cpj))

  rm(pred_trust, chunk_data)
  if (i %% 10 == 0) gc()

  message("Trust chunk ", i, " of ", length(chunks))
}

district_estimates <- bind_rows(results_list)
saveRDS(district_estimates, "district_trust_estimates.rds")





# Temp Validation
state_level <- here("MRP", "State Level", "Public Schools Revised", "mrp_pooled_pubsch_revised.rds") |> 
  readRDS()
district_estimates <- here("MRP", "District Level", "district_trust_estimates.rds") |> 
  readRDS()
poststrat_pid <- here("MRP", "District Level", "poststrat_pid.rds") |> 
  readRDS()

# ── 1. Basic distribution ─────────────────────────────────────────────────────
summary(district_estimates$trust_hat)
hist(district_estimates$trust_hat)
# Should be between 0 and 1, centered somewhere reasonable (probably 0.4-0.6)

# ── 2. Compare against raw survey means for well-observed districts ───────────
district_raw <- yougov_covariates_df |>
  group_by(leaid) |>
  summarise(
    raw_mean = mean(trust_schools_binary),
    n = n()
  ) |>
  filter(n >= 30)

validation <- district_raw |>
  left_join(district_estimates, by = "leaid")

cor(validation$raw_mean, validation$trust_hat)
# Should be positive and reasonably strong

plot(validation$raw_mean, validation$trust_hat,
     xlab = "Raw survey mean", ylab = "MRP estimate",
     main = paste("n =", nrow(validation), "districts with 30+ respondents"))
abline(0, 1, lty = 2)

# ── 3. State-level aggregation check ─────────────────────────────────────────
# MRP district estimates aggregated to state should match your
# existing state-level MRP estimates if you have them
state_from_districts <- district_estimates |>
  mutate(STATE = str_sub(leaid, 1, 2)) |>
  filter(!is.na(trust_hat)) |>
  left_join(
    poststrat_pid |>
      group_by(leaid) |>
      summarise(total_pop = sum(N_cpj)),
    by = "leaid"
  ) |>
  filter(!is.na(total_pop)) |>
  group_by(STATE) |>
  summarise(
    trust_state = weighted.mean(trust_hat, total_pop),
    n_districts = n(),
    coverage = sum(total_pop)
  )

# ── 4. Does trust correlate with partisanship as expected? ────────────────────
state_validation <- state_from_districts |>
  inner_join(state_level, by = "STATE")

district_check <- district_estimates |>
  left_join(
    dist_covariates |> select(leaid, dem_plurality_z),
    by = "leaid"
  )

# Correlations with and without Vermont
r_all <- cor(state_validation$trust_state, state_validation$p_mrp_est,
             use = "complete.obs")
r_no_vt <- state_validation |>
  filter(STATE != "50") |>
  with(cor(trust_state, p_mrp_est))

# State abbreviation labels
state_abbr <- tidycensus::fips_codes |>
  transmute(STATE = state_code, abbr = state) |>
  distinct()

state_validation_plot <- state_validation |>
  left_join(state_abbr, by = "STATE") |>
  mutate(is_vt = STATE == "50")

ggplot(state_validation_plot, aes(x = p_mrp_est, y = trust_state)) +
  geom_point(aes(color = is_vt), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  geom_text(
    data = filter(state_validation_plot, is_vt),
    aes(label = abbr), nudge_y = 0.008, size = 3.5
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
  annotate("text",
    x = min(state_validation_plot$p_mrp_est, na.rm = TRUE),
    y = max(state_validation_plot$trust_state, na.rm = TRUE),
    label = paste0("r = ", round(r_all, 3), " (all states excl. Vermont)\n"),
    hjust = 0, size = 4
  ) +
  labs(
    x = "State-Level MRP (direct estimate)",
    y = "Aggregated District Estimates",
    title = "Validation: District-Aggregated vs. Direct State MRP"
  ) +
  theme_minimal()


# District level map
library(tigris)
options(tigris_use_cache = TRUE)

state_fips <- fips_codes |>
  filter(as.numeric(state_code) <= 56, 
         !state_code %in% c("02", "15", "72")) |>
  distinct(state_code) |>
  pull(state_code)

districts_sf <- map(state_fips, \(st) {
  tryCatch(
    school_districts(state = st, type = "unified", year = 2023),
    error = \(e) NULL
  )
}) |>
  bind_rows()

elementary_sf <- map(state_fips, \(st) {
  tryCatch(
    school_districts(state = st, type = "elementary", year = 2023),
    error = \(e) NULL
  )
}) |>
  bind_rows()

secondary_sf <- map(state_fips, \(st) {
  tryCatch(
    school_districts(state = st, type = "secondary", year = 2023),
    error = \(e) NULL
  )
}) |>
  bind_rows()

districts_sf <- bind_rows(districts_sf, elementary_sf, secondary_sf) |>
  select(GEOID, geometry) |>
  rename(leaid = GEOID)

rm(elementary_sf, secondary_sf)
gc()

districts_sf <- districts_sf |>
  left_join(district_estimates, by = "leaid")

states_sf <- tigris::states(year = 2023, cb = TRUE) |>
  filter(STATEFP %in% state_fips)

trust_map <- ggplot() +
  geom_sf(data = districts_sf |> filter(!is.na(trust_hat)),
          aes(fill = trust_hat), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    name = "Trust in\nPublic Schools",
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_void() +
  labs(
    title = "District-Level Trust in Public Schools (MRP Estimates)")

saveRDS(trust_map, "trust_map.rds")

# Raw survey mean

district_raw <- yougov_covariates_df |>
  group_by(leaid) |>
  summarise(
    raw_mean = mean(trust_schools_binary),
    n = n()
  ) |>
  filter(n >= 30)

validation <- district_raw |>
  left_join(district_estimates, by = "leaid")

r_raw <- cor(validation$raw_mean, validation$trust_hat, use = "complete.obs")

ggplot(validation, aes(x = raw_mean, y = trust_hat)) +
  geom_point(aes(size = n), alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  annotate("text", x = min(validation$raw_mean, na.rm = TRUE),
           y = max(validation$trust_hat, na.rm = TRUE),
           label = paste0("r = ", round(r_raw, 3), "\nn = ", nrow(validation), " districts"),
           hjust = 0, size = 5) +
  scale_size_continuous(name = "Respondents", range = c(1, 5)) +
  labs(
    x = "Raw Survey Mean (districts with 30+ respondents)",
    y = "MRP Estimate",
    title = "Validation: MRP Estimates vs. Raw Survey Means"
  ) +
  theme_minimal()

ggsave("raw_validation.png", width = 8, height = 6, dpi = 300)








# Claude generated ACS Pull Code
# ── 1. Pull ACS district-level tables ────────────────────────────────────────
pums_df <- here("MRP", "State Level", "pums_2023_acs5_allstates.rds") |> 
  readRDS()

states <- fips_codes |>
  filter(as.numeric(state_code) <= 56) |>
  distinct(state) |>
  pull(state)

pull_district_table <- function(table, year = 2023, survey = "acs5") {
  district_types <- c(
    "school district (unified)",
    "school district (elementary)",
    "school district (secondary)"
  )

  map(district_types, \(geo) {
    map(states, \(st) {
      tryCatch(
        get_acs(
          geography = geo,
          table = table,
          state = st,
          year = year,
          survey = survey,
          cache_table = TRUE
        ),
        error = \(e) NULL
      )
    }) |>
      bind_rows()
  }) |>
    bind_rows()
}

age_sex_raw <- pull_district_table("B01001") |> 
  filter(!str_ends(GEOID, "99999"))
educ_raw    <- pull_district_table("B15003") |> 
  filter(!str_ends(GEOID, "99999"))
race_raw    <- pull_district_table("B03002") |> 
  filter(!str_ends(GEOID, "99999"))

saveRDS(age_sex_raw, "age_sex_raw.rds")
saveRDS(educ_raw, "educ_raw.rds")
saveRDS(race_raw, "race_raw.rds")

# ── 2. Variable mappings ─────────────────────────────────────────────────────

age_sex_mapping <- tribble(
  ~variable,      ~sex_mrp, ~age_mrp,
  # Male 18+
  "B01001_007",   "Man",    "Under 30",  # 18-19
  "B01001_008",   "Man",    "Under 30",  # 20
  "B01001_009",   "Man",    "Under 30",  # 21
  "B01001_010",   "Man",    "Under 30",  # 22-24
  "B01001_011",   "Man",    "Under 30",  # 25-29
  "B01001_012",   "Man",    "30-44",     # 30-34
  "B01001_013",   "Man",    "30-44",     # 35-39
  "B01001_014",   "Man",    "30-44",     # 40-44
  "B01001_015",   "Man",    "45-64",     # 45-49
  "B01001_016",   "Man",    "45-64",     # 50-54
  "B01001_017",   "Man",    "45-64",     # 55-59
  "B01001_018",   "Man",    "45-64",     # 60-61
  "B01001_019",   "Man",    "45-64",     # 62-64
  "B01001_020",   "Man",    "65+",       # 65-66
  "B01001_021",   "Man",    "65+",       # 67-69
  "B01001_022",   "Man",    "65+",       # 70-74
  "B01001_023",   "Man",    "65+",       # 75-79
  "B01001_024",   "Man",    "65+",       # 80-84
  "B01001_025",   "Man",    "65+",       # 85+
  # Female 18+
  "B01001_031",   "Woman",  "Under 30",  # 18-19
  "B01001_032",   "Woman",  "Under 30",  # 20
  "B01001_033",   "Woman",  "Under 30",  # 21
  "B01001_034",   "Woman",  "Under 30",  # 22-24
  "B01001_035",   "Woman",  "Under 30",  # 25-29
  "B01001_036",   "Woman",  "30-44",     # 30-34
  "B01001_037",   "Woman",  "30-44",     # 35-39
  "B01001_038",   "Woman",  "30-44",     # 40-44
  "B01001_039",   "Woman",  "45-64",     # 45-49
  "B01001_040",   "Woman",  "45-64",     # 50-54
  "B01001_041",   "Woman",  "45-64",     # 55-59
  "B01001_042",   "Woman",  "45-64",     # 60-61
  "B01001_043",   "Woman",  "45-64",     # 62-64
  "B01001_044",   "Woman",  "65+",       # 65-66
  "B01001_045",   "Woman",  "65+",       # 67-69
  "B01001_046",   "Woman",  "65+",       # 70-74
  "B01001_047",   "Woman",  "65+",       # 75-79
  "B01001_048",   "Woman",  "65+",       # 80-84
  "B01001_049",   "Woman",  "65+"        # 85+
)

# B15003 universe is 25+, not 18+. Rescaled to 18+ pop in IPF step.
educ_mapping <- tribble(
  ~variable,      ~educ_mrp,
  "B15003_002",   "HS or less",    # No schooling
  "B15003_003",   "HS or less",    # Nursery school
  "B15003_004",   "HS or less",    # Kindergarten
  "B15003_005",   "HS or less",    # 1st grade
  "B15003_006",   "HS or less",    # 2nd grade
  "B15003_007",   "HS or less",    # 3rd grade
  "B15003_008",   "HS or less",    # 4th grade
  "B15003_009",   "HS or less",    # 5th grade
  "B15003_010",   "HS or less",    # 6th grade
  "B15003_011",   "HS or less",    # 7th grade
  "B15003_012",   "HS or less",    # 8th grade
  "B15003_013",   "HS or less",    # 9th grade
  "B15003_014",   "HS or less",    # 10th grade
  "B15003_015",   "HS or less",    # 11th grade
  "B15003_016",   "HS or less",    # 12th no diploma
  "B15003_017",   "HS or less",    # HS diploma
  "B15003_018",   "HS or less",    # GED
  "B15003_019",   "Some college",  # Some college < 1yr
  "B15003_020",   "Some college",  # Some college 1+ yr
  "B15003_021",   "Some college",  # Associate's
  "B15003_022",   "College grad",  # Bachelor's
  "B15003_023",   "Postgrad",      # Master's
  "B15003_024",   "Postgrad",      # Professional
  "B15003_025",   "Postgrad"       # Doctorate
)

# B03002 universe is total pop, not 18+. Rescaled in IPF step.
# _003 + _004 + _005 + _006 + _007 + _008 + _009 + _012 = _001 (total)
race_mapping <- tribble(
  ~variable,      ~race_mrp,
  "B03002_003",   "White",     # NH White alone
  "B03002_004",   "Black",     # NH Black alone
  "B03002_005",   "Other",     # NH AIAN alone
  "B03002_006",   "Other",     # NH Asian alone
  "B03002_007",   "Other",     # NH NHPI alone
  "B03002_008",   "Other",     # NH Some other race alone
  "B03002_009",   "Other",     # NH Two or more races (total; do NOT also include _010, _011)
  "B03002_012",   "Hispanic"   # Hispanic or Latino (total)
)

# ── 3. Aggregate to MRP categories ───────────────────────────────────────────

age_sex_marginal <- age_sex_raw |>
  inner_join(age_sex_mapping, by = "variable") |>
  group_by(GEOID, NAME, age_mrp, sex_mrp) |>
  summarise(count = sum(estimate, na.rm = TRUE), .groups = "drop")

educ_marginal <- educ_raw |>
  inner_join(educ_mapping, by = "variable") |>
  group_by(GEOID, NAME, educ_mrp) |>
  summarise(count = sum(estimate, na.rm = TRUE), .groups = "drop")

race_marginal <- race_raw |>
  inner_join(race_mapping, by = "variable") |>
  group_by(GEOID, NAME, race_mrp) |>
  summarise(count = sum(estimate, na.rm = TRUE), .groups = "drop")

# ── 4. Sanity checks on raw marginals ────────────────────────────────────────

# Race: mapped categories should sum to total population (_001)
race_check <- race_raw |>
  inner_join(race_mapping, by = "variable") |>
  group_by(GEOID) |>
  summarise(mapped_total = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  left_join(
    race_raw |>
      filter(variable == "B03002_001") |>
      select(GEOID, total_pop = estimate),
    by = "GEOID"
  ) |>
  mutate(diff = mapped_total - total_pop)

cat("Race mapping residuals:\n")
summary(race_check$diff)

# Education: mapped categories should sum to 25+ total (_001)
educ_check <- educ_raw |>
  inner_join(educ_mapping, by = "variable") |>
  group_by(GEOID) |>
  summarise(mapped_total = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  left_join(
    educ_raw |>
      filter(variable == "B15003_001") |>
      select(GEOID, total_25plus = estimate),
    by = "GEOID"
  ) |>
  mutate(diff = mapped_total - total_25plus)

cat("Education mapping residuals:\n")
summary(educ_check$diff)

# Age x sex: mapped categories should sum to 18+ population
age_sex_check <- age_sex_raw |>
  inner_join(age_sex_mapping, by = "variable") |>
  group_by(GEOID) |>
  summarise(mapped_total = sum(estimate, na.rm = TRUE), .groups = "drop") |>
  left_join(
    age_sex_raw |>
      filter(variable == "B01001_001") |>
      select(GEOID, total_all_ages = estimate),
    by = "GEOID"
  )

cat("Age x sex: 18+ as share of total pop:\n")
summary(age_sex_check$mapped_total / age_sex_check$total_all_ages)
# Should be roughly 0.7-0.85 for most districts

# ── 5. National PUMS seed (128 cells) ────────────────────────────────────────

national_seed <- pums_df |>
  filter(AGEP >= 18) |>
  mutate(
    age_mrp = case_when(
      AGEP < 30  ~ "Under 30",
      AGEP < 45  ~ "30-44",
      AGEP < 65  ~ "45-64",
      AGEP >= 65 ~ "65+"
    ),
    educ_mrp = case_when(
      SCHL %in% c("18", "19", "20") ~ "Some college",
      SCHL == "21"                   ~ "College grad",
      SCHL %in% c("22", "23", "24") ~ "Postgrad",
      TRUE                           ~ "HS or less"
    ),
    race_mrp = case_when(
      HISP != "01"  ~ "Hispanic",
      RAC1P == "1"  ~ "White",
      RAC1P == "2"  ~ "Black",
      TRUE          ~ "Other"
    ),
    sex_mrp = case_when(
      SEX == "1" ~ "Man",
      SEX == "2" ~ "Woman"
    )
  ) |>
  group_by(age_mrp, educ_mrp, race_mrp, sex_mrp) |>
  summarise(seed_prop = sum(PWGTP), .groups = "drop") |>
  mutate(seed_prop = seed_prop / sum(seed_prop))

stopifnot(nrow(national_seed) == 128)

# ── 6. IPF function ──────────────────────────────────────────────────────────

run_ipf <- function(seed, age_sex_target, educ_target, race_target,
                    max_iter = 200, tol = 1e-6) {
  cell_counts <- seed$seed_prop * sum(age_sex_target)

  for (i in 1:max_iter) {
    old_counts <- cell_counts

    # Fit age x sex joint (8 cells)
    for (a in unique(seed$age_mrp)) {
      for (s in unique(seed$sex_mrp)) {
        idx <- which(seed$age_mrp == a & seed$sex_mrp == s)
        key <- paste(a, s, sep = "|")
        target_val <- age_sex_target[key]
        if (is.na(target_val)) target_val <- 0
        current_sum <- sum(cell_counts[idx])
        if (current_sum > 0) {
          cell_counts[idx] <- cell_counts[idx] * target_val / current_sum
        }
      }
    }

    # Fit education marginal (4 cells)
    for (e in unique(seed$educ_mrp)) {
      idx <- which(seed$educ_mrp == e)
      target_val <- educ_target[e]
      if (is.na(target_val)) target_val <- 0
      current_sum <- sum(cell_counts[idx])
      if (current_sum > 0) {
        cell_counts[idx] <- cell_counts[idx] * target_val / current_sum
      }
    }

    # Fit race marginal (4 cells)
    for (r in unique(seed$race_mrp)) {
      idx <- which(seed$race_mrp == r)
      target_val <- race_target[r]
      if (is.na(target_val)) target_val <- 0
      current_sum <- sum(cell_counts[idx])
      if (current_sum > 0) {
        cell_counts[idx] <- cell_counts[idx] * target_val / current_sum
      }
    }

    if (max(abs(cell_counts - old_counts)) < tol) break
  }

  seed |>
    mutate(count = cell_counts, ipf_iters = i)
}

# ── 7. Build poststratification frame ────────────────────────────────────────

# Expected keys for guard checks
expected_as_keys <- national_seed |>
  distinct(age_mrp, sex_mrp) |>
  mutate(key = paste(age_mrp, sex_mrp, sep = "|")) |>
  pull(key)

expected_educ_keys <- unique(national_seed$educ_mrp)
expected_race_keys <- unique(national_seed$race_mrp)

district_ids <- unique(age_sex_marginal$GEOID)

poststrat_frame <- map(district_ids, \(id) {

  # Age x sex target
  as_target <- age_sex_marginal |>
    filter(GEOID == id) |>
    mutate(key = paste(age_mrp, sex_mrp, sep = "|")) |>
    select(key, count) |>
    deframe()

  # Fill missing keys with 0
  missing_as <- setdiff(expected_as_keys, names(as_target))
  if (length(missing_as) > 0) as_target[missing_as] <- 0

  # Education target
  ed_target <- educ_marginal |>
    filter(GEOID == id) |>
    select(educ_mrp, count) |>
    deframe()

  missing_ed <- setdiff(expected_educ_keys, names(ed_target))
  if (length(missing_ed) > 0) ed_target[missing_ed] <- 0

  # Race target
  rc_target <- race_marginal |>
    filter(GEOID == id) |>
    select(race_mrp, count) |>
    deframe()

  missing_rc <- setdiff(expected_race_keys, names(rc_target))
  if (length(missing_rc) > 0) rc_target[missing_rc] <- 0

  # Skip zero-population districts
  as_total <- sum(as_target)
  if (as_total == 0) return(NULL)

  # Rescale educ (25+ universe) and race (total pop universe) to 18+ total
  ed_total <- sum(ed_target)
  rc_total <- sum(rc_target)
  if (ed_total > 0) ed_target <- ed_target * (as_total / ed_total)
  if (rc_total > 0) rc_target <- rc_target * (as_total / rc_total)

  result <- run_ipf(national_seed, as_target, ed_target, rc_target)
  result$GEOID <- id
  result
}, .progress = TRUE) |>
  bind_rows()

poststrat_frame <- poststrat_frame |>
  rename(leaid = GEOID) |>
  mutate(STATE = str_sub(leaid, 1, 2)) |>
  left_join(
    dist_covariates |>
      select(leaid, dem_plurality_z, child_poverty_pct_z,
             log_enrollment_z, log_ppexp_z),
    by = "leaid"
  ) |>
  filter(!is.na(dem_plurality_z))

saveRDS(poststrat_frame, "poststrat_frame_nopid.rds")




# ── Load what's already saved ─────────────────────────────────────────────────
library(here)
library(tidyverse)
library(brms)
library(cmdstanr)

fit_covariates <- readRDS(here("MRP", "District Level", "covariate_dist.rds"))
pid_model <- readRDS(here("MRP", "District Level", "pid_model.rds"))
poststrat_nopid <- readRDS(here("MRP", "District Level", "poststrat_frame_nopid.rds"))

# ── Rerun PID predictions ────────────────────────────────────────────────────
district_ids <- unique(poststrat_nopid$leaid)
chunks <- split(district_ids, ceiling(seq_along(district_ids) / 100))

pid_results <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  chunk_data <- poststrat_nopid |> filter(leaid %in% chunks[[i]])

  pred_chunk <- fitted(
    pid_model,
    newdata = chunk_data,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    scale = "response",
    summary = TRUE
  )

  pid_probs <- pred_chunk[, "Estimate", ]
  colnames(pid_probs) <- c("Democrat", "Independent", "Republican")

  pid_results[[i]] <- chunk_data |>
    bind_cols(as_tibble(pid_probs))

  rm(pred_chunk, pid_probs, chunk_data)
  if (i %% 10 == 0) gc()

  message("PID chunk ", i, " of ", length(chunks))
}

poststrat_pid <- bind_rows(pid_results) |>
  pivot_longer(
    cols = c("Democrat", "Independent", "Republican"),
    names_to = "pid3_mrp",
    values_to = "pid_prob"
  ) |>
  mutate(N_cpj = count * pid_prob)

rm(pid_results, poststrat_nopid, pid_model)
gc()

saveRDS(poststrat_pid, here("MRP", "District Level", "poststrat_pid.rds"))

# ── Rerun trust predictions ──────────────────────────────────────────────────
district_ids <- unique(poststrat_pid$leaid)
chunks <- split(district_ids, ceiling(seq_along(district_ids) / 100))

results_list <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  chunk_data <- poststrat_pid |> filter(leaid %in% chunks[[i]])

  pred_trust <- fitted(
    fit_covariates,
    newdata = chunk_data,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    scale = "response",
    summary = TRUE
  )[, "Estimate"]

  chunk_data$pred_trust <- pred_trust

  results_list[[i]] <- chunk_data |>
    group_by(leaid) |>
    summarise(trust_hat = sum(N_cpj * pred_trust) / sum(N_cpj))

  rm(pred_trust, chunk_data)
  if (i %% 10 == 0) gc()

  message("Trust chunk ", i, " of ", length(chunks))
}

district_estimates <- bind_rows(results_list)
saveRDS(district_estimates, "district_trust_estimates.rds")