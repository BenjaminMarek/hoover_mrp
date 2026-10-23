
# Packages
library(tidyverse)
library(here)
library(tidycensus)
library(labelled)
library(synthjoint)
library(lme4)
library(ggplot2)
library(brms)

# census_api_key("b089d8540d85e3444422655ec2879ecabddbafb6", install = TRUE)

# Gallup Data
# gallup_raw_df <- load(here("Data", "Gallup confidence surveys (1973-2025)", 
#    "gallup_confidence_in_institutions_1973_2025.rdata"))

load(here("Data", "Gallup confidence surveys (1973-2025)", "gallup_confidence_in_institutions_1973_2025.rdata"))

gallup_raw_df <- gallup_confidence_in_institutions_1973_2025

# Find labels
# val_labels(gallup_raw_df$partyr)
# val_labels(gallup_raw_df$raceban1)
# val_labels(gallup_raw_df$educr)
# val_labels(gallup_raw_df$ager)
# val_labels(gallup_raw_df$gr)
# val_labels(gallup_raw_df$state)
# val_labels(gallup_raw_df$conf_schools)

gallup_clean_df <- gallup_raw_df |> 
  mutate(
    year_mrp = as.integer(yr),    
    age_mrp = case_when(
      ager == 1 ~ "18-29",
      ager == 2 ~ "30-49",
      ager == 3 ~ "50-64",
      ager == 4 ~ "65+",
      TRUE ~ NA_character_
    ),
    educ_mrp = case_when(
      educr %in% c(1,2) ~ "HS or less",
      educr == 3 ~ "Some college",
      educr == 4 ~ "College grad",
      TRUE ~ NA_character_
    ),
    race_mrp = case_when(
      raceban2 == 1 ~ "White",
      raceban2 == 2 ~ "Black",
      raceban2 == 3 ~ "Hispanic",
      raceban2 %in% c(4, 7) ~ "Other",
      TRUE ~ NA_character_
    ),
    sex_mrp = case_when(
      gr == 1 ~ "Man",
      gr == 2 ~ "Woman",
      TRUE ~ NA_character_
    ),
    pid3_mrp = case_when(
      partyrr == 1 ~ "Republican",
      partyrr == 2 ~ "Independent",
      partyrr == 3 ~ "Democrat",
      TRUE ~ NA_character_
    ), 
    trust_binary = case_when(
        conf_schools %in% c(1,2) ~  1,    # "Great deal" or "Quite a lot"
        conf_schools %in% c(3,4,5) ~ 0,   # "Some", "Very little", "None"
        TRUE ~ NA_integer_
      ),
    STATE = sprintf("%02d", case_when(
      state == 11 ~ 23,  # Maine
      state == 12 ~ 33,  # New Hampshire
      state == 13 ~ 50,  # Vermont
      state == 14 ~ 25,  # Massachusetts
      state == 15 ~ 44,  # Rhode Island
      state == 16 ~ 9,   # Connecticut
      state == 21 ~ 36,  # New York
      state == 22 ~ 34,  # New Jersey
      state == 23 ~ 42,  # Pennsylvania
      state == 24 ~ 24,  # Maryland
      state == 25 ~ 10,  # Delaware
      state == 26 ~ 54,  # West Virginia
      state == 27 ~ 11,  # District of Columbia
      state == 31 ~ 39,  # Ohio
      state == 32 ~ 26,  # Michigan
      state == 33 ~ 18,  # Indiana
      state == 34 ~ 17,  # Illinois
      state == 41 ~ 55,  # Wisconsin
      state == 42 ~ 27,  # Minnesota
      state == 43 ~ 19,  # Iowa
      state == 44 ~ 29,  # Missouri
      state == 45 ~ 38,  # North Dakota
      state == 46 ~ 46,  # South Dakota
      state == 47 ~ 31,  # Nebraska
      state == 48 ~ 20,  # Kansas
      state == 51 ~ 51,  # Virginia
      state == 52 ~ 37,  # North Carolina
      state == 53 ~ 45,  # South Carolina
      state == 54 ~ 13,  # Georgia
      state == 55 ~ 12,  # Florida
      state == 56 ~ 21,  # Kentucky
      state == 57 ~ 47,  # Tennessee
      state == 58 ~ 1,   # Alabama
      state == 59 ~ 28,  # Mississippi
      state == 61 ~ 5,   # Arkansas
      state == 62 ~ 22,  # Louisiana
      state == 63 ~ 40,  # Oklahoma
      state == 64 ~ 48,  # Texas
      state == 71 ~ 30,  # Montana
      state == 72 ~ 4,   # Arizona
      state == 73 ~ 8,   # Colorado
      state == 74 ~ 16,  # Idaho
      state == 75 ~ 56,  # Wyoming
      state == 76 ~ 49,  # Utah
      state == 77 ~ 32,  # Nevada
      state == 78 ~ 35,  # New Mexico
      state == 81 ~ 6,   # California
      state == 82 ~ 41,  # Oregon
      state == 83 ~ 53,  # Washington
      state == 84 ~ 15,  # Hawaii
      state == 85 ~ 2    # Alaska
    ))) |> 
  filter(complete.cases(year_mrp, STATE, educ_mrp, race_mrp, sex_mrp, pid3_mrp, trust_binary, age_mrp), year_mrp >= 2006)


gallup_fact <- gallup_clean_df |> 
  mutate(pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican"))) |> 
  filter(complete.cases(pid3_mrp)) |> 
  mutate(age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))



# CCES
### CCES marginals
#### Loads CCES data
cces_cum_df <- here("Data", "CCES Surveys", "cumulative_2006-2024.rds") |> 
  read_rds() |> 
  mutate(
    STATE = case_when(
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
      TRUE ~ NA_character_),
    pid3_mrp = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      TRUE ~ NA_character_),
    age_mrp = case_when(
      age < 30 ~ "18-29",
      age < 50 ~ "30-49",
      age < 65 ~ "50-64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_),
    educ_mrp = case_when(
      educ %in% c(1, 2) ~ "HS or less",
      educ %in% c(3, 4) ~ "Some college",
      educ %in% c(5, 6) ~ "College grad",
      TRUE ~ NA_character_),
    race_mrp = case_when(
      race == 3 ~ "Hispanic",
      race == 1 ~ "White",
      race == 2 ~ "Black",
      TRUE ~ "Other"),
    sex_mrp = case_when(
      gender == 1 ~ "Man",
      gender == 2 ~ "Woman",
      TRUE ~ NA_character_)
  ) |> 
  filter(complete.cases(pid3_mrp, STATE, age_mrp, educ_mrp, race_mrp, sex_mrp))



# 2006-2010 poststrat frame (ACS 5 year 2006-2010, CCES pooled 2006-2010)

# pums_06_10_df <- get_pums( c("ST","RAC1P", "SCHL", "HISP", "AGEP", "SEX"), state = "all", 
#     year = 2010, survey = "acs5") |> 
#   filter(complete.cases(ST), AGEP >= 18) |> 
#   mutate(
#     age_mrp = case_when(
#       AGEP < 30 ~ "18-29",
#       AGEP < 50 ~ "30-49", 
#       AGEP < 65 ~ "50-64",
#       AGEP >= 65 ~ "65+"),
#     educ_mrp = case_when(
#       as.integer(SCHL) %in% 10:12 ~ "Some college",
#       as.integer(SCHL) %in% 13:16 ~ "College grad",
#       TRUE ~ "HS or less"),
#     race_mrp = case_when(
#       as.integer(HISP) != 1 ~ "Hispanic",
#       as.integer(RAC1P) == 1 ~ "White",
#       as.integer(RAC1P) == 2 ~ "Black",
#       TRUE ~ "Other"),
#     sex_mrp = case_when(
#       as.integer(SEX) == 1 ~ "Man",
#       TRUE ~ "Woman"),
#     STATE = sprintf("%02d", as.integer(ST))
#   ) |> 
#   group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
#   summarize(count = sum(PWGTP)) |> 
#   ungroup() 

# saveRDS(pums_06_10_df, "pums_06_10_df.rds")

pums_06_10_df <- here("Data", "ACS PUMS", "pums_06_10_df.rds") |> 
  read_rds() 

pop_10 <- pums_06_10_df |> 
  group_by(STATE) |> 
  summarise(state_pop = sum(count)) |> 
  ungroup()

cces_06_10 <- cces_cum_df |> 
  filter(year >= 2006, year <= 2010) |> 
  group_by(STATE, pid3_mrp) |> 
  summarise(s_count = sum(weight_cumulative)) |> 
  ungroup() |> 
  group_by(STATE) |> 
  mutate(proportion = s_count / sum(s_count)) |> 
  ungroup() |> 
  left_join(pop_10, by = "STATE") |> 
  mutate(count = proportion * state_pop) |> 
  select(STATE, pid3_mrp, count)

cces_fact_06_10 <- cces_cum_df |> 
  filter(year >= 2006, year <= 2010) |> 
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

poststrat_06_10 <- synth_smoothfix(
  pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
  cces_fact_06_10,
  pums_06_10_df,
  fix_to = cces_06_10,
  area_var = "STATE")


# 2011-2015 poststrat frame (ACS 5 year 2011-2015, CCES pooled 2011-2015)

# pums_11_15_df <- get_pums( c("ST","RAC1P", "SCHL", "HISP", "AGEP", "SEX"), state = "all", 
#     year = 2015, survey = "acs5") |> 
#   filter(complete.cases(ST), AGEP >= 18) |> 
#   mutate(
#     age_mrp = case_when(
#       AGEP < 30 ~ "18-29",
#       AGEP < 50 ~ "30-49", 
#       AGEP < 65 ~ "50-64",
#       AGEP >= 65 ~ "65+"),
#     educ_mrp = case_when(
#       as.integer(SCHL) %in% 18:20 ~ "Some college",
#       as.integer(SCHL) %in% 21:24 ~ "College grad",
#       TRUE ~ "HS or less"),
#     race_mrp = case_when(
#       as.integer(HISP) != 1 ~ "Hispanic",
#       as.integer(RAC1P) == 1 ~ "White",
#       as.integer(RAC1P) == 2 ~ "Black",
#       TRUE ~ "Other"),
#     sex_mrp = case_when(
#       as.integer(SEX) == 1 ~ "Man",
#       TRUE ~ "Woman"),
#     STATE = sprintf("%02d", as.integer(ST))
#   ) |> 
#   group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
#   summarize(count = sum(PWGTP)) |> 
#   ungroup() 

# saveRDS(pums_11_15_df, "pums_11_15_df.rds")

pums_11_15_df <- here("Data", "ACS PUMS", "pums_11_15_df.rds") |> 
  read_rds()

pop_15 <- pums_11_15_df |> 
  group_by(STATE) |> 
  summarise(state_pop = sum(count)) |> 
  ungroup()

cces_11_15 <- cces_cum_df |> 
  filter(year >= 2011, year <= 2015) |> 
  group_by(STATE, pid3_mrp) |> 
  summarise(s_count = sum(weight_cumulative)) |> 
  ungroup() |> 
  group_by(STATE) |> 
  mutate(proportion = s_count / sum(s_count)) |> 
  ungroup() |> 
  left_join(pop_15, by = "STATE") |> 
  mutate(count = proportion * state_pop) |> 
  select(STATE, pid3_mrp, count)

cces_fact_11_15 <- cces_cum_df |> 
  filter(year >= 2011, year <= 2015) |> 
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

poststrat_11_15 <- synth_smoothfix(
  pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
  cces_fact_11_15,
  pums_11_15_df,
  fix_to = cces_11_15,
  area_var = "STATE")


# 2016-2020 poststrat frame (ACS 5 year 2016-2020, CCES pooled 2016-2020)

# pums_16_20_df <- get_pums( c("ST","RAC1P", "SCHL", "HISP", "AGEP", "SEX"), state = "all", 
#     year = 2020, survey = "acs5") |> 
#   filter(complete.cases(ST), AGEP >= 18) |> 
#   mutate(
#     age_mrp = case_when(
#       AGEP < 30 ~ "18-29",
#       AGEP < 50 ~ "30-49", 
#       AGEP < 65 ~ "50-64",
#       AGEP >= 65 ~ "65+"),
#     educ_mrp = case_when(
#       SCHL %in% c("18", "19", "20") ~ "Some college",
#       SCHL %in% c("21", "22", "23", "24") ~ "College grad",
#       TRUE ~ "HS or less"),
#     race_mrp = case_when(
#       HISP != "01" ~ "Hispanic",
#       RAC1P == "1" ~ "White",
#       RAC1P == "2" ~ "Black",
#       TRUE ~ "Other"),
#     sex_mrp = case_when(
#       SEX == "1" ~ "Man",
#       SEX == "2" ~ "Woman"),
#     STATE = ST
#   ) |> 
#   group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
#   summarize(count = sum(PWGTP)) |> 
#   ungroup() 

# saveRDS(pums_16_20_df, "pums_16_20_df.rds")

pums_16_20_df <- here("Data", "ACS PUMS", "pums_16_20_df.rds") |> 
  read_rds()

pop_20 <- pums_16_20_df |> 
  group_by(STATE) |> 
  summarise(state_pop = sum(count)) |> 
  ungroup()

cces_16_20 <- cces_cum_df |> 
  filter(year >= 2016, year <= 2020) |> 
  group_by(STATE, pid3_mrp) |> 
  summarise(s_count = sum(weight_cumulative)) |> 
  ungroup() |> 
  group_by(STATE) |> 
  mutate(proportion = s_count / sum(s_count)) |> 
  ungroup() |> 
  left_join(pop_20, by = "STATE") |> 
  mutate(count = proportion * state_pop) |> 
  select(STATE, pid3_mrp, count)

cces_fact_16_20 <- cces_cum_df |> 
  filter(year >= 2016, year <= 2020) |> 
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

poststrat_16_20 <- synth_smoothfix(
  pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
  cces_fact_16_20,
  pums_16_20_df,
  fix_to = cces_16_20,
  area_var = "STATE")

# 2021-2025 poststrat frame (ACS 5 year 2019-2023, CCES pooled 2021-2024)
# pums_18_24_df <- get_pums( c("STATE","RAC1P", "SCHL", "HISP", "AGEP", "SEX"), state = "all", 
#     year = 2023, survey = "acs5") |> 
#   filter(complete.cases(STATE), AGEP >= 18) |> 
#   mutate(
#     age_mrp = case_when(
#       AGEP < 30 ~ "18-29",
#       AGEP < 50 ~ "30-49", 
#       AGEP < 65 ~ "50-64",
#       AGEP >= 65 ~ "65+"),
#     educ_mrp = case_when(
#       SCHL %in% c("18", "19", "20") ~ "Some college",
#       SCHL %in% c("21", "22", "23", "24") ~ "College grad",
#       TRUE ~ "HS or less"),
#     race_mrp = case_when(
#       HISP != "01" ~ "Hispanic",
#       RAC1P == "1" ~ "White",
#       RAC1P == "2" ~ "Black",
#       TRUE ~ "Other"),
#     sex_mrp = case_when(
#       SEX == "1" ~ "Man",
#       SEX == "2" ~ "Woman")
#   ) |> 
#   group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
#   summarize(count = sum(PWGTP)) |> 
#   ungroup() 

# saveRDS(pums_18_23_df, "pums_18_23_df.rds")

pums_18_23_df <- here("Data", "ACS PUMS", "pums_18_23_df.rds") |> 
  read_rds() 

pop_23 <- pums_18_23_df |> 
  group_by(STATE) |> 
  summarise(state_pop = sum(count)) |> 
  ungroup()

cces_21_24 <- cces_cum_df |> 
  filter(year >= 2021, year <= 2024) |> 
  group_by(STATE, pid3_mrp) |> 
  summarise(s_count = sum(weight_cumulative)) |> 
  ungroup() |> 
  group_by(STATE) |> 
  mutate(proportion = s_count / sum(s_count)) |> 
  ungroup() |> 
  left_join(pop_23, by = "STATE") |> 
  mutate(count = proportion * state_pop) |> 
  select(STATE, pid3_mrp, count)

cces_fact_21_24 <- cces_cum_df |> 
  filter(year >= 2021, year <= 2024) |> 
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

poststrat_21_24 <- synth_smoothfix(
  pid3_mrp ~ age_mrp + educ_mrp + race_mrp + sex_mrp,
  cces_fact_21_24,112
  pums_18_23_df,
  fix_to = cces_21_24,
  area_var = "STATE")


# Pacheco-style MRP estimation. Loops through with 3
#  year pooled chunks of data. Matching 3 years of CCES in poststrat frame

yrs <- 2007:2024
pacheco_results <- list()
pacheco_models <- list()

for (yr in yrs) {

  # Pool data from year before and after target year
  window_3 <- gallup_fact |> 
      filter(year_mrp >= (yr - 1) & year_mrp <= (yr + 1))

  # Select proper base demographic
  if (yr <= 2010) {
      demo_base <- pums_06_10_df
    } else if (yr <= 2015) {
      demo_base <- pums_11_15_df
    } else if (yr <= 2020) {
      demo_base <- pums_16_20_df
    } else {
      demo_base <- pums_18_23_df}
  
  # Clears out sex (dropped it from model)
  demo_base_nosex <- demo_base |> 
    group_by(STATE, age_mrp, educ_mrp, race_mrp) |> 
    summarise(count = sum(count)) |> 
    ungroup()

  # Pull 3 year pooled CCES and state pop marginals
  state_pop <- demo_base |> 
    group_by(STATE) |> 
    summarise(state_pop = sum(count)) |> 
    ungroup()
  
  pid_marginals <- cces_cum_df |> 
    filter(year >= (yr - 1) & year <= (yr + 1)) |> 
    group_by(STATE, pid3_mrp) |> 
    summarise(s_count = sum(weight_cumulative)) |> 
    ungroup() |> 
    group_by(STATE) |> 
    mutate(proportion = s_count / sum(s_count)) |> 
    ungroup() |> 
    left_join(state_pop, by = "STATE") |> 
    mutate(count = proportion * state_pop) |> 
    select(STATE, pid3_mrp, count)

  cces_window <- cces_cum_df |> 
    filter(year >= (yr - 1) & year <= (yr + 1)) |> 
    mutate(
      pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
      age_mrp = factor(age_mrp, levels = c("18-29", "30-49", "50-64", "65+")),
      educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad")),
      race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Other")))

  # Build poststrat frame
  poststrat <- synth_smoothfix(pid3_mrp ~ age_mrp + educ_mrp + race_mrp,
    cces_window,
    demo_base_nosex,
    fix_to = pid_marginals,
    area_var = "STATE")
  
  # MR model
  model <- trust_binary ~ (1 | pid3_mrp) + (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + 
    (1 | STATE)

  # Fit model
   fit <- brm(model,
    data = window_3,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 1), class = "Intercept"),
      prior(normal(0, 0.5), class = "sd")),
    iter = 2000,
    chains = 4,
    cores = 4,
    backend = "cmdstanr")
  
  # Poststrat draws + poststrat
  poststrat$pred <- fitted(fit, newdata = poststrat, re_formula = NULL, 
    allow_new_levels = TRUE)[, "Estimate"]
 state_ests <- poststrat |> 
    group_by(STATE) |> 
    summarise(trust_est = sum(pred * count) / sum(count)) |> 
    ungroup() |> 
    mutate(year = yr)

  # Adds model + results to list
  pacheco_results[[as.character(yr)]] <- state_ests
  pacheco_models[[as.character(yr)]] <- fit
}

pacheco_panel <- bind_rows(pacheco_results)

# Vibe checks
# National trend
pacheco_panel |>
  left_join(pop_23 |> rename(pop = state_pop), by = "STATE") |>
  group_by(year) |>
  summarise(trust = weighted.mean(trust_est, pop)) |>
  ggplot(aes(x = year, y = trust)) +
  geom_line() +
  geom_point() +
  labs(title = "Trust in Public Schools, 2007-2024", y = "Proportion with confidence") +
  theme_minimal()

# Spaghetti plot
ggplot(pacheco_panel, aes(x = year, y = trust_est, group = STATE)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", se = FALSE) +
  labs(title = "State-level trust trajectories") +
  theme_minimal()

# Which states changed most?
pacheco_panel |>
  filter(year %in% c(2007, 2024)) |>
  pivot_wider(names_from = year, values_from = trust_est, names_prefix = "y") |>
  mutate(change = y2024 - y2007) |>
  arrange(change) |>
  print(n = 51)

# Current ranking
pacheco_panel |>
  filter(year == 2024) |>
  arrange(desc(trust_est)) |>
  print(n = 51)

# Pre/post 2020 drop by state
pacheco_panel |>
  mutate(era = if_else(year <= 2019, "pre", "post")) |>
  group_by(STATE, era) |>
  summarise(trust = mean(trust_est), .groups = "drop") |>
  pivot_wider(names_from = era, values_from = trust) |>
  mutate(drop = post - pre) |>
  arrange(drop) |>
  print(n = 51)

# Trust vs partisan lean 2024
pacheco_panel |>
  filter(year == 2024) |>
  left_join(
    cces_cum_df |>
      filter(year >= 2020) |>
      group_by(STATE) |>
      summarise(rep_share = mean(pid3_mrp == "Republican")),
    by = "STATE") |>
  ggplot(aes(x = rep_share, y = trust_est)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "State trust vs. Republican share, 2024") +
  theme_minimal()

# Summary table
pacheco_panel |>
  group_by(year) |>
  summarise(
    mean = round(mean(trust_est), 3),
    sd = round(sd(trust_est), 3),
    min = round(min(trust_est), 3),
    max = round(max(trust_est), 3)) |>
  print(n = 18)

pacheco_panel |>
  filter(year == 2015) |>
  left_join(
    gallup_fact |>
      filter(year_mrp %in% 2014:2016) |>
      group_by(STATE) |>
      summarise(raw = mean(trust_binary), n = n()),
    by = "STATE") |>
  filter(n >= 30) |>
  ggplot(aes(x = raw, y = trust_est)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "MRP vs raw estimates (states with n >= 30)")

pacheco_panel |>
  filter(year == 2015) |>
  left_join(
    gallup_fact |>
      filter(year_mrp %in% 2014:2016) |>
      group_by(STATE) |>
      summarise(raw = mean(trust_binary), n = n()),
    by = "STATE") |>
  mutate(shrinkage = trust_est - raw) |>
  ggplot(aes(x = n, y = abs(shrinkage))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Shrinkage by sample size",
       x = "State sample size",
       y = "Absolute difference (MRP - raw)")



# Dynamic MRP
# Single model, all years
model <- trust_binary ~ (1 | pid3_mrp) + (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + 
  (1 | STATE) + (1 | year_mrp) + (1 | state_year)

gallup_fact <- gallup_fact |>
  mutate(year_factor = factor(year_mrp),
         state_year = interaction(STATE, year_mrp))

fit_dynamic <- brm(model,
  data = gallup_fact,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 0.5), class = "sd"),
    prior(normal(0, 0.1), class = "sd", group = "state_year")),  # tight prior on deviations
  iter = 4000,
  chains = 4,
  cores = 4,
  backend = "cmdstanr")

# Check state_year variance
summary(fit_dynamic)

# Build poststrat frame for each year
# Using your existing frames, just need to tag with year

poststrat_all <- bind_rows(
  poststrat_06_10 |> 
    group_by(STATE, pid3_mrp, age_mrp, educ_mrp, race_mrp) |>
    summarise(count = sum(count), .groups = "drop") |>
    crossing(year_mrp = 2007:2010),
  poststrat_11_15 |> 
    group_by(STATE, pid3_mrp, age_mrp, educ_mrp, race_mrp) |>
    summarise(count = sum(count), .groups = "drop") |>
    crossing(year_mrp = 2011:2015),
  poststrat_16_20 |> 
    group_by(STATE, pid3_mrp, age_mrp, educ_mrp, race_mrp) |>
    summarise(count = sum(count), .groups = "drop") |>
    crossing(year_mrp = 2016:2020),
  poststrat_21_24 |> 
    group_by(STATE, pid3_mrp, age_mrp, educ_mrp, race_mrp) |>
    summarise(count = sum(count), .groups = "drop") |>
    crossing(year_mrp = 2021:2024)
) |>
  mutate(state_year = interaction(STATE, year_mrp, drop = TRUE))

# Poststratify
poststrat_all$pred <- fitted(fit_dynamic, 
  newdata = poststrat_all, 
  re_formula = NULL,
  allow_new_levels = TRUE)[, "Estimate"]

# Aggregate to state-year
dynamic_panel <- poststrat_all |>
  group_by(STATE, year_mrp) |>
  summarise(trust_est = sum(pred * count) / sum(count), .groups = "drop")



# Non-Diagnostic Robustness check
model <- trust_binary ~ (1|pid3_mrp) + (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|state) + 
  (1|yr) + (1|state:yr) + (1|yr:pid3_mrp)

# Gaussian parameters with year by year brms. Struggle with potential large post covid drops














# MRP w/ generalized linear model (not bayesian)


# Setup
years_to_estimate <- 2006:2024
get_poststrat_frame <- function(year) {
  if (year >= 2006 & year <= 2010) {
    return(poststrat_06_10)
  } else if (year >= 2011 & year <= 2015) {
    return(poststrat_11_15)
  } else if (year >= 2016 & year <= 2020) {
    return(poststrat_16_20)
  } else if (year >= 2021 & year <= 2025) {
    return(poststrat_21_24)
  } else {
    stop("Year ", year, " outside supported range")
  }
}

# Model formula
model_formula <- trust_binary ~ sex_mrp + pid3_mrp + 
  (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + (1 | STATE)

# Looped estimates

results_list <- vector("list", length(years_to_estimate))
names(results_list) <- as.character(years_to_estimate)

model_fits <- vector("list", length(years_to_estimate))
names(model_fits) <- as.character(years_to_estimate)

diagnostics <- tibble(
  year = integer(),
  n_obs = integer(),
  converged = logical(),
  log_lik = double(),
  intercept = double(),
  coef_republican = double(),
  coef_independent = double(),
  coef_woman = double(),
  var_state = double(),
  var_age = double(),
  var_educ = double(),
  var_race = double()
)

cat("Starting estimation loop...\n")
cat(strrep("=", 60), "\n")

for (yr in years_to_estimate) {
  
  yr_char <- as.character(yr)
  survey_year <- gallup_fact[gallup_fact$year_mrp == yr, ]
  n_obs <- nrow(survey_year)
  
  cat(sprintf("Year %d | n = %4d | ", yr, n_obs))
  
  fit <- tryCatch({
    glmer(
      model_formula,
      data = survey_year,
      family = binomial(link = "logit"),
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 100000)
      )
    )
  }, error = function(e) {
    warning("Model fitting failed for year ", yr, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) {
    cat("FAILED\n")
    next
  }
  
  converged <- is.null(fit@optinfo$conv$lme4$messages)
  if (!converged) cat("CONVERGENCE WARNING | ")
  
  model_fits[[yr_char]] <- fit
  
  fe <- fixef(fit)
  vc <- VarCorr(fit)
  
  diagnostics <- bind_rows(diagnostics, tibble(
    year = yr,
    n_obs = n_obs,
    converged = converged,
    log_lik = as.numeric(logLik(fit)),
    intercept = fe["(Intercept)"],
    coef_republican = fe["pid3_mrpRepublican"],
    coef_independent = fe["pid3_mrpIndependent"],
    coef_woman = fe["sex_mrpWoman"],
    var_state = as.numeric(vc$STATE),
    var_age = as.numeric(vc$age_mrp),
    var_educ = as.numeric(vc$educ_mrp),
    var_race = as.numeric(vc$race_mrp)
  ))
  
  ps_frame <- get_poststrat_frame(yr)
  ps_frame$predicted_prob <- predict(fit, newdata = ps_frame, 
                                      type = "response", allow.new.levels = TRUE)
  
  state_estimates <- ps_frame |>
    group_by(STATE) |>
    summarise(
      trust_estimate = sum(predicted_prob * count) / sum(count),
      total_pop = sum(count),
      .groups = "drop"
    ) |>
    mutate(year = yr)
  
  results_list[[yr_char]] <- state_estimates
  
  est_range <- range(state_estimates$trust_estimate)
  cat(sprintf("range: [%.3f, %.3f]\n", est_range[1], est_range[2]))
}
cat(strrep("=", 60), "\n")

state_year_estimates <- bind_rows(results_list)

# Post Hoc smoothing

state_year_smoothed <- state_year_estimates |>
  arrange(STATE, year) |>
  group_by(STATE) |>
  mutate(
    trust_smoothed = predict(
      loess(trust_estimate ~ year, span = 0.5, degree = 1),
      newdata = year
    )
  ) |>
  ungroup()

# Diagnostic output

cat("\nPID coefficients over time:\n")
print(diagnostics |> select(year, coef_republican, coef_independent), n = 25)

cat("\nVariance components (first and last year):\n")
diagnostics |> 
  filter(year %in% c(min(year), max(year))) |>
  select(year, var_state, var_age, var_educ, var_race) |>
  print()

if (any(!diagnostics$converged)) {
  cat("\nConvergence issues:\n")
  print(diagnostics |> filter(!converged) |> select(year, n_obs))
}

# Saving
saveRDS(state_year_estimates, here("Data", "state_year_estimates_raw.rds"))
saveRDS(state_year_smoothed, here("Data", "state_year_estimates_smoothed.rds"))
saveRDS(model_fits, here("Data", "glmer_fits_by_year.rds"))
saveRDS(diagnostics, here("Data", "model_diagnostics.rds"))

# Visualization
large_states <- c("06" = "California", "48" = "Texas", "12" = "Florida", 
                  "36" = "New York", "17" = "Illinois", "42" = "Pennsylvania",
                  "39" = "Ohio", "13" = "Georgia", "37" = "North Carolina", 
                  "26" = "Michigan")

# Large states
state_year_smoothed |> 
  filter(STATE %in% names(large_states)) |>
  ggplot(aes(year, trust_smoothed, color = large_states[STATE])) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  labs(x = NULL, y = "Trust in Public Schools", color = "State") +
  theme_minimal()

# National summary
state_year_smoothed |>
  summarise(mean = mean(trust_smoothed), median = median(trust_smoothed),
            p25 = quantile(trust_smoothed, 0.25), p75 = quantile(trust_smoothed, 0.75),
            .by = year) |>
  ggplot(aes(year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2) +
  geom_line(aes(y = mean, linetype = "Mean"), linewidth = 1) +
  geom_line(aes(y = median, linetype = "Median"), linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  labs(x = NULL, y = "Trust in Public Schools", linetype = NULL) +
  theme_minimal()

# Large states - raw
state_year_estimates |> 
  filter(STATE %in% names(large_states)) |>
  ggplot(aes(year, trust_estimate, color = large_states[STATE])) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  labs(x = NULL, y = "Trust in Public Schools", color = "State") +
  theme_minimal()

# National summary - raw
state_year_estimates |>
  summarise(mean = mean(trust_estimate), median = median(trust_estimate),
            p25 = quantile(trust_estimate, 0.25), p75 = quantile(trust_estimate, 0.75),
            .by = year) |>
  ggplot(aes(year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2) +
  geom_line(aes(y = mean, linetype = "Mean"), linewidth = 1) +
  geom_line(aes(y = median, linetype = "Median"), linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  labs(x = NULL, y = "Trust in Public Schools", linetype = NULL) +
  theme_minimal()


# Pooled period model
periods <- list(
  "2006-2010" = 2006:2010,
  "2011-2015" = 2011:2015,
  "2016-2020" = 2016:2020,
  "2021-2024" = 2021:2024
)

poststrat_frames <- list(
  "2006-2010" = poststrat_06_10,
  "2011-2015" = poststrat_11_15,
  "2016-2020" = poststrat_16_20,
  "2021-2024" = poststrat_21_24
)

period_results <- list()
period_fits <- list()

for (p in names(periods)) {
  
  survey_period <- gallup_fact |> filter(year_mrp %in% periods[[p]])
  cat(p, "| n =", nrow(survey_period), "| ")
  
  fit <- glmer(
    trust_binary ~ sex_mrp + pid3_mrp + race_mrp + age_mrp + educ_mrp + (1 | STATE),
    data = survey_period,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  )
  
  cat("var_state =", round(as.numeric(VarCorr(fit)$STATE), 4), "\n")
  
  period_fits[[p]] <- fit
  
  ps <- poststrat_frames[[p]]
  ps$pred <- predict(fit, newdata = ps, type = "response", allow.new.levels = TRUE)
  
  period_results[[p]] <- ps |>
    summarise(trust = sum(pred * count) / sum(count), .by = STATE) |>
    mutate(period = p)
}

period_estimates <- bind_rows(period_results)

# Extract coefficients
period_coefs <- map_dfr(names(period_fits), function(p) {
  fe <- fixef(period_fits[[p]])
  vc <- as.numeric(VarCorr(period_fits[[p]])$STATE)
  tibble(
    period = p,
    intercept = fe["(Intercept)"],
    coef_rep = fe["pid3_mrpRepublican"],
    coef_ind = fe["pid3_mrpIndependent"],
    coef_woman = fe["sex_mrpWoman"],
    var_state = vc
  )
})

period_coefs

period_coefs |>
  mutate(
    # Baseline trust for Democrats (reference group)
    dem_prob = plogis(intercept),
    # Trust for Republicans  
    rep_prob = plogis(intercept + coef_rep),
    # Partisan gap in percentage points
    partisan_gap_pp = (dem_prob - rep_prob) * 100,
    # Ratio: how many state SDs does the R-D gap span?
    pid_to_state_ratio = abs(coef_rep) / sqrt(var_state)
  ) |>
  select(period, dem_prob, rep_prob, partisan_gap_pp, var_state, pid_to_state_ratio)


# Claude gen graph
period_coefs |>
  mutate(
    period = factor(period, levels = c("2006-2010", "2011-2015", "2016-2020", "2021-2024")),
    dem_trust = plogis(intercept),
    rep_trust = plogis(intercept + coef_rep)
  ) |>
  select(period, dem_trust, rep_trust) |>
  pivot_longer(cols = c(dem_trust, rep_trust), names_to = "party", values_to = "trust") |>
  mutate(party = ifelse(party == "dem_trust", "Democrat", "Republican")) |>
  ggplot(aes(x = period, y = trust, color = party, group = party)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Democrat" = "#2166ac", "Republican" = "#b2182b")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0.1, 0.6)) +
  labs(
    x = NULL, 
    y = "Trust in Public Schools",
    color = NULL,
    title = "The Partisan Gap in School Trust",
    subtitle = "Predicted trust for otherwise-average respondent, by party identification"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.85, 0.85),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )



gallup_clean <- gallup_clean_df %>%
  mutate(era = case_when(
    yr <= 2010 ~ "2006-2010",
    yr <= 2015 ~ "2011-2015",
    yr <= 2020 ~ "2016-2020",
    TRUE ~ "2021-2025"
  ))

# Fit era-specific models and compare PID coefficients
eras <- c("2006-2010", "2011-2015", "2016-2020", "2021-2025")

era_fits <- map(eras, function(e) {
  glm(trust_binary ~ pid3_mrp + age_mrp + educ_mrp + race_mrp + sex_mrp,
      data = filter(gallup_clean, era == e),
      family = binomial(),
      weights = wtfctr)
})

# Extract PID coefficients
map_dfr(era_fits, ~ tibble(
  pid_ind = coef(.x)["pid3_mrpIndependent"],
  pid_rep = coef(.x)["pid3_mrpRepublican"]
), .id = "era")