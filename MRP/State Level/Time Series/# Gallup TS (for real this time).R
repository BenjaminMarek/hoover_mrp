# Gallup TS (for real this time)

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


# ACS
pums_06_10_df <- here("Data", "ACS PUMS", "pums_06_10_df.rds") |> 
  read_rds() 
pums_11_15_df <- here("Data", "ACS PUMS", "pums_11_15_df.rds") |> 
  read_rds()
pums_16_20_df <- here("Data", "ACS PUMS", "pums_16_20_df.rds") |> 
  read_rds()
pums_18_23_df <- here("Data", "ACS PUMS", "pums_18_23_df.rds") |> 
  read_rds() 



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

