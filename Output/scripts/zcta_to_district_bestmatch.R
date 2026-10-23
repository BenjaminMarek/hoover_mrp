# ZCTA to District Crosswalking (Best Match)

# Matches survey respondents to their most probable school district using census ZCTA crosswalks. 
# Assigns each respondent to district with highest population for their zip.
# 
# Output includes single district assignment per respondent

# REQUIREMENTS:
# 1. Survey data with zipcode column (csv, dta, sav, xlsx, or rds)
# 2. ZCTA-to-district crosswalk
#    - From Missouri Census Data Center:
#     - 2020: https://mcdc.missouri.edu/applications/geocorr2022.html
#     - 2010: https://mcdc.missouri.edu/applications/geocorr2018.html
#     - 2000: https://mcdc.missouri.edu/applications/geocorr2000.html
#    
#     Download settings:
#     - Source geography: Zipcode/ZCTA
#     - Target geography: School District (Primary, Secondary, Unified, or Best)
#     - Format: CSV
#
# HOW TO USE:
# 1. Download crosswalk file from Missouri Census
# 2. Edit USER INPUT section below:
#    - Input file paths to your survey and crosswalk
#    - Input file types for survey and crosswalk (csv, dta, sav, xlsx, or rds)
#    - Specify column names for zipcodes in survey and crosswalk and target district in crosswalk
# 3. Run entire script
# 4. Results will be in df_merged
# 5. Export as csv or dta if needed:
#    write_csv(df_merged, "output.csv") 
#    haven::write_dta(df_merged, "output.dta")

## OUTPUT:
# Each respondent assigned to ONE district (most probable based on afact):
#
# respondent_id  zipcode  district_id  afact   district_match_status
# 001           94610    28050        0.9149  matched
# 002           12345    NA           NA      missing
# 
# district_match_status: matched (zipcode input, corresponds to district), 
# missing (no zipcode input), not_in_crosswalk (valid zipcodes not included in the crosswalk), 
# and invalid (not a valid zip code).
# Will print the counts of each status.
#
#############################################################################################

## Setup
library(here)
library(tidyverse)
library(haven)
library(readxl)


#######################################
## USER INPUT (EXAMPLE VALUES BELOW) ##
#######################################

survey <- here("Survey Data", "YouGov Surveys", "say-24-baseline-2024-03-20-baseline-2.csv") # file path to survey
survey_file_type <- "csv" # file type of survey
survey_zip <- "zipcode" # name of survey zipcode/zcta column

crosswalk <- here("Survey Data", "Crosswalks", "geocorr2022_2532801595.csv") # file path to crosswalk
crosswalk_file_type <- "csv" # file type of crosswalk
crosswalk_zip <- "zcta" # name of crosswalk zipcode/zcta column
crosswalk_dist <- "sdbest20" # name of target district column

#######################################

## Loading data

# Survey
if (survey_file_type == "csv") {
  df_survey <- read_csv(survey, show_col_types = FALSE)
} else if (survey_file_type == "dta") {
  df_survey <- haven::read_dta(survey)
} else if (survey_file_type == "sav") {
  df_survey <- haven::read_sav(survey)
} else if (survey_file_type == "xlsx") {
  df_survey <- readxl::read_excel(survey)
} else if (survey_file_type == "rds") {
  df_survey <- readRDS(survey)
} else {
  stop("Unsupported survey file type. Use: csv, dta, sav, xlsx, or rds")
}

# Crosswalk
if (crosswalk_file_type == "csv") {
  df_crosswalk <- read_csv(crosswalk, show_col_types = FALSE)
} else if (crosswalk_file_type == "dta") {
  df_crosswalk <- haven::read_dta(crosswalk)
} else if (crosswalk_file_type == "sav") {
  df_crosswalk <- haven::read_sav(crosswalk)
} else if (crosswalk_file_type == "xlsx") {
  df_crosswalk <- readxl::read_excel(crosswalk)
} else if (crosswalk_file_type == "rds") {
  df_crosswalk <- readRDS(crosswalk)
} else {
  stop("Unsupported crosswalk file type. Use: csv, dta, sav, xlsx, or rds")
}
  
df_survey <- df_survey |>
  rename(zipcode = all_of(survey_zip))

df_crosswalk <- df_crosswalk |>
  rename(
    zcta = all_of(crosswalk_zip),
    district_id = all_of(crosswalk_dist)
  )

## Cleaning
df_crosswalk <- df_crosswalk |> 
  mutate(zcta = case_when(
      str_detect(as.character(zcta), "^_{3}") ~ NA_character_,  # 2000 placeholder
      zcta == 99999 | zcta == "99999" ~ NA_character_,          # 2010 placeholder
      TRUE ~ as.character(zcta)
      ),
    zcta = as.double(zcta), afact = as.double(afact)) |> 
  filter(complete.cases(zcta)) 

df_survey <- df_survey |>
  mutate(zipcode = as.double(zipcode), zipcode_status = case_when(
    is.na(zipcode) ~ "missing", 
    zipcode < 501 | zipcode > 99950 ~ "invalid", 
    TRUE ~ "valid"))

## Validating crosswalk
validation <- df_crosswalk |> 
  group_by(zcta) |> 
  summarize(n = sum(afact, na.rm = TRUE)) |> 
  filter(abs(n - 1) > 0.01) 

if (nrow(validation) > 0){
  stop("Error in the crosswalk. ZCTAs have afact not summing to 1.0")
}

## Merging
df_crosswalk <- df_crosswalk |> 
  group_by(zcta) |> 
  slice_max(afact, n = 1, with_ties = FALSE) |> 
  ungroup()

df_merged <- df_survey |>
  left_join(df_crosswalk, 
  by = join_by(zipcode == zcta), na_matches = "never") |> 
  mutate(district_match_status = case_when(
    zipcode_status != "valid" ~ zipcode_status,
    zipcode_status == "valid" & is.na(district_id) ~ "not_in_crosswalk",
    TRUE ~ "matched"))

## Output summary
cat("\n District Matching Summary \n")
print(count(df_merged, district_match_status)) 
# will give the number: matched (zipcode input, corresponds to district), missing (no zipcode input), not_in_crosswalk (valid zipcodes not included in the crosswalk), and invalid (not a valid zip code)
