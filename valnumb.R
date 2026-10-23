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


# Deduplicate dist_id (take first occurrence, or check if duplicates have conflicting LEAIDs)
dist_id |>
  group_by(YouGovID) |>
  filter(n() > 1) |>
  summarise(n_distinct_leaid = n_distinct(leaid)) |>
  count(n_distinct_leaid)

# If duplicates all agree, just deduplicate
dist_id_deduped <- dist_id |>
  distinct(YouGovID, .keep_all = TRUE)

# Validation: crosswalk vs postdoc LEAIDs

# 1. Dedup dist_id
dist_id_deduped <- dist_id |>
  distinct(YouGovID, .keep_all = TRUE)

# 2. Load and prep crosswalk (best-match)
xwalk <- read_csv(
  here("Data", "Crosswalks", "geocorr2022_2533201713.csv"),
  show_col_types = FALSE
) |>
  mutate(
    zcta = case_when(
      str_detect(as.character(zcta), "^_{3}") ~ NA_character_,
      zcta == 99999 | zcta == "99999" ~ NA_character_,
      TRUE ~ as.character(zcta)
    ),
    zcta = as.double(zcta),
    afact = as.double(afact),
    sdbest20 = paste0(sprintf("%02d", as.integer(state)), substr(str_pad(as.character(sdbest20), width = 5, pad = "0"), 1, 5))
  ) |>
  filter(complete.cases(zcta)) |>
  group_by(zcta) |>
  slice_max(afact, n = 1, with_ties = FALSE) |>
  ungroup()


# 3. Build validation set: respondents with both postdoc LEAID and a ZIP
validation <- yougov_raw_df |>
  inner_join(dist_id_deduped, by = "YouGovID") |>
  filter(!is.na(leaid), !is.na(zipcode)) |>
  mutate(zipcode = as.double(zipcode)) |>
  left_join(xwalk, by = join_by(zipcode == zcta)) |>
  filter(!is.na(sdbest20))

# 4. Agreement
validation <- validation |>
  mutate(match = leaid == sdbest20)

cat("Validation set N:", nrow(validation), "\n")
cat("Agreement rate:", round(mean(validation$match), 3), "\n\n")

# 5. Agreement by afact bin
validation |>
  mutate(afact_bin = cut(afact,
    breaks = c(0, 0.5, 0.7, 0.8, 0.9, 0.95, 1),
    include.lowest = TRUE)) |>
  summarise(n = n(), agreement = round(mean(match), 3), .by = afact_bin) |>
  arrange(afact_bin) |>
  print()

# 6. Afact distribution
cat("\nAfact quantiles:\n")
quantile(validation$afact, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> print()
cat("Pct afact >= 0.9:", round(mean(validation$afact >= 0.9), 3), "\n")

