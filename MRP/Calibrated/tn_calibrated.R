# Calibrated MRP Proof of Concept: Tennessee
# Tests Marble & Clinton (2026) calibration approach on district-level school trust
# Ben Marek | August 2025
#
# WHAT THIS DOES:
# Jointly models school trust and a partisan variable with correlated district
# random effects, then calibrates trust estimates using known district-level
# election results. The partisan signal in the election data propagates to
# trust via the estimated cross-outcome correlation.
#
# KEY LIMITATION:
# The SAY24 survey has pid3 (party ID) but may not have presidential vote choice.
# Ideally the auxiliary variable would be vote choice, since it should match
# vote share in expectation. PID != vote choice: some Independents and even
# cross-partisans vote differently from their identification. The logit shifts
# below therefore conflate genuine survey error with the inherent PID-vote gap.
# If the survey includes a vote choice variable, swap it in at the marked line.

# ── Setup ─────────────────────────────────────────────────────────────────────

library(here)
library(tidyverse)
library(brms)
library(cmdstanr)
library(haven)

TN_FIPS <- "47"

# ── 1. Load survey data ──────────────────────────────────────────────────────

yougov_raw <- bind_rows(
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20240320.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241025.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241121.rds"))
) |>
  group_by(YouGovID) |>
  slice_tail(n = 1) |>
  ungroup()

# District linkage (postdoc file; for crosswalk version, swap in best-match output)
dist_id <- here("Data", "YouGov Surveys", "say_districts.csv") |>
  read.csv() |>
  mutate(leaid = str_pad(as.character(leaid), width = 7, pad = "0")) |>
  distinct(YouGovID, .keep_all = TRUE)

# Prep TN respondents with both outcomes
tn_survey <- yougov_raw |>
  filter(inputstate == "Tennessee", gender4 %in% c("Man", "Woman")) |>
  mutate(
    age_mrp = factor(age4, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ4, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(case_when(
      hispanic == "Yes" ~ "Hispanic",
      race == "White"   ~ "White",
      race == "Black"   ~ "Black",
      TRUE              ~ "Other"
    ), levels = c("White", "Black", "Hispanic", "Other")),
    sex_mrp = factor(case_when(
      gender4 == "Man"   ~ "Man",
      gender4 == "Woman" ~ "Woman"
    ), levels = c("Man", "Woman")),
    trust_binary = case_when(
      institutional_trust_public_schools %in% c("A great deal", "Quite a lot") ~ 1L,
      institutional_trust_public_schools %in% c("Some", "Very little")         ~ 0L,
      TRUE ~ NA_integer_
    ),
    # *** SWAP POINT: if survey has vote choice, replace pid3 logic below ***
    # e.g. repub_binary = if_else(presvote2024 == "Trump", 1L, 0L)
    repub_binary = case_when(
      pid3 == "Republican"                     ~ 1L,
      pid3 %in% c("Democrat", "Independent")  ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  left_join(dist_id, by = "YouGovID") |>
  filter(
    !is.na(trust_binary), !is.na(repub_binary), !is.na(leaid),
    str_sub(leaid, 1, 2) == TN_FIPS
  )

cat("TN respondents with trust + PID + district:", nrow(tn_survey), "\n")
cat("TN districts observed in survey:", n_distinct(tn_survey$leaid), "\n")


# ── 2. District-level election results (ground truth for calibration) ────────

dist_vote <- here("Data", "Election Results",
                  "school_district_vote_shares_2016_2020_2024.dta") |>
  read_dta() |>
  mutate(
    leaid = str_pad(as.character(sd_id), width = 7, pad = "0"),
    dem_share = coalesce(dem_plurality_allprecincts_2024,
                         dem_plurality_allprecincts_2020)
  ) |>
  filter(str_sub(leaid, 1, 2) == TN_FIPS, !is.na(dem_share)) |>
  # VERIFY: is dem_share a two-party proportion (0-1)?
  # If it's a margin (-1 to 1), this needs adjustment.
  mutate(repub_share = 1 - dem_share) |>
  select(leaid, dem_share, repub_share)

cat("TN districts with election data:", nrow(dist_vote), "\n")


# ── 3. District covariates ───────────────────────────────────────────────────

# Rebuild from the same sources as dist_trust.R
dist_ppexp <- read_delim(here("Data", "NCES", "sdf22_1a.txt"), delim = "\t",
                         show_col_types = FALSE) |>
  mutate(
    leaid = str_pad(as.character(LEAID), width = 7, pad = "0"),
    ppexp = TCURELSC / MEMBERSCH
  ) |>
  filter(ppexp > 0, is.finite(ppexp)) |>
  mutate(log_ppexp = log(ppexp)) |>
  select(leaid, log_ppexp)

# For enrollment and poverty, load from saved poststrat frame covariates
# to avoid re-hitting the educationdata API
tn_dist_covariates <- dist_vote |>
  left_join(dist_ppexp, by = "leaid") |>
  left_join(
    readRDS(here("MRP", "District Level", "poststrat_frame_nopid.rds")) |>
      distinct(leaid, dem_plurality_z, child_poverty_pct_z,
               log_enrollment_z, log_ppexp_z),
    by = "leaid"
  )

# Attach covariates to survey
tn_survey <- tn_survey |>
  left_join(
    tn_dist_covariates |> select(leaid, dem_plurality_z, child_poverty_pct_z,
                                  log_enrollment_z, log_ppexp_z),
    by = "leaid"
  ) |>
  filter(!is.na(dem_plurality_z))

cat("TN respondents after covariate join:", nrow(tn_survey), "\n")
cat("TN districts after covariate join:", n_distinct(tn_survey$leaid), "\n")


# ── 4. Poststratification frame (TN only) ───────────────────────────────────

tn_poststrat <- readRDS(here("MRP", "District Level", "poststrat_frame_nopid.rds")) |>
  filter(STATE == TN_FIPS, !is.na(dem_plurality_z))

cat("TN poststrat cells:", nrow(tn_poststrat), "\n")
cat("TN poststrat districts:", n_distinct(tn_poststrat$leaid), "\n")


# ── 5. Multivariate model ───────────────────────────────────────────────────
#
# KEY MODELING CHANGE from the univariate trust model:
# - pid3_mrp moves from PREDICTOR to OUTCOME (as repub_binary)
# - District random effects are correlated across outcomes via (1 | d | leaid)
# - The estimated cross-outcome correlation is what enables calibration
# - No state random effects (single state)

mv_formula <- bf(
  mvbind(trust_binary, repub_binary) ~
    sex_mrp +
    (1 | a | age_mrp) + (1 | e | educ_mrp) + (1 | r | race_mrp) +
    dem_plurality_z + child_poverty_pct_z + log_enrollment_z + log_ppexp_z +
    (1 | d | leaid)
)

fit_mv <- brm(
  mv_formula,
  data = tn_survey,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b", resp = "trustbinary"),
    prior(normal(0, 1), class = "b", resp = "repubbinary"),
    prior(exponential(1), class = "sd", resp = "trustbinary"),
    prior(exponential(1), class = "sd", resp = "repubbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "trustbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "repubbinary"),
    prior(lkj(2), class = "cor")
  ),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 42
)

saveRDS(fit_mv, here("MRP", "District Level", "tn_calibrated_mv.rds"))


# ── 5b. Refit without dem_plurality_z ────────────────────────────────────────
# dem_plurality_z absorbs the partisan signal that calibration exploits.
# Dropping it lets the district random effects carry that variation,
# giving the calibration something to work with.

mv_formula_nopol <- bf(
  mvbind(trust_binary, repub_binary) ~
    sex_mrp +
    (1 | a | age_mrp) + (1 | e | educ_mrp) + (1 | r | race_mrp) +
    child_poverty_pct_z + log_enrollment_z + log_ppexp_z +
    (1 | d | leaid)
)

fit_mv2 <- brm(
  mv_formula_nopol,
  data = tn_survey,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b", resp = "trustbinary"),
    prior(normal(0, 1), class = "b", resp = "repubbinary"),
    prior(exponential(1), class = "sd", resp = "trustbinary"),
    prior(exponential(1), class = "sd", resp = "repubbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "trustbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "repubbinary"),
    prior(lkj(2), class = "cor")
  ),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 42
)

# ── 6. Extract correlation structure ─────────────────────────────────────────

sd_trust <- VarCorr(fit_mv2)$leaid$sd[1, 1]
sd_repub <- VarCorr(fit_mv2)$leaid$sd[2, 1]
rho <- VarCorr(fit_mv2)$leaid$cor[2, 1, 1]

cat("\n=== Correlation Structure (no dem_plurality_z) ===\n")
cat("SD(trust district RE):", round(sd_trust, 3), "\n")
cat("SD(repub district RE):", round(sd_repub, 3), "\n")
cat("Rho(trust, repub):", round(rho, 3), "\n")
cat("Calibration coefficient:", round(rho * sd_trust / sd_repub, 3), "\n")

# ── 7. Poststratify (uncalibrated) ──────────────────────────────────────────

# Drop dem_plurality_z from poststrat frame to match model
tn_poststrat_nopol <- tn_poststrat |>
  select(-dem_plurality_z)

district_ids <- unique(tn_poststrat_nopol$leaid)
chunks <- split(district_ids, ceiling(seq_along(district_ids) / 50))

uncalib_results <- vector("list", length(chunks))

for (i in seq_along(chunks)) {
  chunk_data <- tn_poststrat_nopol |> filter(leaid %in% chunks[[i]])

  pred <- fitted(
    fit_mv2,
    newdata = chunk_data,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    scale = "response",
    summary = TRUE
  )

  chunk_data$pred_trust <- pred[, "Estimate", "trustbinary"]
  chunk_data$pred_repub <- pred[, "Estimate", "repubbinary"]

  uncalib_results[[i]] <- chunk_data |>
    group_by(leaid) |>
    summarise(
      trust_uncalib = weighted.mean(pred_trust, count),
      repub_uncalib = weighted.mean(pred_repub, count),
      .groups = "drop"
    )

  rm(pred, chunk_data)
  if (i %% 5 == 0) gc()
  message("Poststrat chunk ", i, " of ", length(chunks))
}

uncalib <- bind_rows(uncalib_results) |>
  left_join(dist_vote, by = "leaid")

# ── 8. Calibration ──────────────────────────────────────────────────────────

calibrated <- uncalib |>
  filter(!is.na(repub_share), !is.na(repub_uncalib),
         repub_uncalib > 0.001, repub_uncalib < 0.999,
         repub_share > 0.001, repub_share < 0.999) |>
  mutate(
    delta_repub = qlogis(repub_share) - qlogis(repub_uncalib),
    delta_trust = rho * (sd_trust / sd_repub) * delta_repub,
    trust_calibrated = plogis(qlogis(trust_uncalib) + delta_trust)
  )

# ── 9. Results ──────────────────────────────────────────────────────────────

cat("\n=== Calibration Results ===\n")
cat("Districts calibrated:", nrow(calibrated), "\n")
cat("Mean abs trust shift:",
    round(mean(abs(calibrated$trust_calibrated - calibrated$trust_uncalib)), 4), "\n")
cat("\nTrust summary (uncalibrated):\n")
print(round(summary(calibrated$trust_uncalib), 3))
cat("\nTrust summary (calibrated):\n")
print(round(summary(calibrated$trust_calibrated), 3))

cat("\nCorr(trust, repub_share) uncalibrated:",
    round(cor(calibrated$trust_uncalib, calibrated$repub_share), 3), "\n")
cat("Corr(trust, repub_share) calibrated:",
    round(cor(calibrated$trust_calibrated, calibrated$repub_share), 3), "\n")

# ── 10. Plots ────────────────────────────────────────────────────────────────

p1 <- ggplot(calibrated, aes(x = trust_uncalib, y = trust_calibrated)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Uncalibrated Trust Estimate",
    y = "Calibrated Trust Estimate",
    title = "Calibrated vs Uncalibrated District Trust (Tennessee)",
    subtitle = paste0("rho = ", round(rho, 2),
                      ", calibration coeff = ", round(rho * sd_trust / sd_repub, 2))
  ) +
  theme_minimal()

p2 <- ggplot(calibrated, aes(x = delta_repub, y = delta_trust)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = rho * sd_trust / sd_repub, intercept = 0,
              color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Republican Logit Shift (actual - estimated)",
    y = "Implied Trust Logit Shift",
    title = "Calibration Adjustment by District",
    subtitle = "Slope = calibration coefficient"
  ) +
  theme_minimal()

print(p1)
print(p2)










# Calibrated MRP Proof of Concept: State-Level
# Tests Marble & Clinton (2026) calibration on state-level school trust
# Ben Marek | August 2025
#
# QUESTION THIS ANSWERS:
# Can calibrating to known state-level election results recover the partisan
# signal in trust estimates WITHOUT including PID as a predictor?
# The existing state model uses pid3_mrp + trump_z. This model drops both,
# lets state random effects absorb partisan variation, and calibrates to
# actual state-level Republican vote share.

# ── Setup ─────────────────────────────────────────────────────────────────────

library(here)
library(tidyverse)
library(brms)
library(cmdstanr)
library(readxl)

# ── 1. Load existing data ────────────────────────────────────────────────────

# Survey data (already prepped from Revised_Model.R)
yougov_df <- readRDS(here("MRP", "State Level", "Public Schools Revised",
                           "yougov_cleaner_synth.rds"))

# Poststrat frame WITHOUT PID (PID is now an outcome, not a predictor)
pums_df <- readRDS(here("MRP", "State Level", "pums_2023_acs5_allstates.rds"))

poststrat_nopid <- pums_df |>
  filter(complete.cases(STATE), AGEP >= 18) |>
  mutate(
    age_mrp = case_when(
      AGEP < 30  ~ "Under 30",
      AGEP < 45  ~ "30-44",
      AGEP < 65  ~ "45-64",
      AGEP >= 65 ~ "65+"),
    educ_mrp = case_when(
      SCHL %in% c("18", "19", "20") ~ "Some college",
      SCHL == "21"                   ~ "College grad",
      SCHL %in% c("22", "23", "24") ~ "Postgrad",
      TRUE                           ~ "HS or less"),
    race_mrp = case_when(
      HISP != "01"  ~ "Hispanic",
      RAC1P == "1"  ~ "White",
      RAC1P == "2"  ~ "Black",
      RAC1P == "6"  ~ "Asian",
      TRUE          ~ "Other"),
    sex_mrp = case_when(
      SEX == "1" ~ "Man",
      SEX == "2" ~ "Woman")
  ) |>
  group_by(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp) |>
  summarise(count = sum(PWGTP), .groups = "drop")

# State-level election results (ground truth)
state_results <- here("Data", "Election Results", "2024presgeresults.xlsx") |>
  read_excel() |>
  filter(complete.cases(STATE)) |>
  mutate(
    repub_share = TRUMP / (TRUMP + HARRIS),
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
      "VT" ~ "50", "VA" ~ "51", "WA" ~ "53", "WV" ~ "54", "WI" ~ "55",
      "WY" ~ "56", .default = NA)
  ) |>
  select(STATE_FIPS, repub_share)

# Existing uncalibrated MRP estimates for comparison
existing_mrp <- readRDS(here("MRP", "State Level", "Public Schools Revised",
                              "mrp_pooled_pubsch_revised.rds"))

# ── 2. Prep survey data ─────────────────────────────────────────────────────

# Add Republican binary outcome
# *** SWAP POINT: if survey has vote choice, use that instead ***
survey_mv <- yougov_df |>
  mutate(
    repub_binary = case_when(
      pid3_mrp == "Republican" ~ 1L,
      pid3_mrp %in% c("Democrat", "Independent") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  filter(!is.na(trust_schools_binary), !is.na(repub_binary))

cat("Survey N:", nrow(survey_mv), "\n")
cat("States:", n_distinct(survey_mv$STATE), "\n")
cat("Mean respondents per state:", round(nrow(survey_mv) / n_distinct(survey_mv$STATE)), "\n")


# ── 3. Multivariate model ───────────────────────────────────────────────────
#
# NO pid3_mrp as predictor, NO trump_z.
# The partisan signal goes into the state random effects instead,
# which are correlated across outcomes via (1 | s | STATE).

mv_formula <- bf(
  mvbind(trust_schools_binary, repub_binary) ~
    sex_mrp +
    (1 | a | age_mrp) + (1 | e | educ_mrp) + (1 | r | race_mrp) +
    (1 | s | STATE)
)

fit_mv <- brm(
  mv_formula,
  data = survey_mv,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b", resp = "trustschoolsbinary"),
    prior(normal(0, 1), class = "b", resp = "repubbinary"),
    prior(exponential(1), class = "sd", resp = "trustschoolsbinary"),
    prior(exponential(1), class = "sd", resp = "repubbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "trustschoolsbinary"),
    prior(normal(0, 1), class = "Intercept", resp = "repubbinary"),
    prior(lkj(2), class = "cor")
  ),
  backend = "cmdstanr",
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 42
)

saveRDS(fit_mv, here("MRP", "State Level", "calibrated_mv_state.rds"))


# ── 4. Extract correlation structure ─────────────────────────────────────────

sd_trust <- VarCorr(fit_mv)$STATE$sd[1, 1]
sd_repub <- VarCorr(fit_mv)$STATE$sd[2, 1]
rho <- VarCorr(fit_mv)$STATE$cor[2, 1, 1]

cat("\n=== State-Level Correlation Structure ===\n")
cat("SD(trust state RE):", round(sd_trust, 3), "\n")
cat("SD(repub state RE):", round(sd_repub, 3), "\n")
cat("Rho(trust, repub):", round(rho, 3), "\n")
cat("Calibration coefficient:", round(rho * sd_trust / sd_repub, 3), "\n")

# Also check the CI on rho
cat("\nRho 95% CI:",
    round(VarCorr(fit_mv)$STATE$cor[2, 3, 1], 3), "to",
    round(VarCorr(fit_mv)$STATE$cor[2, 4, 1], 3), "\n")


# ── 5. Poststratify (uncalibrated) ──────────────────────────────────────────

# Factor levels must match the model
poststrat_nopid <- poststrat_nopid |>
  mutate(
    age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college",
                                            "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic",
                                            "Asian", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman"))
  )

pred <- fitted(
  fit_mv,
  newdata = poststrat_nopid,
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian",
  scale = "response",
  summary = TRUE
)

poststrat_nopid$pred_trust <- pred[, "Estimate", "trustschoolsbinary"]
poststrat_nopid$pred_repub <- pred[, "Estimate", "repubbinary"]

uncalib <- poststrat_nopid |>
  group_by(STATE) |>
  summarise(
    trust_uncalib = weighted.mean(pred_trust, count),
    repub_uncalib = weighted.mean(pred_repub, count)
  ) |>
  left_join(state_results, by = c("STATE" = "STATE_FIPS"))


# ── 6. Calibration ──────────────────────────────────────────────────────────

calibrated <- uncalib |>
  filter(!is.na(repub_share),
         repub_uncalib > 0.001, repub_uncalib < 0.999) |>
  mutate(
    delta_repub = qlogis(repub_share) - qlogis(repub_uncalib),
    delta_trust = rho * (sd_trust / sd_repub) * delta_repub,
    trust_calibrated = plogis(qlogis(trust_uncalib) + delta_trust)
  )


# ── 7. Compare to existing MRP ──────────────────────────────────────────────

# Join existing estimates (from model with pid3 + trump_z)
calibrated <- calibrated |>
  left_join(existing_mrp |> select(STATE, trust_existing = p_mrp_est), by = "STATE")

state_abbr <- tidycensus::fips_codes |>
  transmute(STATE = state_code, abbr = state) |>
  distinct()

calibrated <- calibrated |>
  left_join(state_abbr, by = "STATE")

cat("\n=== Comparison ===\n")
cat("Corr(uncalibrated, existing MRP):",
    round(cor(calibrated$trust_uncalib, calibrated$trust_existing, use = "complete"), 3), "\n")
cat("Corr(calibrated, existing MRP):",
    round(cor(calibrated$trust_calibrated, calibrated$trust_existing, use = "complete"), 3), "\n")
cat("MAE uncalibrated vs existing:",
    round(mean(abs(calibrated$trust_uncalib - calibrated$trust_existing), na.rm = TRUE), 4), "\n")
cat("MAE calibrated vs existing:",
    round(mean(abs(calibrated$trust_calibrated - calibrated$trust_existing), na.rm = TRUE), 4), "\n")


# ── 8. Plots ─────────────────────────────────────────────────────────────────

# Calibrated vs existing MRP (the key plot)
p1 <- ggplot(calibrated, aes(x = trust_existing, y = trust_calibrated)) +
  geom_point() +
  geom_text(aes(label = abbr), nudge_y = 0.005, size = 2.5, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Existing MRP (pid3 + trump_z)",
    y = "Calibrated MRP (no pid3, calibrated to election results)",
    title = "Can calibration substitute for direct PID modeling?",
    subtitle = paste0("rho = ", round(rho, 2))
  ) +
  theme_minimal()

# Uncalibrated vs calibrated
p2 <- ggplot(calibrated) +
  geom_point(aes(x = repub_share, y = trust_uncalib), color = "grey60") +
  geom_point(aes(x = repub_share, y = trust_calibrated), color = "steelblue") +
  labs(
    x = "State Republican Vote Share",
    y = "Trust Estimate",
    title = "Trust vs Partisanship: Before (grey) and After (blue) Calibration"
  ) +
  theme_minimal()

# Calibration shift
p3 <- ggplot(calibrated, aes(x = delta_repub, y = delta_trust)) +
  geom_point() +
  geom_text(aes(label = abbr), nudge_y = 0.01, size = 2.5, alpha = 0.7) +
  geom_abline(slope = rho * sd_trust / sd_repub, intercept = 0,
              color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Republican ID Logit Shift",
    y = "Implied Trust Logit Shift",
    title = "Calibration Adjustment by State"
  ) +
  theme_minimal()

print(p1)
print(p2)
print(p3)