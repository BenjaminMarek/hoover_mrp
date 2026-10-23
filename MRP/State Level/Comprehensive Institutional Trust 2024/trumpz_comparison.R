# trump_z Model Comparison: Does state partisanship or state RE drive geography?
# Uses congress-style spec (consistent across all 8 institutions) with glmer

library(here)
library(tidyverse)
library(lme4)
library(readxl)

# ══════════════════════════════════════════════════════════════════════════════
# 1. DATA PREP (borrowed from 2024_pooled_congress.R)
# ══════════════════════════════════════════════════════════════════════════════

## Survey data
yougov_raw_df <- bind_rows(
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20240320.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241025.rds")),
  readRDS(here("Data", "YouGov Surveys", "Summer_25_Project", "say-baseline-20241121.rds"))) |>
  group_by(person_id) |>
  slice_tail(n = 1) |>
  ungroup()

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
      TRUE ~ NA_character_))

yougov_clean_df <- yougov_raw_df |>
  mutate(
    STATE = case_when(
      inputstate == "Alabama" ~ "01", inputstate == "Alaska" ~ "02",
      inputstate == "Arizona" ~ "04", inputstate == "Arkansas" ~ "05",
      inputstate == "California" ~ "06", inputstate == "Colorado" ~ "08",
      inputstate == "Connecticut" ~ "09", inputstate == "Delaware" ~ "10",
      inputstate == "District of Columbia" ~ "11", inputstate == "Florida" ~ "12",
      inputstate == "Georgia" ~ "13", inputstate == "Hawaii" ~ "15",
      inputstate == "Idaho" ~ "16", inputstate == "Illinois" ~ "17",
      inputstate == "Indiana" ~ "18", inputstate == "Iowa" ~ "19",
      inputstate == "Kansas" ~ "20", inputstate == "Kentucky" ~ "21",
      inputstate == "Louisiana" ~ "22", inputstate == "Maine" ~ "23",
      inputstate == "Maryland" ~ "24", inputstate == "Massachusetts" ~ "25",
      inputstate == "Michigan" ~ "26", inputstate == "Minnesota" ~ "27",
      inputstate == "Mississippi" ~ "28", inputstate == "Missouri" ~ "29",
      inputstate == "Montana" ~ "30", inputstate == "Nebraska" ~ "31",
      inputstate == "Nevada" ~ "32", inputstate == "New Hampshire" ~ "33",
      inputstate == "New Jersey" ~ "34", inputstate == "New Mexico" ~ "35",
      inputstate == "New York" ~ "36", inputstate == "North Carolina" ~ "37",
      inputstate == "North Dakota" ~ "38", inputstate == "Ohio" ~ "39",
      inputstate == "Oklahoma" ~ "40", inputstate == "Oregon" ~ "41",
      inputstate == "Pennsylvania" ~ "42", inputstate == "Rhode Island" ~ "44",
      inputstate == "South Carolina" ~ "45", inputstate == "South Dakota" ~ "46",
      inputstate == "Tennessee" ~ "47", inputstate == "Texas" ~ "48",
      inputstate == "Utah" ~ "49", inputstate == "Vermont" ~ "50",
      inputstate == "Virginia" ~ "51", inputstate == "Washington" ~ "53",
      inputstate == "West Virginia" ~ "54", inputstate == "Wisconsin" ~ "55",
      inputstate == "Wyoming" ~ "56", TRUE ~ NA_character_),
    trust_schools_binary = case_when(
      institutional_trust_public_schools %in% c("A great deal", "Quite a lot") ~ 1,
      institutional_trust_public_schools %in% c("Some", "Very little") ~ 0,
      TRUE ~ NA_real_)) |>
  filter(complete.cases(age_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp,
                        STATE, trust_schools_binary))

## Trump z-score
state_results <- here("Data", "Election Results", "2024presgeresults.xlsx") |>
  read_excel() |>
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
      "VT" ~ "50", "VA" ~ "51", "WA" ~ "53", "WV" ~ "54", "WI" ~ "55",
      "WY" ~ "56", .default = NA))

## Prepare regression data (factor levels matching congress spec)
survey_df <- yougov_clean_df |>
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman"))) |>
  filter(complete.cases(pid3_mrp)) |>
  left_join(state_results |> select(STATE_FIPS, trump_z),
            by = c("STATE" = "STATE_FIPS"))

## Poststrat frame (already has PID from synthjoint)
poststrat <- here("MRP", "State Level", "Comprehensive Institutional Trust 2024",
                  "2024_pooled_poststrat.rds") |>
  read_rds() |>
  mutate(
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp, levels = c("Under 30", "30-44", "45-64", "65+")),
    educ_mrp = factor(educ_mrp, levels = c("HS or less", "Some college", "College grad", "Postgrad")),
    race_mrp = factor(race_mrp, levels = c("White", "Black", "Hispanic", "Asian", "Other")),
    sex_mrp = factor(sex_mrp, levels = c("Man", "Woman")))

# ══════════════════════════════════════════════════════════════════════════════
# 2. FIT FOUR MODELS (congress-style spec, glmer for speed)
# ══════════════════════════════════════════════════════════════════════════════

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

cat("Fitting Model A: full (STATE RE + trump_z)...\n")
mod_a <- glmer(
  trust_schools_binary ~ sex_mrp + pid3_mrp +
    (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|STATE) +
    (1|STATE:educ_mrp) + (1|STATE:race_mrp) + trump_z,
  data = survey_df, family = binomial, control = ctrl)

cat("Fitting Model B: trump_z only (no STATE RE, keep state interactions)...\n")
mod_b <- glmer(
  trust_schools_binary ~ sex_mrp + pid3_mrp +
    (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) +
    (1|STATE:educ_mrp) + (1|STATE:race_mrp) + trump_z,
  data = survey_df, family = binomial, control = ctrl)

cat("Fitting Model C: STATE RE only (no trump_z)...\n")
mod_c <- glmer(
  trust_schools_binary ~ sex_mrp + pid3_mrp +
    (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + (1|STATE) +
    (1|STATE:educ_mrp) + (1|STATE:race_mrp),
  data = survey_df, family = binomial, control = ctrl)

cat("Fitting Model D: trump_z only (no state-indexed REs at all)...\n")
mod_d <- glmer(
  trust_schools_binary ~ sex_mrp + pid3_mrp +
    (1|age_mrp) + (1|educ_mrp) + (1|race_mrp) + trump_z,
  data = survey_df, family = binomial, control = ctrl)

# ══════════════════════════════════════════════════════════════════════════════
# 3. POSTSTRATIFY (manual: predict on frame, weighted mean by state)
# ══════════════════════════════════════════════════════════════════════════════

poststratify <- function(mod, frame) {
  frame |>
    mutate(pred = predict(mod, newdata = frame, type = "response",
                          allow.new.levels = TRUE)) |>
    summarise(mrp_est = weighted.mean(pred, count), .by = STATE)
}

mrp_a <- poststratify(mod_a, poststrat) |> rename(est_a = mrp_est)
mrp_b <- poststratify(mod_b, poststrat) |> rename(est_b = mrp_est)
mrp_c <- poststratify(mod_c, poststrat) |> rename(est_c = mrp_est)
mrp_d <- poststratify(mod_d, poststrat) |> rename(est_d = mrp_est)

estimates <- mrp_a |>
  left_join(mrp_b, by = "STATE") |>
  left_join(mrp_c, by = "STATE") |>
  left_join(mrp_d, by = "STATE") |>
  left_join(state_results |> select(STATE_FIPS, trump_z),
            by = c("STATE" = "STATE_FIPS"))

# ══════════════════════════════════════════════════════════════════════════════
# 4. COMPARISONS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n══ Model summaries ══\n")
cat("A (full):            AIC =", AIC(mod_a), "\n")
cat("B (no STATE RE):     AIC =", AIC(mod_b), "\n")
cat("C (no trump_z):      AIC =", AIC(mod_c), "\n")
cat("D (trump_z only):    AIC =", AIC(mod_d), "\n")

cat("\n══ State RE variance ══\n")
cat("A (full):        ", VarCorr(mod_a)$STATE[1,1], "\n")
cat("C (no trump_z):  ", VarCorr(mod_c)$STATE[1,1], "\n")
cat("trump_z coefficient in A:", fixef(mod_a)["trump_z"], "\n")
cat("trump_z coefficient in B:", fixef(mod_b)["trump_z"], "\n")
cat("trump_z coefficient in D:", fixef(mod_d)["trump_z"], "\n")

cat("\n══ MRP estimate correlations (Spearman) ══\n")
cat("Key question: can trump_z replace state REs?\n\n")
cat("A vs C (does trump_z add anything?):        ",
    cor(estimates$est_a, estimates$est_c, method = "spearman") |> round(4), "\n")
cat("A vs D (can trump_z alone match full?):      ",
    cor(estimates$est_a, estimates$est_d, method = "spearman") |> round(4), "\n")
cat("C vs D (state RE alone vs trump_z alone):    ",
    cor(estimates$est_c, estimates$est_d, method = "spearman") |> round(4), "\n")
cat("A vs B (does dropping STATE RE matter?):     ",
    cor(estimates$est_a, estimates$est_b, method = "spearman") |> round(4), "\n")

cat("\n══ MRP estimate correlations (Pearson) ══\n")
cat("A vs C:", cor(estimates$est_a, estimates$est_c) |> round(4), "\n")
cat("A vs D:", cor(estimates$est_a, estimates$est_d) |> round(4), "\n")
cat("C vs D:", cor(estimates$est_c, estimates$est_d) |> round(4), "\n")
cat("A vs B:", cor(estimates$est_a, estimates$est_b) |> round(4), "\n")

cat("\n══ Spread of state estimates (SD across states) ══\n")
cat("A (full):          ", sd(estimates$est_a) |> round(4), "\n")
cat("B (no STATE RE):   ", sd(estimates$est_b) |> round(4), "\n")
cat("C (no trump_z):    ", sd(estimates$est_c) |> round(4), "\n")
cat("D (trump_z only):  ", sd(estimates$est_d) |> round(4), "\n")

cat("\n══ Max absolute difference from full model (A) ══\n")
cat("B (no STATE RE):  ", max(abs(estimates$est_a - estimates$est_b)) |> round(4), "\n")
cat("C (no trump_z):   ", max(abs(estimates$est_a - estimates$est_c)) |> round(4), "\n")
cat("D (trump_z only): ", max(abs(estimates$est_a - estimates$est_d)) |> round(4), "\n")

cat("\n══ States with largest shifts when dropping STATE RE (A vs D) ══\n")
estimates |>
  mutate(shift = est_a - est_d) |>
  arrange(desc(abs(shift))) |>
  head(10) |>
  print()

# ══════════════════════════════════════════════════════════════════════════════
# 5. DIAGNOSTIC PLOTS
# ══════════════════════════════════════════════════════════════════════════════

## A vs D: the main question
p1 <- ggplot(estimates, aes(x = est_d, y = est_a)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 0.5) +
  labs(x = "Model D: trump_z only (no state REs)",
       y = "Model A: full (STATE RE + trump_z)",
       title = "Can trump_z alone replace state random effects?",
       subtitle = paste0("Pearson r = ",
                         round(cor(estimates$est_a, estimates$est_d), 3))) +
  theme_minimal()

## A vs C: does trump_z add anything?
p2 <- ggplot(estimates, aes(x = est_c, y = est_a)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  labs(x = "Model C: STATE RE only (no trump_z)",
       y = "Model A: full (STATE RE + trump_z)",
       title = "What does trump_z add beyond state REs?",
       subtitle = paste0("Pearson r = ",
                         round(cor(estimates$est_a, estimates$est_c), 3))) +
  theme_minimal()

## Spread comparison: how much state variation does each model produce?
p3 <- estimates |>
  pivot_longer(cols = starts_with("est_"),
               names_to = "model", values_to = "estimate",
               names_prefix = "est_") |>
  mutate(model = recode(model,
    "a" = "A: Full",
    "b" = "B: No STATE RE",
    "c" = "C: No trump_z",
    "d" = "D: trump_z only")) |>
  ggplot(aes(x = estimate, fill = model)) +
  geom_density(alpha = 0.4) +
  labs(x = "State-level MRP estimate", fill = "Model",
       title = "Distribution of state estimates across specifications") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
print(p2)
print(p3)

ggsave("trump_z_vs_full.png", p1, width = 7, height = 6, dpi = 300)
ggsave("state_re_vs_full.png", p2, width = 7, height = 6, dpi = 300)
ggsave("estimate_distributions.png", p3, width = 8, height = 5, dpi = 300)

cat("\n══ INTERPRETATION GUIDE ══\n")
cat("If A ≈ D (high correlation, similar spread): trump_z captures the state\n")
cat("  geography and the 'Republican in CA ≈ Republican in AL' claim holds.\n")
cat("If A ≈ C but A ≠ D: the state REs are doing the work, trump_z is\n")
cat("  redundant, and the near-zero trump_z coefficient is mechanical.\n")
cat("If A ≠ C and A ≠ D: both contribute, and neither alone is sufficient.\n")