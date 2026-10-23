# ══════════════════════════════════════════════════════════════════════════
# Ordinal Robustness Check for State-Level Public School Trust
# Does the binary cutpoint distort the geographic/partisan pattern?
# ══════════════════════════════════════════════════════════════════════════

library(here)
library(tidyverse)
library(ordinal)
library(brms)
library(broom)

# ── 0. Data loading and cleaning ─────────────────────────────────────────
# Reproduces the cleaning pipeline from Revised_Model.R

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
      TRUE ~ NA_character_)
  )

yougov_cleaner_df <- yougov_raw_df |>
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

# Load saved binary brms fit
fit_binary <- readRDS(
  here("MRP", "State Level", "Public Schools Revised", "2024_pooled_pubsch_mr_revised.rds")
)

# Load saved binary MRP estimates (for secondary check only)
mrp_binary <- readRDS(
  here("MRP", "State Level", "Public Schools Revised", "mrp_pooled_pubsch_revised.rds")
)


# ── 1. Marginal distribution ────────────────────────────────────────────
cat("══ 1. Response distribution ══\n")
yougov_raw_df |>
  filter(!is.na(institutional_trust_public_schools)) |>
  count(institutional_trust_public_schools) |>
  mutate(pct = round(n / sum(n), 3)) |>
  print()


# ── 2. Cutpoint sensitivity (raw state means) ───────────────────────────
cat("\n══ 2. Cutpoint sensitivity ══\n")
yougov_sensitivity <- yougov_cleaner_df |>
  mutate(
    cut_top1 = as.integer(
      institutional_trust_public_schools == "A great deal"),
    cut_top2 = trust_schools_binary,
    cut_top3 = as.integer(
      institutional_trust_public_schools != "Very little")
  )

state_cuts <- yougov_sensitivity |>
  group_by(STATE) |>
  summarise(
    mean_top1 = mean(cut_top1, na.rm = TRUE),
    mean_top2 = mean(cut_top2, na.rm = TRUE),
    mean_top3 = mean(cut_top3, na.rm = TRUE)
  )

tibble(
  comparison = c("top1 vs top2", "top2 vs top3", "top1 vs top3"),
  spearman_r = c(
    cor(state_cuts$mean_top1, state_cuts$mean_top2, method = "spearman"),
    cor(state_cuts$mean_top2, state_cuts$mean_top3, method = "spearman"),
    cor(state_cuts$mean_top1, state_cuts$mean_top3, method = "spearman")
  )
) |> print()


# ── 3. Ordinal model with state random intercept ────────────────────────
cat("\n══ 3. Ordinal CLMM ══\n")
yougov_ord <- yougov_cleaner_df |>
  mutate(
    trust_ord = factor(
      institutional_trust_public_schools,
      levels = c("Very little", "Some", "Quite a lot", "A great deal"),
      ordered = TRUE),
    pid3_mrp = factor(pid3_mrp, levels = c("Democrat", "Independent", "Republican")),
    age_mrp = factor(age_mrp),
    educ_mrp = factor(educ_mrp),
    race_mrp = factor(race_mrp),
    sex_mrp = factor(sex_mrp)
  ) |>
  filter(!is.na(trust_ord))

fit_clmm <- clmm(
  trust_ord ~ sex_mrp + pid3_mrp + age_mrp + educ_mrp + race_mrp + (1 | STATE),
  data = yougov_ord,
  control = clmm.control(maxIter = 500)
)

summary(fit_clmm)
saveRDS(fit_clmm, "ordinal_conf.rds")


# ── 4. State ranking comparison (BLUP vs BLUP) ──────────────────────────
# Both quantities are residual state deviations on a log-odds scale.
# The only difference is functional form: binary logit vs cumulative logit.
cat("\n══ 4. State BLUP comparison (like-for-like) ══\n")

state_blups_ord <- ranef(fit_clmm)$STATE |>
  as_tibble(rownames = "STATE") |>
  rename(blup_ordinal = `(Intercept)`)

state_blups_binary <- ranef(fit_binary)$STATE[, , "Intercept"] |>
  as_tibble(rownames = "STATE") |>
  select(STATE, blup_binary = Estimate)

state_comparison <- state_blups_binary |>
  left_join(state_blups_ord, by = "STATE") |>
  filter(!is.na(blup_ordinal))

cat("Spearman rank correlation (binary BLUP vs ordinal BLUP):",
    round(cor(state_comparison$blup_binary, state_comparison$blup_ordinal,
              method = "spearman"), 3), "\n")
cat("Pearson correlation (binary BLUP vs ordinal BLUP):",
    round(cor(state_comparison$blup_binary, state_comparison$blup_ordinal), 3), "\n")
cat("N states:", nrow(state_comparison), "\n")

# Secondary check: poststratified MRP estimate vs ordinal BLUP.
# NOTE: this compares a poststratified quantity (reflecting both demographics
# and residual state effects) to a non-poststratified one (residual only).
# Any gap could reflect demographic composition, not functional form.
cat("\nSecondary check (MRP estimate vs ordinal BLUP, not like-for-like):\n")
secondary <- mrp_binary |>
  left_join(state_blups_ord, by = "STATE") |>
  filter(!is.na(blup_ordinal))
cat("  Spearman:",
    round(cor(secondary$p_mrp_est, secondary$blup_ordinal, method = "spearman"), 3), "\n")


# ── 5. Partisan gap check ───────────────────────────────────────────────
cat("\n══ 5. Partisan coefficients ══\n")
cat("Ordinal (clmm):\n")
coef_ord <- coef(fit_clmm)
print(coef_ord[grep("pid3_mrp", names(coef_ord))])

cat("\nBinary (brms):\n")
print(fixef(fit_binary)[grep("pid3_mrp", rownames(fixef(fit_binary))), ])


# ── 6. Proportional odds diagnostic ─────────────────────────────────────
# With ~58k obs formal tests reject everything. Instead, fit separate
# binary logits at each of the three possible cutpoints and compare
# coefficient magnitudes. Under proportional odds, coefficients should
# be roughly constant across cuts.
cat("\n══ 6. Proportional odds diagnostic ══\n")

cut_coefs <- map(1:3, \(k) {
  yougov_ord |>
    mutate(y = as.integer(as.numeric(trust_ord) > k)) |>
    glm(y ~ sex_mrp + pid3_mrp + age_mrp + educ_mrp + race_mrp,
        data = _, family = binomial) |>
    tidy() |>
    mutate(cutpoint = k)
}) |>
  list_rbind()

cat("Republican coefficient across cutpoints:\n")
cut_coefs |>
  filter(term == "pid3_mrpRepublican") |>
  select(cutpoint, estimate, std.error) |>
  print()

cat("\nIndependent coefficient across cutpoints:\n")
cut_coefs |>
  filter(term == "pid3_mrpIndependent") |>
  select(cutpoint, estimate, std.error) |>
  print()
