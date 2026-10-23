# Test Doc For Inst Trust Correlation

library(here)
library(brms)
library(ppcor)
library(tidyverse)
library(readxl)


base_path <- here("MRP", "State Level", "Comprehensive Institutional Trust 2024")

# MRP estimates
mrp_congress <- read_rds(file.path(base_path, "Congress", "mrp_pooled_congress.rds"))
mrp_media <- read_rds(file.path(base_path, "Media", "mrp_pooled_media.rds"))
mrp_military <- read_rds(file.path(base_path, "Military", "mrp_pooled_military.rds"))
mrp_police <- read_rds(file.path(base_path, "Police", "mrp_pooled_police.rds"))
mrp_religious <- read_rds(file.path(base_path, "Religious Leaders", "mrp_pooled_religious_leaders.rds"))
mrp_scotus <- read_rds(file.path(base_path, "SCOTUS", "mrp_pooled_scotus.rds"))
mrp_universities <- read_rds(file.path(base_path, "Universities", "mrp_pooled_unis.rds"))
mrp_schools <- read_rds(file.path(base_path, "Public Schools", "mrp_pooled_pubsch.rds"))

mrp_estimates <- bind_rows(mrp_congress |> mutate(institution = "congress"),
  mrp_media |> mutate(institution = "media"),
  mrp_military |> mutate(institution = "military"),
  mrp_police |> mutate(institution = "police"),
  mrp_religious |> mutate(institution = "religious"),
  mrp_scotus |> mutate(institution = "scotus"),
  mrp_universities |> mutate(institution = "universities"),
  mrp_schools |> mutate(institution = "schools"))

# Fitted models
fit_congress <- read_rds(file.path(base_path, "Congress", "2024_pooled_congress_mr.rds"))
fit_media <- read_rds(file.path(base_path, "Media", "2024_pooled_media_mr.rds"))
fit_military <- read_rds(file.path(base_path, "Military", "2024_pooled_military_mr.rds"))
fit_police <- read_rds(file.path(base_path, "Police", "2024_pooled_police_mr.rds"))
fit_religious <- read_rds(file.path(base_path, "Religious Leaders", "2024_pooled_religious_leaders_mr.rds"))
fit_scotus <- read_rds(file.path(base_path, "SCOTUS", "2024_pooled_scotus_mr.rds"))
fit_universities <- read_rds(file.path(base_path, "Universities", "2024_pooled_unis_mr.rds"))
fit_schools <- read_rds(file.path(base_path, "Public Schools", "2024_pooled_pubsch_mr.rds"))

fits <- list(congress = fit_congress,
  media = fit_media,
  military = fit_military,
  police = fit_police,
  religious = fit_religious,
  scotus = fit_scotus,
  universities = fit_universities,
  schools = fit_schools)


# Summary df
summary_stats <- fits |>
  imap_dfr(\(fit, inst) {
    fe <- fixef(fit)
    vc <- VarCorr(fit, summary = TRUE)
    est <- mrp_estimates |> filter(institution == inst) |> pull(p_mrp_est)
    tibble(
      institution = inst,
      pid_rep = fe["pid3_mrpRepublican", "Estimate"],
      pid_rep_se = fe["pid3_mrpRepublican", "Est.Error"],
      trump_z = fe["trump_z", "Estimate"],
      trump_z_se = fe["trump_z", "Est.Error"],
      sd_state = vc$STATE$sd[, "Estimate"],
      mean_trust = mean(est),
      sd_trust = sd(est),
      min_trust = min(est),
      max_trust = max(est)
    )
  })

summary_stats

mrp_estimates |>
  ggplot(aes(x = p_mrp_est)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  facet_wrap(~institution, scales = "free_y") +
  labs(
    x = "State-Level Trust Estimate",
    y = "Density",
    title = "Distribution of State-Level Trust by Institution"
  ) +
  theme_minimal()

# =============================================================================
# Test 1: Coefficient Position Plot
# =============================================================================

# Extract PID (Republican vs Democrat) and trump_z coefficients
coef_data <- fits |>
  imap_dfr(\(fit, inst) {
    fe <- fixef(fit)
    tibble(
      institution = inst,
      pid_rep = fe["pid3_mrpRepublican", "Estimate"],
      pid_rep_lower = fe["pid3_mrpRepublican", "Q2.5"],
      pid_rep_upper = fe["pid3_mrpRepublican", "Q97.5"],
      trump_z = fe["trump_z", "Estimate"],
      trump_z_lower = fe["trump_z", "Q2.5"],
      trump_z_upper = fe["trump_z", "Q97.5"]
    )
  })

# Plot
ggplot(coef_data, aes(x = pid_rep, y = trump_z, label = institution)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = trump_z_lower, ymax = trump_z_upper), width = 0, alpha = 0.4) +
  geom_errorbarh(aes(xmin = pid_rep_lower, xmax = pid_rep_upper), height = 0, alpha = 0.4) +
  geom_point(size = 2) +
  geom_text(vjust = -1, size = 3) +
  labs(
    x = "Individual Partisan Effect (Republican vs Democrat)",
    y = "Contextual Effect (trump_z)",
    title = "Institutional Trust: Individual vs Contextual Partisan Effects"
  ) +
  theme_minimal()

# =============================================================================
# Test 2: State-Level Estimate Correlations
# =============================================================================

cor_matrix <- mrp_estimates |>
  select(STATE, institution, p_mrp_est) |>
  pivot_wider(names_from = institution, values_from = p_mrp_est) |>
  select(-STATE) |>
  cor()

# Heatmap
cor_matrix |>
  as_tibble(rownames = "inst1") |>
  pivot_longer(-inst1, names_to = "inst2", values_to = "r") |>
  ggplot(aes(x = inst1, y = inst2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0, limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "r", title = "State-Level Trust Correlations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# Test 3: Residual State Variance
# =============================================================================

fits |>
  imap_dfr(\(fit, inst) {
    vc <- VarCorr(fit, summary = TRUE)
    tibble(
      institution = inst,
      sd_state = vc$STATE$sd[, "Estimate"],
      sd_state_lower = vc$STATE$sd[, "Q2.5"],
      sd_state_upper = vc$STATE$sd[, "Q97.5"]
    )
  }) |>
  ggplot(aes(x = reorder(institution, sd_state), y = sd_state)) +
  geom_pointrange(aes(ymin = sd_state_lower, ymax = sd_state_upper)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Residual State SD (after trump_z)",
    title = "Unexplained State-Level Variation by Institution"
  ) +
  theme_minimal()

# Highest deviations from model prediction
ranef(fit_schools)$STATE |>
  as_tibble(rownames = "state") |>
  arrange(desc(abs(Estimate.Intercept)))


# Correlations with partisanship at state level
# =============================================================================
# State partisanship data
# =============================================================================

state_trump <- here("Data", "Election Results", "2024presgeresults.xlsx") |>
  read_excel() |>
  filter(complete.cases(STATE)) |>
  transmute(
    STATE = case_match(STATE,
      "AL" ~ "01", "AK" ~ "02", "AZ" ~ "04", "AR" ~ "05", "CA" ~ "06",
      "CO" ~ "08", "CT" ~ "09", "DE" ~ "10", "DC" ~ "11", "FL" ~ "12",
      "GA" ~ "13", "HI" ~ "15", "ID" ~ "16", "IL" ~ "17", "IN" ~ "18",
      "IA" ~ "19", "KS" ~ "20", "KY" ~ "21", "LA" ~ "22", "ME" ~ "23",
      "MD" ~ "24", "MA" ~ "25", "MI" ~ "26", "MN" ~ "27", "MS" ~ "28",
      "MO" ~ "29", "MT" ~ "30", "NE" ~ "31", "NV" ~ "32", "NH" ~ "33",
      "NJ" ~ "34", "NM" ~ "35", "NY" ~ "36", "NC" ~ "37", "ND" ~ "38",
      "OH" ~ "39", "OK" ~ "40", "OR" ~ "41", "PA" ~ "42", "RI" ~ "44",
      "SC" ~ "45", "SD" ~ "46", "TN" ~ "47", "TX" ~ "48", "UT" ~ "49",
      "VT" ~ "50", "VA" ~ "51", "WA" ~ "53", "WV" ~ "54", "WI" ~ "55", "WY" ~ "56"
    ),
    trump_share = TRUMP / (TRUMP + HARRIS)
  )

# =============================================================================
# Bivariate correlations: trust ~ trump_share by institution
# =============================================================================

mrp_estimates |>
  left_join(state_trump, by = "STATE") |>
  group_by(institution) |>
  summarise(r_trump = cor(p_mrp_est, trump_share)) |>
  arrange(r_trump)

# =============================================================================
# School BLUPs: correlation with trump_share
# =============================================================================

school_blups <- ranef(fit_schools)$STATE |>
  as_tibble(rownames = "STATE") |>
  select(STATE, blup = Estimate.Intercept) |>
  left_join(state_trump, by = "STATE")

cor(school_blups$blup, school_blups$trump_share)

school_blups |>
  ggplot(aes(x = trump_share, y = blup)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Trump Vote Share (2024)",
    y = "School Trust Residual (BLUP)",
    title = "School Trust Residuals vs State Partisanship"
  ) +
  theme_minimal()

# Saving
saveRDS(mrp_estimates, "mrp_estimates_all.rds")
saveRDS(summary_stats, "summary_stats.rds")
saveRDS(coef_data, "coef_data.rds")
saveRDS(state_variance, "state_variance.rds")
saveRDS(cor_matrix, "cor_matrix.rds")
saveRDS(trump_cors, "trump_cors.rds")
saveRDS(demo_ranefs, "demo_ranefs.rds")
saveRDS(demographic_ranges, "demographic_ranges.rds")