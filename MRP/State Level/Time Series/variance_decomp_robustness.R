# LPM Robustness Check for Variance Decomposition
# Checks whether the logistic rescaling problem affects the interpretation

library(here)
library(tidyverse)
library(lme4)

# Load the same cleaned Gallup data used in the main analysis
gallup_clean_df <- here("Final", "Time", "gallup_clean_df.rds") |> readRDS()

# Also load the original logistic results for side-by-side comparison
variance_results <- here("Final", "Time", "variance_results.rds") |> readRDS()

# Period names (replaces the imap over frames, which never used the frame object)
periods <- c("1973-1982", "1983-1991", "1992-2001",
             "2002-2009", "2010-2019", "2020-2025")

# LPM variance decomposition
variance_lpm <- map(periods, \(period) {
  df <- gallup_clean_df |>
    filter(period_mrp == period, !is.na(partyrr))

  mod_nopid <- lmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                      (1 | STATE),
                    data = df)

  mod_pid <- lmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp + pid3_mrp +
                    (1 | STATE),
                  data = df)

  tibble(
    period_mrp = period,
    n = nrow(df),
    var_nopid_lpm = VarCorr(mod_nopid)$STATE[1, 1],
    var_pid_lpm = VarCorr(mod_pid)$STATE[1, 1],
    pct_reduction_lpm = round(100 * (var_nopid_lpm - var_pid_lpm) / var_nopid_lpm, 1)
  )
}) |>
  list_rbind()

# Side-by-side comparison
comparison <- variance_results |>
  select(period_mrp, n, pct_reduction_logit = pct_reduction) |>
  left_join(
    variance_lpm |> select(period_mrp, pct_reduction_lpm),
    by = "period_mrp"
  )

print(comparison)