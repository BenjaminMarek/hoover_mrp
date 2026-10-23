# =====================================================================
# calibrated_mrp_state_test.R
# State-level feasibility test of calibrated MRP
# (Marble and Clinton 2026, Political Analysis, doi:10.1017/pan.2026.10044)
# lme4 plug-in point estimates; no uncertainty propagation.
# Ben Marek | October 2026
#
# Calibration variable: 2024 presidential vote (presvote24post, Trump vs Harris),
# calibrated to state Trump two-party share. PID available for comparison.
# Two specifications:
#   "none"       : no partisan predictors
#   "state_vote" : state 2024 Trump share as predictor (same scaling as trump_z)
# For each: separate and joint models, rho and beta, cell-level logit-shift
# calibration, comparison to existing pid3 + trump_z estimates, and a
# sparse-regime pseudo-ground-truth test.
# =====================================================================

suppressPackageStartupMessages({
  library(here); library(dplyr); library(tidyr)
  library(lme4); library(ggplot2); library(readxl)
})
set.seed(20261023)

# ---- 0. Config --------------------------------------------------------
CALIB              <- "vote"   # "vote" (presvote24post) or "pid" (earlier run)
POST_ELECTION_ONLY <- FALSE    # TRUE drops presvote24post responses fielded on/before Nov 5, 2024

SPECS         <- c("none", "state_vote")
DROP_DC       <- FALSE
RUN_SUBSAMPLE <- TRUE
SUB_N         <- c(10, 30)   # respondents per state in sparse-regime test
SUB_REPS      <- 20

AGE_LABELS  <- c("Under 30", "30-44", "45-64", "65+")
EDUC_LABELS <- c("HS or less", "Some college", "College grad", "Postgrad")
RACE_LABELS <- c("White", "Black", "Hispanic", "Asian", "Other")
SEX_LABELS  <- c("Man", "Woman")

ELECTION_DAY <- as.Date("2024-11-05")
ctrl    <- glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
out_dir <- here("MRP", "State Level", "Public Schools Revised", "calibration_test")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Helpers -------------------------------------------------------
fips_tbl <- tidycensus::fips_codes |>
  distinct(abb = state, STATE = state_code) |>
  filter(as.integer(STATE) <= 56)

to_fips <- function(x) {
  x   <- trimws(as.character(x))
  num <- suppressWarnings(as.integer(x))
  out <- ifelse(!is.na(num), sprintf("%02d", num),
                fips_tbl$STATE[match(toupper(x), fips_tbl$abb)])
  out[!out %in% fips_tbl$STATE] <- NA
  out
}

as_chr <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x)
  as.character(x)
}

code_vote <- function(x) case_when(x == "Donald Trump"  ~ 1L,
                                   x == "Kamala Harris" ~ 0L,
                                   TRUE ~ NA_integer_)  # third party, non-voters dropped

solve_delta <- function(eta, w, target) {
  uniroot(\(d) sum(w * plogis(eta + d)) / sum(w) - target,
          c(-15, 15), tol = 1e-10)$root
}

# Cell-level calibration (M&C eqs. 2, 3, 6); aggregate shortcut kept for comparison
calibrate <- function(cells, beta, targets) {
  cells <- mutate(cells, STATE = as.character(STATE))
  st <- cells |>
    inner_join(targets, by = "STATE") |>
    summarise(pop         = sum(count),
              p_repub_unc = sum(count * plogis(eta_repub)) / pop,
              p_trust_unc = sum(count * plogis(eta_trust)) / pop,
              target      = first(target),
              delta_r     = solve_delta(eta_repub, count, first(target)),
              .by = STATE) |>
    arrange(STATE) |>
    mutate(delta_r_c = delta_r - weighted.mean(delta_r, pop),  # ad hoc centered variant
           delta_t   = beta * delta_r,
           delta_t_c = beta * delta_r_c)
  cal <- cells |>
    inner_join(select(st, STATE, delta_t, delta_t_c), by = "STATE") |>
    summarise(p_trust_cal   = sum(count * plogis(eta_trust + first(delta_t)))   / sum(count),
              p_trust_cal_c = sum(count * plogis(eta_trust + first(delta_t_c))) / sum(count),
              .by = STATE)
  st |>
    left_join(cal, by = "STATE") |>
    mutate(p_trust_cal_agg = plogis(qlogis(p_trust_unc) + delta_t))
}

cmp <- function(x, y) c(r = cor(x, y),
                        rmsd_pp = 100 * sqrt(mean((x - y)^2)),
                        mean_diff_pp = 100 * mean(x - y))

rmse  <- \(x, y) 100 * sqrt(mean((x - y)^2))
re_st <- function(m) { r <- ranef(m)$STATE; tibble(STATE = rownames(r), u = r[, 1]) }

# ---- 2. Survey --------------------------------------------------------
yougov_df <- readRDS(here("MRP", "State Level", "Public Schools Revised", "yougov_cleaner_synth.rds"))

calib_label <- switch(CALIB,
  vote = if (POST_ELECTION_ONLY) "2024 vote (post-election reports)" else "2024 vote (presvote24post)",
  pid  = "Republican PID")

svy <- yougov_df |>
  mutate(
    STATE      = to_fips(STATE),
    trust_schools_binary = as.integer(trust_schools_binary),
    field_date = as.Date(starttime),
    vote_raw   = as_chr(presvote24post),
    vote_post  = code_vote(vote_raw),
    repub_binary = if (CALIB == "pid") {
      as.integer(as.character(pid3_mrp) == "Republican")
    } else {
      if_else(!POST_ELECTION_ONLY | field_date > ELECTION_DAY, vote_post, NA_integer_)
    },
    across(c(age_mrp, educ_mrp, race_mrp, sex_mrp), as.character)
  ) |>
  filter(!is.na(STATE), !is.na(age_mrp), !is.na(educ_mrp),
         !is.na(race_mrp), !is.na(sex_mrp))

stopifnot(all(svy$trust_schools_binary %in% c(0L, 1L, NA)),
          all(svy$repub_binary %in% c(0L, 1L, NA)))

cat("\npresvote24post coding check:\n")
print(table(raw = svy$vote_raw, coded = svy$vote_post, useNA = "ifany"))

cat("\nTiming of Trump/Harris responses in presvote24post:\n")
svy |>
  filter(!is.na(vote_post)) |>
  mutate(timing = case_when(is.na(field_date)           ~ "unknown",
                            field_date <= ELECTION_DAY  ~ "on/before Nov 5 (intent)",
                            TRUE                        ~ "after Nov 5 (retrospective)")) |>
  count(timing) |>
  mutate(share = round(n / sum(n), 3)) |>
  print()

cat(sprintf("\nRaw unweighted share = 1 for %s: %.3f (N = %d)\n",
            calib_label, mean(svy$repub_binary, na.rm = TRUE), sum(!is.na(svy$repub_binary))))

chk <- function(v, labs) {
  got <- sort(unique(svy[[v]]))
  if (!setequal(got, labs)) stop(v, ": survey has [", paste(got, collapse = " | "),
                                 "] but frame labels are [", paste(labs, collapse = " | "), "]")
}
chk("age_mrp", AGE_LABELS); chk("educ_mrp", EDUC_LABELS)
chk("race_mrp", RACE_LABELS); chk("sex_mrp", SEX_LABELS)

# ---- 3. Poststrat frame (demographics only, all adults) --------------
frame <- readRDS(here("MRP", "State Level", "pums_2023_acs5_allstates.rds")) |>
  filter(complete.cases(STATE)) |>
  mutate(across(c(AGEP, SCHL, RAC1P, HISP, SEX, PWGTP), \(x) as.numeric(as.character(x)))) |>
  filter(AGEP >= 18) |>
  transmute(
    STATE    = to_fips(STATE),
    age_mrp  = case_when(AGEP < 30 ~ AGE_LABELS[1], AGEP < 45 ~ AGE_LABELS[2],
                         AGEP < 65 ~ AGE_LABELS[3], TRUE      ~ AGE_LABELS[4]),
    educ_mrp = case_when(SCHL %in% 18:20 ~ EDUC_LABELS[2],
                         SCHL == 21      ~ EDUC_LABELS[3],
                         SCHL %in% 22:24 ~ EDUC_LABELS[4],
                         TRUE            ~ EDUC_LABELS[1]),
    race_mrp = case_when(HISP != 1  ~ RACE_LABELS[3],
                         RAC1P == 1 ~ RACE_LABELS[1],
                         RAC1P == 2 ~ RACE_LABELS[2],
                         RAC1P == 6 ~ RACE_LABELS[4],
                         TRUE       ~ RACE_LABELS[5]),
    sex_mrp  = if_else(SEX == 1, SEX_LABELS[1], SEX_LABELS[2]),
    PWGTP
  ) |>
  summarise(count = sum(PWGTP), .by = c(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp))

cat("\nPopulated cells per state (max 160):\n")
print(summary(count(frame, STATE)$n))

# ---- 4. Targets, state vote predictor, existing estimates ------------
targets <- read_excel(here("Data", "Election Results", "2024presgeresults.xlsx")) |>
  filter(complete.cases(STATE)) |>
  transmute(STATE  = to_fips(STATE),
            TRUMP  = readr::parse_number(as.character(TRUMP)),
            HARRIS = readr::parse_number(as.character(HARRIS))) |>
  filter(!is.na(STATE)) |>
  mutate(target = TRUMP / (TRUMP + HARRIS),
         vote_z = (target - mean(target)) / (2 * sd(target)))  # identical to trump_z construction

existing <- readRDS(here("MRP", "State Level", "Public Schools Revised", "mrp_pooled_pubsch_revised.rds")) |>
  transmute(STATE = to_fips(STATE), p_existing = p_mrp_est)

stopifnot(!anyDuplicated(targets$STATE), !anyDuplicated(existing$STATE))

states <- Reduce(intersect, list(unique(svy$STATE), unique(frame$STATE),
                                 targets$STATE, existing$STATE))
if (DROP_DC) states <- setdiff(states, "11")
cat("\nStates used:", length(states), "| dropped:",
    paste(setdiff(union(unique(svy$STATE), targets$STATE), states), collapse = ", "), "\n")

nat_target <- with(filter(targets, STATE %in% states), sum(TRUMP) / sum(TRUMP + HARRIS))
vz         <- select(targets, STATE, vote_z)
tg         <- targets |> filter(STATE %in% states) |> select(STATE, target)
existing   <- filter(existing, STATE %in% states)

fac <- function(d) d |>
  filter(STATE %in% states) |>
  left_join(vz, by = "STATE") |>
  mutate(STATE    = factor(STATE, levels = sort(states)),
         age_mrp  = factor(age_mrp, AGE_LABELS),
         educ_mrp = factor(educ_mrp, EDUC_LABELS),
         race_mrp = factor(race_mrp, RACE_LABELS),
         sex_mrp  = factor(sex_mrp, SEX_LABELS))
svy   <- fac(svy)
frame <- fac(frame)

cat(sprintf("Survey N: trust %d | %s %d | both %d\n",
            sum(!is.na(svy$trust_schools_binary)), calib_label,
            sum(!is.na(svy$repub_binary)),
            sum(!is.na(svy$trust_schools_binary) & !is.na(svy$repub_binary))))

# ---- 5. Model functions ----------------------------------------------
fit_sep <- function(dat, y, use_vote) {
  f <- as.formula(paste(y, "~ sex_mrp + (1 | age_mrp) + (1 | educ_mrp) + (1 | race_mrp) + (1 | STATE)",
                        if (use_vote) "+ vote_z" else ""))
  glmer(f, data = dat[!is.na(dat[[y]]), ], family = binomial, control = ctrl)
}

stack_long <- function(dat) {
  bind_rows(
    dat |> filter(!is.na(trust_schools_binary)) |> mutate(y = trust_schools_binary, outcome = "trust"),
    dat |> filter(!is.na(repub_binary))         |> mutate(y = repub_binary,         outcome = "repub")
  ) |>
    mutate(outcome = factor(outcome, c("trust", "repub")),
           is_t = as.numeric(outcome == "trust"), is_r = 1 - is_t)
}

# Demographic intercepts outcome-specific and independent (as in the separate models);
# only state intercepts are correlated, so Sigma_state is the sole difference.
fit_joint <- function(dat, use_vote) {
  f <- as.formula(paste(
    "y ~ 0 + outcome + outcome:sex_mrp", if (use_vote) "+ outcome:vote_z" else "",
    "+ (0 + is_t | age_mrp)  + (0 + is_r | age_mrp)",
    "+ (0 + is_t | educ_mrp) + (0 + is_r | educ_mrp)",
    "+ (0 + is_t | race_mrp) + (0 + is_r | race_mrp)",
    "+ (0 + outcome | STATE)"))
  glmer(f, data = stack_long(dat), family = binomial, control = ctrl)
}

sigma_from <- function(m) {
  S <- VarCorr(m)$STATE[c("outcometrust", "outcomerepub"), c("outcometrust", "outcomerepub")]
  sd_t <- sqrt(S[1, 1]); sd_r <- sqrt(S[2, 2])
  list(sd_t = sd_t, sd_r = sd_r, rho = S[1, 2] / (sd_t * sd_r),
       beta = S[1, 2] / S[2, 2], singular = isSingular(m))
}

predict_cells_joint <- function(m, frame) {
  nd <- bind_rows(mutate(frame, outcome = "trust"), mutate(frame, outcome = "repub")) |>
    mutate(outcome = factor(outcome, c("trust", "repub")),
           is_t = as.numeric(outcome == "trust"), is_r = 1 - is_t)
  nd$eta <- predict(m, newdata = nd, type = "link", allow.new.levels = FALSE)
  nd |>
    select(-is_t, -is_r) |>
    pivot_wider(names_from = outcome, values_from = eta, names_prefix = "eta_")
}

predict_cells_sep <- function(mt, mr, frame) frame |>
  mutate(eta_trust = predict(mt, newdata = frame, type = "link", allow.new.levels = FALSE),
         eta_repub = predict(mr, newdata = frame, type = "link", allow.new.levels = FALSE))

# ---- 6. Sparse-regime pseudo-ground-truth test ------------------------
# Subsample n per state, calibrate to the FULL-SAMPLE calib-variable estimate,
# score against the FULL-SAMPLE trust estimate (same spec). Isolates the
# variance-reduction channel; by construction it cannot show bias correction.
run_subsample <- function(use_vote, S_full, st_full) {
  bench     <- select(st_full, STATE, bench_trust = p_trust_unc)
  pseudo_tg <- transmute(st_full, STATE, target = p_repub_unc)
  svy_cc    <- filter(svy, !is.na(trust_schools_binary), !is.na(repub_binary))

  one_rep <- function(n) tryCatch({
    sub   <- svy_cc |> slice_sample(n = n, by = STATE)
    m     <- suppressMessages(suppressWarnings(fit_joint(sub, use_vote)))
    S     <- sigma_from(m)
    cells <- predict_cells_joint(m, frame)
    orc   <- calibrate(cells, S_full$beta, pseudo_tg)                          # oracle beta
    own   <- if (S$sd_r > 1e-3) calibrate(cells, S$beta, pseudo_tg) else NULL  # subsample beta
    b     <- bench$bench_trust[match(orc$STATE, bench$STATE)]
    tibble(n_per_state = n, rho_sub = S$rho, beta_sub = S$beta,
           sd_t_sub = S$sd_t, sd_r_sub = S$sd_r,
           state_degenerate = is.nan(S$rho) | S$sd_t < 1e-4 | S$sd_r < 1e-4 | abs(S$rho) > 0.99,
           rmse_unc        = rmse(orc$p_trust_unc, b),
           rmse_cal_own    = if (is.null(own)) NA_real_ else rmse(own$p_trust_cal, b),
           rmse_cal_oracle = rmse(orc$p_trust_cal, b))
  }, error = \(e) NULL)

  raw <- bind_rows(lapply(rep(SUB_N, each = SUB_REPS), one_rep))
  summ <- raw |>
    summarise(reps             = n(),
              share_degenerate = mean(state_degenerate),
              median_rho_sub   = median(rho_sub, na.rm = TRUE),
              median_beta_sub  = median(beta_sub, na.rm = TRUE),
              beta_sub_iqr     = IQR(beta_sub, na.rm = TRUE),
              rmse_unc         = mean(rmse_unc),
              rmse_cal_own     = mean(rmse_cal_own, na.rm = TRUE),
              rmse_cal_oracle  = mean(rmse_cal_oracle),
              improved_own     = mean(rmse_cal_own < rmse_unc, na.rm = TRUE),
              improved_oracle  = mean(rmse_cal_oracle < rmse_unc),
              .by = n_per_state)
  cat("\n--- Sparse-regime test (RMSE in pp vs full-sample benchmark) ---\n")
  print(summ, width = Inf)
  list(raw = raw, summary = summ)
}

# ---- 7. Run one specification ----------------------------------------
run_spec <- function(spec) {
  use_vote <- spec == "state_vote"
  cat("\n\n################  SPEC:", spec, " ################\n")

  m_t <- fit_sep(svy, "trust_schools_binary", use_vote)
  m_r <- fit_sep(svy, "repub_binary", use_vote)
  cat("Separate models done; fitting joint model...\n")
  m_j <- fit_joint(svy, use_vote)

  blup <- inner_join(rename(re_st(m_t), u_trust = u),
                     rename(re_st(m_r), u_repub = u), by = "STATE") |>
    left_join(fips_tbl, by = "STATE")
  ct       <- cor.test(blup$u_trust, blup$u_repub)
  sd_sep   <- c(sqrt(VarCorr(m_t)$STATE[1, 1]), sqrt(VarCorr(m_r)$STATE[1, 1]))
  beta_sep <- unname(ct$estimate) * sd_sep[1] / sd_sep[2]
  S        <- sigma_from(m_j)

  cat("\n--- Rho ---\n")
  cat(sprintf("Separate-model BLUP r = %.3f, 95%% CI [%.3f, %.3f] (Fisher z; treats BLUPs as data)\n",
              ct$estimate, ct$conf.int[1], ct$conf.int[2]))
  cat(sprintf("  Spearman = %.3f | excluding DC r = %.3f\n",
              cor(blup$u_trust, blup$u_repub, method = "spearman"),
              with(filter(blup, STATE != "11"), cor(u_trust, u_repub))))
  cat(sprintf("Joint-model rho = %.3f (singular: %s)\n", S$rho, S$singular))
  cat(sprintf("State RE SDs (joint): trust %.3f | calib var %.3f\n", S$sd_t, S$sd_r))
  cat(sprintf("beta = rho * sd_t / sd_r: joint %.3f | separate %.3f\n", S$beta, beta_sep))

  st <- calibrate(predict_cells_joint(m_j, frame), S$beta, tg) |>
    left_join(existing, by = "STATE") |>
    left_join(fips_tbl, by = "STATE")
  st_sep <- calibrate(predict_cells_sep(m_t, m_r, frame), beta_sep, tg)
  stopifnot(identical(st_sep$STATE, st$STATE))

  cat("\n--- Calibration shifts ---\n")
  with(st, cat(sprintf("Logit shift for %s: mean %.3f, SD %.3f, range [%.3f, %.3f]\n",
                       calib_label, mean(delta_r), sd(delta_r), min(delta_r), max(delta_r))))
  # Model share is poststratified over ALL adults; target is among voters.
  # Part of any remaining gap is turnout composition, not survey error.
  cat(sprintf("National: model (adult-weighted) %.3f | actual Trump two-party share %.3f\n",
              with(st, weighted.mean(p_repub_unc, pop)), nat_target))
  cat(sprintf("Cross-state r(logit model, logit target) = %.3f | r(shift, target) = %.3f\n",
              with(st, cor(qlogis(p_repub_unc), qlogis(target))),
              with(st, cor(delta_r, target))))
  cat(sprintf("Max |cell-level minus aggregate shortcut| = %.2f pp\n",
              100 * max(abs(st$p_trust_cal - st$p_trust_cal_agg))))
  cat(sprintf("Sanity: r(uncalibrated joint, uncalibrated separate) = %.4f\n",
              cor(st$p_trust_unc, st_sep$p_trust_unc)))

  versions <- list(
    "Uncalibrated"                       = st$p_trust_unc,
    "Calibrated, joint Sigma"            = st$p_trust_cal,
    "Calibrated, joint Sigma, centered"  = st$p_trust_cal_c,
    "Calibrated, separate-model rho"     = st_sep$p_trust_cal,
    "Calibrated, aggregate shortcut"     = st$p_trust_cal_agg)

  cat("\n--- Agreement with existing pid3 + trump_z estimates (not ground truth) ---\n")
  print(round(do.call(rbind, lapply(versions, cmp, y = st$p_existing)), 3))

  cat("\nr with 2024 Trump share (calibrated values are mechanically tied to it):\n")
  print(round(sapply(c(versions[1:3], list(Existing = st$p_existing)),
                     \(x) cor(x, st$target)), 3))

  sub <- if (RUN_SUBSAMPLE) run_subsample(use_vote, S, st) else NULL

  list(spec = spec, blup = blup, ct = ct, S = S, beta_sep = beta_sep,
       st = st, st_sep = st_sep, sub = sub)
}

res <- lapply(SPECS, run_spec) |> setNames(SPECS)

# ---- 8. Cross-spec summary -------------------------------------------
summ <- bind_rows(lapply(res, \(r) tibble(
  spec            = r$spec,
  rho_blup        = unname(r$ct$estimate),
  rho_blup_lo     = r$ct$conf.int[1],
  rho_blup_hi     = r$ct$conf.int[2],
  rho_joint       = r$S$rho,
  sd_trust        = r$S$sd_t,
  sd_calib        = r$S$sd_r,
  beta_joint      = r$S$beta,
  mean_delta      = mean(r$st$delta_r),
  sd_delta        = sd(r$st$delta_r),
  r_unc_existing  = cor(r$st$p_trust_unc, r$st$p_existing),
  r_cal_existing  = cor(r$st$p_trust_cal, r$st$p_existing),
  rmsd_unc_pp     = rmse(r$st$p_trust_unc, r$st$p_existing),
  rmsd_cal_pp     = rmse(r$st$p_trust_cal, r$st$p_existing),
  mean_shift_pp   = 100 * mean(r$st$p_trust_cal - r$st$p_trust_unc))))

cat("\n\n================ CROSS-SPEC SUMMARY (", calib_label, ") ================\n")
print(mutate(summ, across(where(is.numeric), \(x) round(x, 3))), width = Inf)

if (all(c("none", "state_vote") %in% SPECS)) {
  cat(sprintf("\nr(calibrated 'none', calibrated 'state_vote') = %.3f\n",
              cor(res$none$st$p_trust_cal, res$state_vote$st$p_trust_cal)))
}

if (RUN_SUBSAMPLE) {
  cat("\n================ SPARSE-REGIME SUMMARY, ALL SPECS ================\n")
  bind_rows(lapply(res, \(r) mutate(r$sub$summary, spec = r$spec, .before = 1))) |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    print(width = Inf)
}

# ---- 9. Plots ---------------------------------------------------------
spec_lab <- setNames(
  sprintf("%s: BLUP r = %.2f, joint rho = %.2f, beta = %.2f",
          summ$spec, summ$rho_blup, summ$rho_joint, summ$beta_joint),
  summ$spec)

blup_all <- bind_rows(lapply(res, \(r) mutate(r$blup, spec = r$spec)))
st_all   <- bind_rows(lapply(res, \(r) mutate(r$st, spec = r$spec)))

p1 <- ggplot(blup_all, aes(u_repub, u_trust, label = abb)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.4, colour = "grey50") +
  geom_text(size = 2.4) +
  facet_wrap(~ spec, scales = "free", labeller = as_labeller(spec_lab), ncol = 1) +
  labs(x = paste("State random effect,", calib_label, "(logit)"),
       y = "State random effect, trust (logit)",
       title = "State random effects: trust vs. calibration variable") +
  theme_minimal(base_size = 10)

p2 <- ggplot(st_all, aes(delta_r, 100 * (p_trust_cal - p_trust_unc), label = abb)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_vline(xintercept = 0, linewidth = 0.2) +
  geom_text(size = 2.4) +
  facet_wrap(~ spec, scales = "free", labeller = as_labeller(spec_lab), ncol = 1) +
  labs(x = paste("Logit shift needed for", calib_label),
       y = "Calibrated minus uncalibrated trust (pp)",
       title = "Calibration adjustment by state") +
  theme_minimal(base_size = 10)

p3 <- st_all |>
  select(spec, abb, p_existing, Uncalibrated = p_trust_unc, Calibrated = p_trust_cal) |>
  pivot_longer(-c(spec, abb, p_existing), names_to = "version", values_to = "est") |>
  mutate(version = factor(version, c("Uncalibrated", "Calibrated"))) |>
  ggplot(aes(p_existing, est, label = abb)) +
  geom_abline(linetype = 2, linewidth = 0.3) +
  geom_text(size = 2) +
  facet_grid(spec ~ version) +
  coord_equal() +
  labs(x = "Existing estimate (pid3 + trump_z)", y = "Estimate",
       title = "Estimates vs. existing pid3 + trump_z estimates (agreement, not accuracy)") +
  theme_minimal(base_size = 10)


print(p1); print(p2); print(p3)
tag <- paste0("_", CALIB, if (POST_ELECTION_ONLY) "_postonly" else "")
ggsave(file.path(out_dir, paste0("p1_state_re", tag, ".png")),    p1, width = 6.5, height = 8, dpi = 300)
ggsave(file.path(out_dir, paste0("p2_adjustment", tag, ".png")),  p2, width = 6.5, height = 8, dpi = 300)
ggsave(file.path(out_dir, paste0("p3_vs_existing", tag, ".png")), p3, width = 7,   height = 6.5, dpi = 300)

saveRDS(list(calib = calib_label, summary = summ,
             results = lapply(res, \(r) r[c("spec", "blup", "S", "beta_sep", "st", "st_sep", "sub")])),
        file.path(out_dir, paste0("calibration_test_results", tag, ".rds")))