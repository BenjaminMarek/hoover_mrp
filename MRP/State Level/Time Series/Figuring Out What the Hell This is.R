# Figuring Out What the Hell This is
# Election results from MEDSL

library(here)
library(tidyverse)
library(ipumsr)
library(readxl)
library(tidycensus)
library(labelled)
library(synthjoint)
library(lme4)
library(usmap)
library(ggplot2)
library(survey)

load(here("Data", "Gallup confidence surveys (1973-2025)", "gallup_confidence_in_institutions_1973_2025.rdata"))

gallup_raw_df <- gallup_confidence_in_institutions_1973_2025

gallup_clean_df <- gallup_raw_df |> 
  mutate(
    year_mrp = as.integer(yr), 
    period_mrp = case_when(
      year_mrp <= 1982 ~ "1973-1982",
      year_mrp <= 1991 ~ "1983-1991",
      year_mrp <= 2001 ~ "1992-2001",
      year_mrp <= 2009 ~ "2002-2009",
      year_mrp <= 2019 ~ "2010-2019",
      year_mrp >= 2020 ~ "2020-2025"
    ),
    age_mrp = case_when(
      ager == 1 ~ "18-29",
      ager == 2 ~ "30-49",
      ager == 3 ~ "50-64",
      ager == 4 ~ "65+",
      TRUE ~ NA_character_
    ),
    pid3_mrp = case_when(
      partyrr == 1 ~ "Republican",
      partyrr == 2 ~ "Independent",
      partyrr == 3 ~ "Democrat",
      TRUE ~ NA_character_
    ),
    educ_mrp = case_when(
      educr %in% c(1,2) ~ "HS or less",
      educr == 3 ~ "Some college",
      educr == 4 ~ "College grad",
      TRUE ~ NA_character_
    ),
    race_mrp = case_when(
      !is.na(raceban2) & raceban2 == 1              ~ "White",
      !is.na(raceban2) & raceban2 == 2              ~ "Black",
      !is.na(raceban2) & raceban2 %in% c(3, 4, 7)  ~ "Other",
      !is.na(race7393) & race7393 == 1              ~ "White",
      !is.na(race7393) & race7393 == 2              ~ "Black",
      !is.na(race7393) & race7393 %in% c(4, 7)      ~ "Other",
      TRUE ~ NA_character_
    ),
    sex_mrp = case_when(
      gr == 1 ~ "Man",
      gr == 2 ~ "Woman",
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
  filter(complete.cases(year_mrp, period_mrp, STATE, educ_mrp, pid3_mrp, # filtering by pid here is not necessary for baseline model, but removes only 210 obs
    race_mrp, sex_mrp, trust_binary, age_mrp))



# Election Results
results_24 <- here("Data", "Election Results", "2024presgeresults.xlsx") |> 
  read_excel()
results_24 <- results_24 |>
  filter(complete.cases(STATE)) |>
  mutate(
    rep_share = TRUMP / (TRUMP + HARRIS),
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
      "VT" ~ "50", "VA" ~ "51", "WA" ~ "53", "WV" ~ "54", "WI" ~ "55",
      "WY" ~ "56"
    ),
    year = 2024L
  ) |>
  select(year, STATE, rep_share)

medsl_results <- here("Data", "Election Results", "1976-2020-president.csv") |> 
  read_csv()
state_repvote <- medsl_results |>
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) |>
  summarise(
    rep_votes = sum(candidatevotes[party_simplified == "REPUBLICAN"]),
    dem_votes = sum(candidatevotes[party_simplified == "DEMOCRAT"]),
    .by = c(year, state_fips)) |>
  mutate(
    rep_share = rep_votes / (rep_votes + dem_votes),
    STATE = sprintf("%02d", state_fips)) |>
  select(year, STATE, rep_share) |>
  bind_rows(results_24) |>
  mutate(
    period_mrp = case_when(
      year <= 1982 ~ "1973-1982",
      year <= 1991 ~ "1983-1991",
      year <= 2001 ~ "1992-2001",
      year <= 2009 ~ "2002-2009",
      year <= 2019 ~ "2010-2019",
      year >= 2020 ~ "2020-2025"
    )) |>
  summarise(
    state_repvote_raw = mean(rep_share),
    .by = c(period_mrp, STATE)) |>
  mutate(
    state_repvote = (state_repvote_raw - mean(state_repvote_raw)) / sd(state_repvote_raw),
    .by = period_mrp) |> 
  filter(complete.cases(STATE))




# PUMS

ddi_1 <- read_ipums_ddi(here("Data", "IPUMS Extracts", "1980-2000 Census", "usa_00001.xml"))
census_80_00 <- read_ipums_micro(ddi_1)

ddi_2 <- read_ipums_ddi(here("Data", "IPUMS Extracts", "ACS5", "usa_00002.xml"))
acs_09_24 <- read_ipums_micro(ddi_2)

# Building Poststrat Frames
build_frame <- function(df, use_hispan = TRUE) {
  df |>
    filter(AGE >= 18, GQ %in% c(1, 2, 5)) |>
    mutate(
      age_mrp = case_when(
        AGE < 30  ~ "18-29",
        AGE < 50  ~ "30-49",
        AGE < 65  ~ "50-64",
        AGE >= 65 ~ "65+"),
      educ_mrp = case_when(
        EDUC %in% 7:9  ~ "Some college",
        EDUC %in% 10:11 ~ "College grad",
        TRUE            ~ "HS or less"),
      race_mrp = case_when(
        use_hispan & HISPAN > 0 ~ "Other",
        RACE == 1               ~ "White",
        RACE == 2               ~ "Black",
        TRUE                    ~ "Other"),
      sex_mrp = case_when(
        SEX == 1 ~ "Man",
        SEX == 2 ~ "Woman"),
      STATE = sprintf("%02d", STATEFIP)) |>
    summarise(
      count = sum(PERWT), .by = c(STATE, age_mrp, educ_mrp, race_mrp, sex_mrp))
}

frame_1980 <- census_80_00 |> filter(YEAR == 1980) |> build_frame(use_hispan = FALSE)
frame_1990 <- census_80_00 |> filter(YEAR == 1990) |> build_frame(use_hispan = FALSE)
frame_2000 <- census_80_00 |> filter(YEAR == 2000) |> build_frame(use_hispan = TRUE)
frame_2009 <- acs_09_24 |> filter(YEAR == 2009) |> build_frame(use_hispan = TRUE)
frame_2018 <- acs_09_24 |> filter(YEAR == 2018) |> build_frame(use_hispan = TRUE)
frame_2024 <- acs_09_24 |> filter(YEAR == 2024) |> build_frame(use_hispan = TRUE)

frames <- list(
  "1973-1982" = frame_1980,
  "1983-1991" = frame_1990,
  "1992-2001" = frame_2000,
  "2002-2009" = frame_2009,
  "2010-2019" = frame_2018,
  "2020-2025" = frame_2024
)

# MRP
# Fit MRP without state_repvote

mrp_base_estimates <- imap(frames, \(frame, period) {
  mod <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                 (1 | STATE),
               data = filter(gallup_clean_df, period_mrp == period),
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))
  frame |>
    mutate(pred_trust = predict(mod, newdata = frame, type = "response", allow.new.levels = TRUE)) |>
    summarise(
      mrp_estimate = weighted.mean(pred_trust, count),
      .by = STATE
    ) |>
    mutate(period_mrp = period)
}) |>
  list_rbind()

# Fit MRP with state_repvote
gallup_with_rv <- gallup_clean_df |>
  left_join(state_repvote, by = c("period_mrp", "STATE"))

mrp_rv_estimates <- imap(frames, \(frame, period) {
  frame_p <- frame |>
    mutate(period_mrp = period) |>
    left_join(state_repvote, by = c("period_mrp", "STATE"))

  mod <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                 (1 | STATE) + state_repvote,
               data = filter(gallup_with_rv, period_mrp == period),
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))
  frame_p |>
    mutate(pred_trust = predict(mod, newdata = frame_p, type = "response", allow.new.levels = TRUE)) |>
    summarise(
      mrp_estimate = weighted.mean(pred_trust, count),
      .by = STATE
    ) |>
    mutate(period_mrp = period)
}) |>
  list_rbind()


# Variance Decomposition
variance_results <- imap(frames, \(frame, period) {
  df <- filter(gallup_clean_df, period_mrp == period, !is.na(partyrr))
  
  mod_nopid <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                       (1 | STATE),
                     data = df, family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))
  
  mod_pid <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp + pid3_mrp +
                     (1 | STATE),
                   data = df, family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)))
  
  tibble(
    period_mrp = period,
    n = nrow(df),
    var_nopid = VarCorr(mod_nopid)$STATE[1, 1],
    var_pid = VarCorr(mod_pid)$STATE[1, 1],
    pct_reduction = round(100 * (var_nopid - var_pid) / var_nopid, 1)
  )
}) |>
  list_rbind()





# MRP Maps

# Base model maps
plot_usmap(data = mrp_base_estimates |> rename(fips = STATE),
           values = "mrp_estimate", regions = "states") +
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
    midpoint = 0.4, limits = c(0.2, 0.7),
    name = "Trust") +
  facet_wrap(~period_mrp, nrow = 2) +
  labs(title = "MRP Estimates of Trust in Public Schools (Base Model)") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12))

# Repvote model maps
plot_usmap(data = mrp_rv_estimates |> rename(fips = STATE),
           values = "mrp_estimate", regions = "states") +
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
    midpoint = 0.4, limits = c(0.2, 0.7),
    name = "Trust") +
  facet_wrap(~period_mrp, nrow = 2) +
  labs(title = "MRP Estimates of Trust in Public Schools (With State Vote Shares)") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12))

mrp_rv_estimates |>
  filter(period_mrp %in% c("1973-1982", "2020-2025")) |>
  group_by(period_mrp) |>
  mutate(mrp_rank = rank(mrp_estimate) / n()) |>
  rename(fips = STATE) |>
  plot_usmap(data = _, values = "mrp_rank", regions = "states") +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                       midpoint = 0.5, name = "Rank\n(within period)") +
  facet_wrap(~period_mrp)

# Correlation between state trust rank and state_repvote by period w/ bootstrap 
set.seed(42)
n_boot <- 2000

period_cor <- mrp_base_estimates |>
  left_join(state_repvote, by = c("period_mrp", "STATE")) |>
  filter(!is.na(state_repvote), !is.na(mrp_estimate)) |>
  summarise(
    {
      merged <- pick(mrp_estimate, state_repvote)
      r_obs <- cor(merged$mrp_estimate, merged$state_repvote)
      boot_cors <- replicate(n_boot, {
        idx <- sample(nrow(merged), replace = TRUE)
        cor(merged$mrp_estimate[idx], merged$state_repvote[idx])
      })
      tibble(
        cor_est = r_obs,
        cor_lo = quantile(boot_cors, 0.025),
        cor_hi = quantile(boot_cors, 0.975)
      )
    },
    .by = period_mrp
  ) |>
  mutate(midpoint = recode(period_mrp,
    "1973-1982" = 1977.5, "1983-1991" = 1987, "1992-2001" = 1996.5,
    "2002-2009" = 2005.5, "2010-2019" = 2014.5, "2020-2025" = 2022.5)) |>
  arrange(midpoint)

period_cor

# Year by Year Trust by Pid
gallup_clean_df |>
  filter(period_mrp %in% c("1992-2001", "2002-2009", "2010-2019")) |>
  summarise(
    trust_rep = weighted.mean(trust_binary[pid3_mrp == "Republican"], 
                           wtfctr[pid3_mrp == "Republican"], na.rm = TRUE),
    trust_dem = weighted.mean(trust_binary[pid3_mrp == "Democrat"], 
                               wtfctr[pid3_mrp == "Democrat"], na.rm = TRUE),
    gap = trust_rep - trust_dem,
    n = n(),
    .by = year_mrp
  ) |>
  arrange(year_mrp)


# KBO Decomposition
# Comparing 2006-2009 (early) to 2020+ (late)
# Build joint PID shares from CCES: P(PID | age, educ, race, sex, state)
# Three factors: coefficients (model), demographics (frame), PID composition

# CCES data
cces_cum_df <- here("Data", "CCES Surveys", "cumulative_2006-2024.rds") |>
  read_rds()

state_fips <- tibble(
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
            "Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
            "New Jersey","New Mexico","New York","North Carolina","North Dakota",
            "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
            "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
            "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
  STATE = c("01","02","04","05","06","08","09","10","11","12","13","15","16",
            "17","18","19","20","21","22","23","24","25","26","27","28","29",
            "30","31","32","33","34","35","36","37","38","39","40","41","42",
            "44","45","46","47","48","49","50","51","53","54","55","56")
)

cces_clean <- cces_cum_df |>
  left_join(state_fips, by = "state") |>
  mutate(
    pid3_mrp = case_when(
      pid3 == 1 ~ "Democrat",
      pid3 == 2 ~ "Republican",
      pid3 == 3 ~ "Independent",
      TRUE ~ NA_character_
    ),
    kbo_period = case_when(
      year %in% 2006:2009 ~ "early",
      year >= 2020         ~ "late",
      TRUE ~ NA_character_
    ),
    age_mrp = case_when(
      age < 30  ~ "18-29",
      age < 50  ~ "30-49",
      age < 65  ~ "50-64",
      age >= 65 ~ "65+"),
    educ_mrp = case_when(
      educ <= 2 ~ "HS or less",
      educ == 3 ~ "Some college",
      educ >= 4 ~ "College grad"),
    race_mrp = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      TRUE      ~ "Other"),
    sex_mrp = case_when(
      gender == 1 ~ "Man",
      gender == 2 ~ "Woman")
  ) |>
  filter(!is.na(pid3_mrp), !is.na(STATE), !is.na(kbo_period),
         !is.na(age_mrp), !is.na(educ_mrp), !is.na(race_mrp), !is.na(sex_mrp))

# National P(PID | age, educ, race, sex) by period
cces_joint_pid <- cces_clean |>
  summarise(
    n = sum(weight_cumulative),
    .by = c(kbo_period, age_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp)
  ) |>
  mutate(
    pid_share = n / sum(n),
    .by = c(kbo_period, age_mrp, educ_mrp, race_mrp, sex_mrp)
  ) |>
  select(-n)

# Verify: 72 complete cells per period
cces_joint_pid |>
  summarise(total = sum(pid_share),
            .by = c(kbo_period, age_mrp, educ_mrp, race_mrp, sex_mrp)) |>
  count(kbo_period)

# Cross demographic frame with joint PID shares
build_pid_frame_joint <- function(demo_frame, pid_shares) {
  demo_frame |>
    left_join(pid_shares,
              by = c("age_mrp", "educ_mrp", "race_mrp", "sex_mrp")) |>
    mutate(count = count * pid_share) |>
    select(-pid_share) |>
    filter(!is.na(pid3_mrp))
}

pid_e <- cces_joint_pid |> filter(kbo_period == "early") |> select(-kbo_period)
pid_l <- cces_joint_pid |> filter(kbo_period == "late")  |> select(-kbo_period)

# Eight counterfactual frames: {demo_early, demo_late} × {pid_early, pid_late}
f_dee_pe <- build_pid_frame_joint(frame_2009, pid_e)
f_dee_pl <- build_pid_frame_joint(frame_2009, pid_l)
f_del_pe <- build_pid_frame_joint(frame_2024, pid_e)
f_del_pl <- build_pid_frame_joint(frame_2024, pid_l)

# Fit models
mod_early <- glmer(
  trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp + pid3_mrp + (1 | STATE),
  data = filter(gallup_clean_df, period_mrp == "2002-2009"),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

mod_late <- glmer(
  trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp + pid3_mrp + (1 | STATE),
  data = filter(gallup_clean_df, period_mrp == "2020-2025"),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Predict all 8 cells
predict_state <- function(mod, frame) {
  frame |>
    mutate(pred = predict(mod, newdata = frame, type = "response",
                          allow.new.levels = TRUE)) |>
    summarise(trust = weighted.mean(pred, count), .by = STATE)
}

cf <- list(
  eee = predict_state(mod_early, f_dee_pe),  # early model, early demo, early PID
  eed = predict_state(mod_early, f_dee_pl),  # early model, early demo, late PID
  ede = predict_state(mod_early, f_del_pe),  # early model, late demo, early PID
  edd = predict_state(mod_early, f_del_pl),  # early model, late demo, late PID
  dee = predict_state(mod_late,  f_dee_pe),  # late model, early demo, early PID
  ded = predict_state(mod_late,  f_dee_pl),  # late model, early demo, late PID
  dde = predict_state(mod_late,  f_del_pe),  # late model, late demo, early PID
  ddd = predict_state(mod_late,  f_del_pl)   # late model, late demo, late PID
) |>
  imap(\(d, name) rename(d, !!paste0("trust_", name) := trust)) |>
  reduce(left_join, by = "STATE")

# ANOVA-style decomposition: uniform average of marginal effects
kbo_full <- cf |>
  mutate(
    total_change = trust_ddd - trust_eee,
    coef_effect = ((trust_dee - trust_eee) + (trust_ded - trust_eed) +
                   (trust_dde - trust_ede) + (trust_ddd - trust_edd)) / 4,
    demo_effect = ((trust_ede - trust_eee) + (trust_edd - trust_eed) +
                   (trust_dde - trust_dee) + (trust_ddd - trust_ded)) / 4,
    pid_effect  = ((trust_eed - trust_eee) + (trust_edd - trust_ede) +
                   (trust_ded - trust_dee) + (trust_ddd - trust_dde)) / 4,
    residual = total_change - coef_effect - demo_effect - pid_effect
  )

# Population-weighted national summary
state_pops <- frame_2024 |> summarise(state_pop = sum(count), .by = STATE)

kbo_full |>
  left_join(state_pops, by = "STATE") |>
  summarise(across(total_change:residual, \(x) weighted.mean(x, state_pop))) |>
  mutate(
    pct_coef  = round(100 * coef_effect / total_change, 1),
    pct_demo  = round(100 * demo_effect / total_change, 1),
    pct_pid   = round(100 * pid_effect / total_change, 1),
    pct_resid = round(100 * residual / total_change, 1)
  )


# Within-cell PID-trust analysis
# Compare trust by PID within demographic cells across periods
# If trust drops among Republicans within the same demographic cell,
# it's an attitude shift. If it doesn't, the aggregate pattern
# reflects compositional change in who identifies as Republican.

cell_pid_trust <- gallup_clean_df |>
  filter(
    pid3_mrp %in% c("Republican", "Democrat"),
    period_mrp %in% c("2002-2009", "2020-2025")
  ) |>
  summarise(
    trust = weighted.mean(trust_binary, wtfctr, na.rm = TRUE),
    n = n(),
    .by = c(period_mrp, pid3_mrp, age_mrp, educ_mrp, race_mrp, sex_mrp)
  )

# Within-cell change in partisan gap
cell_gaps <- cell_pid_trust |>
  pivot_wider(
    names_from = pid3_mrp,
    values_from = c(trust, n),
    names_sep = "_"
  ) |>
  mutate(gap = trust_Republican - trust_Democrat) |>
  pivot_wider(
    names_from = period_mrp,
    values_from = c(trust_Republican, trust_Democrat, gap, n_Republican, n_Democrat),
    names_sep = "."
  ) |>
  mutate(
    gap_change = `gap.2020-2025` - `gap.2002-2009`,
    rep_change = `trust_Republican.2020-2025` - `trust_Republican.2002-2009`,
    dem_change = `trust_Democrat.2020-2025` - `trust_Democrat.2002-2009`
  )

# Summary: average within-cell change
cell_gaps |>
  filter(!is.na(gap_change)) |>
  summarise(
    mean_gap_change = mean(gap_change),
    mean_rep_change = mean(rep_change),
    mean_dem_change = mean(dem_change),
    n_cells = n()
  )

# Weighted by cell size
cell_gaps |>
  filter(!is.na(gap_change)) |>
  mutate(cell_n = pmin(`n_Republican.2002-2009`, `n_Republican.2020-2025`, na.rm = TRUE)) |>
  summarise(
    wmean_gap_change = weighted.mean(gap_change, cell_n, na.rm = TRUE),
    wmean_rep_change = weighted.mean(rep_change, cell_n, na.rm = TRUE),
    wmean_dem_change = weighted.mean(dem_change, cell_n, na.rm = TRUE),
    n_cells = n()
  )

# Break out by education to see if it's concentrated
cell_gaps |>
  filter(!is.na(gap_change)) |>
  summarise(
    mean_rep_change = mean(rep_change),
    mean_dem_change = mean(dem_change),
    mean_gap_change = mean(gap_change),
    n_cells = n(),
    .by = educ_mrp
  )

# And by race
cell_gaps |>
  filter(!is.na(gap_change)) |>
  summarise(
    mean_rep_change = mean(rep_change),
    mean_dem_change = mean(dem_change),
    mean_gap_change = mean(gap_change),
    n_cells = n(),
    .by = race_mrp
  )



# Revised Pacheco

# Assign nearest frame to each window midpoint
assign_frame <- function(midpoint) {
  frame_years <- c(1980, 1990, 2000, 2009, 2018, 2024)
  frame_names <- c("1980", "1990", "2000", "2009", "2018", "2024")
  frame_names[which.min(abs(midpoint - frame_years))]
}

frame_lookup <- list(
  "1980" = frame_1980, "1990" = frame_1990, "2000" = frame_2000,
  "2009" = frame_2009, "2018" = frame_2018, "2024" = frame_2024
)

# Build rolling windows
window_width <- 10
start_years <- seq(1973, max(gallup_clean_df$year_mrp) - window_width + 1, by = 2)

rolling_mrp <- map(start_years, \(s) {
  end <- s + window_width - 1
  midpoint <- s + (window_width - 1) / 2
  window_label <- paste0(s, "-", end)

  df <- gallup_clean_df |>
    filter(year_mrp >= s, year_mrp <= end)

  if (nrow(df) < 500) return(NULL)

  frame <- frame_lookup[[assign_frame(midpoint)]]

  mod <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                 (1 | STATE),
               data = df, family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))

  frame |>
    mutate(pred_trust = predict(mod, newdata = frame, type = "response",
                                allow.new.levels = TRUE)) |>
    summarise(mrp_estimate = weighted.mean(pred_trust, count), .by = STATE) |>
    mutate(window = window_label, midpoint = midpoint)
}) |>
  compact() |>
  list_rbind()

# Rolling correlation with state_repvote
# Need rolling state_repvote too - use raw election-year values
# and average within each window

election_years <- medsl_results |>
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) |>
  summarise(
    rep_votes = sum(candidatevotes[party_simplified == "REPUBLICAN"]),
    dem_votes = sum(candidatevotes[party_simplified == "DEMOCRAT"]),
    .by = c(year, state_fips)
  ) |>
  mutate(
    rep_share = rep_votes / (rep_votes + dem_votes),
    STATE = sprintf("%02d", state_fips)
  ) |>
  select(year, STATE, rep_share) |>
  bind_rows(results_24)

# Rolling correlation with bootstrap CIs 
set.seed(42)
n_boot <- 2000

rolling_cor <- map(start_years, \(s) {
  end <- s + window_width - 1
  midpoint <- s + (window_width - 1) / 2

  rv <- election_years |>
    filter(year >= s - 2, year <= end + 2) |>
    summarise(state_repvote = mean(rep_share), .by = STATE)

  mrp <- rolling_mrp |> filter(window == paste0(s, "-", end))

  if (nrow(mrp) == 0) return(NULL)

  merged <- mrp |>
    inner_join(rv, by = "STATE") |>
    filter(!is.na(state_repvote), !is.na(mrp_estimate))

  r_obs <- cor(merged$mrp_estimate, merged$state_repvote)

  boot_cors <- replicate(n_boot, {
    idx <- sample(nrow(merged), replace = TRUE)
    cor(merged$mrp_estimate[idx], merged$state_repvote[idx])
  })

  tibble(
    midpoint = midpoint,
    cor_est = r_obs,
    cor_lo = quantile(boot_cors, 0.025),
    cor_hi = quantile(boot_cors, 0.975)
  )
}) |>
  compact() |>
  list_rbind()

# Plot
ggplot(rolling_cor, aes(midpoint, cor_est)) +
  geom_ribbon(aes(ymin = cor_lo, ymax = cor_hi), alpha = 0.2) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Window midpoint",
    y = "r (MRP trust vs. Republican vote share)",
    title = "Rolling Correlation: State Trust and Partisanship",
    caption = "Shaded: 95% bootstrap CI (states resampled, 2,000 iterations)"
  ) +
  theme_minimal()

# Combine both estimate sets
mrp_combined <- bind_rows(
  mrp_base_estimates |> mutate(model = "Base"),
  mrp_rv_estimates |> mutate(model = "With state partisanship")
)

# State name lookup for readable labels
state_names <- tibble(
  STATE = c("01","02","04","05","06","08","09","10","11","12","13","15","16",
            "17","18","19","20","21","22","23","24","25","26","27","28","29",
            "30","31","32","33","34","35","36","37","38","39","40","41","42",
            "44","45","46","47","48","49","50","51","53","54","55","56"),
  state_name = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
                 "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
                 "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
                 "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
                 "UT","VT","VA","WA","WV","WI","WY")
)



# Line Plots By state over time
# Small multiples
bind_rows(
  mrp_base_estimates |> mutate(model = "Base"),
  mrp_rv_estimates |> mutate(model = "With state partisanship")
) |>
  left_join(fips_codes |> 
              transmute(STATE = state_code, state_name = state) |> 
              distinct(),
            by = "STATE") |>
  mutate(midpoint = recode(period_mrp,
    "1973-1982" = 1977.5, "1983-1991" = 1987, "1992-2001" = 1996.5,
    "2002-2009" = 2005.5, "2010-2019" = 2014.5, "2020-2025" = 2022.5)) |>
  ggplot(aes(midpoint, mrp_estimate, color = model)) +
  geom_line(linewidth = 0.4) +
  geom_point(size = 0.5) +
  facet_wrap(~state_name, ncol = 9) +
  scale_y_continuous(limits = c(0.15, 0.75), breaks = c(0.2, 0.4, 0.6)) +
  labs(x = NULL, y = "MRP Trust Estimate", color = NULL,
       title = "State-Level Trust in Public Schools Over Time") +
  theme_minimal() +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 5),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

# Highlighted states spaghetti plot with national line
mrp_rv_estimates |>
  left_join(fips_codes |>
              transmute(STATE = state_code, state_name = state) |>
              distinct(),
            by = "STATE") |>
  mutate(
    midpoint = recode(period_mrp,
      "1973-1982" = 1977.5, "1983-1991" = 1987, "1992-2001" = 1996.5,
      "2002-2009" = 2005.5, "2010-2019" = 2014.5, "2020-2025" = 2022.5),
    highlight = state_name %in% c("Massachusetts", "Texas", "Iowa",
                                   "Mississippi", "California", "West Virginia")
  ) |>
  ggplot(aes(midpoint, mrp_estimate, group = state_name)) +
  geom_line(data = \(d) filter(d, !highlight), color = "grey80", linewidth = 0.3) +
  geom_line(data = \(d) filter(d, highlight), aes(color = state_name), linewidth = 0.8) +
  geom_point(data = \(d) filter(d, highlight), aes(color = state_name), size = 1.5) +
  geom_line(
    data = \(d) summarise(d, mrp_estimate = mean(mrp_estimate), .by = midpoint),
    aes(group = 1), color = "black", linewidth = 1, linetype = "dashed"
  ) +
  labs(x = NULL, y = "MRP Trust Estimate", color = NULL,
       title = "State-Level Trust in Public Schools Over Time") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Robustness Check w/ Hispanics as white

# Robustness: Hispanic coded as White (matching pre-1993 Gallup behavior)
gallup_robust_df <- gallup_clean_df |>
  mutate(race_mrp = case_when(
    !is.na(raceban2) & raceban2 %in% c(1, 3) ~ "White",
    !is.na(raceban2) & raceban2 == 2           ~ "Black",
    !is.na(raceban2) & raceban2 %in% c(4, 7)  ~ "Other",
    !is.na(race7393) & race7393 == 1           ~ "White",
    !is.na(race7393) & race7393 == 2           ~ "Black",
    !is.na(race7393) & race7393 %in% c(4, 7)  ~ "Other",
    TRUE ~ NA_character_
  ))

frames_robust <- list(
  "1973-1982" = frame_1980,
  "1983-1991" = frame_1990,
  "1992-2001" = census_80_00 |> filter(YEAR == 2000) |> build_frame(use_hispan = FALSE),
  "2002-2009" = acs_09_24 |> filter(YEAR == 2009) |> build_frame(use_hispan = FALSE),
  "2010-2019" = acs_09_24 |> filter(YEAR == 2018) |> build_frame(use_hispan = FALSE),
  "2020-2025" = acs_09_24 |> filter(YEAR == 2024) |> build_frame(use_hispan = FALSE)
)

mrp_robust_estimates <- imap(frames_robust, \(frame, period) {
  mod <- glmer(trust_binary ~ sex_mrp + age_mrp + educ_mrp + race_mrp +
                 (1 | STATE),
               data = filter(gallup_robust_df, period_mrp == period),
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))
  frame |>
    mutate(pred_trust = predict(mod, newdata = frame, type = "response",
                                allow.new.levels = TRUE)) |>
    summarise(mrp_estimate = weighted.mean(pred_trust, count), .by = STATE) |>
    mutate(period_mrp = period)
}) |>
  list_rbind()

mrp_base_estimates |>
  inner_join(mrp_robust_estimates, by = c("period_mrp", "STATE"),
             suffix = c("_main", "_robust")) |>
  summarise(
    cor = cor(mrp_estimate_main, mrp_estimate_robust),
    mean_diff = mean(mrp_estimate_main - mrp_estimate_robust),
    max_diff = max(abs(mrp_estimate_main - mrp_estimate_robust)),
    .by = period_mrp
  ) |>
  arrange(period_mrp)


# Within Cell Regression
within_cell_df <- gallup_clean_df |>
  filter(pid3_mrp %in% c("Republican", "Democrat")) |>
  mutate(
    wtfctr = as.numeric(wtfctr),
    is_republican = as.integer(pid3_mrp == "Republican"),
    period_mrp = factor(period_mrp, levels = c(
    "1973-1982", "1983-1991", "1992-2001",
    "2002-2009", "2010-2019", "2020-2025"
  )))

# Option A: survey-weighted (design-consistent SEs)
# ids = ~1 because Gallup doesn't publish stratum/PSU identifiers
svy_design <- svydesign(ids = ~1, weights = ~wtfctr, data = within_cell_df)

within_cell_weighted <- svyglm(
  trust_binary ~ is_republican * period_mrp +
    age_mrp + educ_mrp + race_mrp + sex_mrp,
  design = svy_design,
  family = quasibinomial  # svyglm requires quasibinomial, not binomial
)

summary(within_cell_weighted)

# Option B: unweighted (robustness check)
within_cell_unweighted <- glm(
  trust_binary ~ is_republican * period_mrp +
    age_mrp + educ_mrp + race_mrp + sex_mrp,
  data = within_cell_df,
  family = binomial
)

summary(within_cell_unweighted)


# Period MRP estimates (base model, no partisan covariate)
saveRDS(mrp_base_estimates, here("mrp_base_estimates.rds"))

# Period MRP estimates (with state_repvote)
saveRDS(mrp_rv_estimates, here("mrp_rv_estimates.rds"))

# Rolling MRP estimates
saveRDS(rolling_mrp, here("rolling_mrp.rds"))

# Rolling correlation with bootstrap CIs
saveRDS(rolling_cor, here("rolling_cor.rds"))

# Fixed-period correlation with bootstrap CIs
saveRDS(period_cor, here("period_cor.rds"))

# Within-cell regression (svyglm and unweighted)
saveRDS(within_cell_weighted, here("within_cell_weighted.rds"))
saveRDS(within_cell_unweighted, here("within_cell_unweighted.rds"))

# Variance decomposition
saveRDS(variance_results, here("variance_results.rds"))

saveRDS(gallup_clean_df, here("gallup_clean_df.rds"))








# Coefficients dropping across the board. Being Republican itself strongly associated with distrusting schools, even comparing people with same demos. 

# Compare (when benchmarked), to "What about your oldest child's school"
# What's the punchline, how do I think about this? How are Americans in the different states evaluating different states?
# We have gallup
# People are puzzled by whether same pattern holds for school-aged parents

# Back on big yougov survey. Did voters 
# The dream project here was to have a public facing dataset for year by year, so a person could pull data from say Alabama 1983 and see Oh that's what happened to trust after this reform bill
# Also talk to Professor Rivers

# Dynamic MRP.

