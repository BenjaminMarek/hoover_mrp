library(here)
library(tidyverse)
library(haven)

load(here("Data", "Gallup confidence surveys (1973-2025)", "gallup_confidence_in_institutions_1973_2025.rdata"))

gallup_raw_df <- gallup_confidence_in_institutions_1973_2025

gallup_lines_df <- gallup_raw_df |> 
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
      !is.na(raceban2) & raceban2 == 1              ~ "White",
      !is.na(raceban2) & raceban2 == 2              ~ "Black",
      !is.na(raceban2) & raceban2 == 3              ~ "Hispanic",
      !is.na(raceban2) & raceban2 %in% c(4, 7)     ~ "Other",
      !is.na(race7393) & race7393 == 1              ~ "White",
      !is.na(race7393) & race7393 == 2              ~ "Black",
      !is.na(race7393) & race7393 %in% c(4, 7)     ~ "Other",
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
    across(
      c(conf_church, conf_military, conf_sup_ct, conf_banks, conf_schools,
        conf_newspapers, conf_congress, conf_org_labor, conf_big_business),
      \(x) case_when(
      x %in% c(1, 2) ~ 1L,
      x %in% c(3, 4, 5) ~ 0L,
      TRUE ~ NA_integer_), 
      .names = "trust_{str_remove(.col, 'conf_')}")) |> 
  filter(complete.cases(year_mrp, educ_mrp, race_mrp, sex_mrp, pid3_mrp, age_mrp))




# Per Institution Graphs

plot_trust <- function(df, trust_col, title) {
  df |>
    mutate(wtfctr = zap_labels(wtfctr) |> as.numeric()) |>
    filter(!is.na(.data[[trust_col]])) |>
    reframe(
      trust = c(
        weighted.mean(.data[[trust_col]], wtfctr, na.rm = TRUE),
        weighted.mean(.data[[trust_col]][pid3_mrp == "Democrat"], wtfctr[pid3_mrp == "Democrat"], na.rm = TRUE),
        weighted.mean(.data[[trust_col]][pid3_mrp == "Republican"], wtfctr[pid3_mrp == "Republican"], na.rm = TRUE)
      ),
      group = c("Overall", "Democrat", "Republican"),
      .by = year_mrp
    ) |>
    mutate(group = factor(group, levels = c("Overall", "Democrat", "Republican"))) |>
    ggplot(aes(year_mrp, trust, color = group)) +
    geom_point(alpha = 0.3, size = 1.5) +
    geom_smooth(method = "loess", span = 0.4, se = FALSE, linewidth = 1) +
    scale_color_manual(values = c("Overall" = "black", "Democrat" = "blue", "Republican" = "red")) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Confidence", color = NULL, title = title) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

institution_labels <- c(
  trust_church = "Churches",
  trust_military = "Military",
  trust_sup_ct = "Supreme Court",
  trust_banks = "Banks",
  trust_schools = "Public Schools",
  trust_newspapers = "Newspapers",
  trust_congress = "Congress",
  trust_org_labor = "Organized Labor",
  trust_big_business = "Big Business"
)

plots <- imap(institution_labels, \(title, col) plot_trust(gallup_lines_df, col, title))