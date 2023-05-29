library(tidyverse)
library(here)
library(readr)

raw_data <- read_csv(here("Healthy Minds Study", "HMS 2021-2022 Data.csv"))[-1,]

clean_data <- raw_data %>%
  transmute(
    age = round(as.numeric(age)),
    gender = case_when(
      gender_male %in% "1" ~ "Male",
      gender_female %in% "1" ~ "Female",
      T ~ "Trans/Gender-Diverse"
    ),
    race_black = race_black %in% "1",
    race_american_indian = race_ainaan %in% "1",
    race_asian = race_asian %in% "1",
    race_hispanic_latinx = race_his %in% "1",
    race_pacific_islander = race_pi %in% "1",
    race_mideast = race_mides %in% "1",
    race_white = race_white %in% "1",
    race_other = race_other %in% "1",
    citizen = st_citizen %in% "1",
    financial_stress = 6 - as.numeric(fincur),
    weekly_hours_paid_work = as.numeric(hours_work_paid) %>%
      if_else(. < 0, 0, .),
    grades = case_when(
      gr_A %in% "1" ~ "Mostly A's",
      gr_B %in% "1" ~ "Mostly B's",
      gr_C %in% "1" ~ "Mostly C's",
      gr_D %in% "1" ~ "Mostly D's",
      gr_F %in% "1" ~ "Mostly F's",
      gr_none %in% "1" ~ "None of these",
      gr_dk %in% "1" ~ "No grade or don't know"
    ),
    belonging = 7 - as.numeric(belong1),
    depression = as.numeric(dep_maj),
    depression_severity = as.numeric(deprawsc),
    anxiety = as.numeric(anx_any),
    anxiety_severity = as.numeric(anx_score),
    past_year_therapy = as.numeric(ther_any),
    past_year_medication = as.numeric(meds_any)
  ) %>%
  filter(age <= 50)

saveRDS(clean_data, file = here("Healthy Minds Study", "Clean Data.rds"))
