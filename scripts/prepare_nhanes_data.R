# ==============================================================================
# Prepare CDC NHANES 2011-2012 (G) and 2013-2014 (H) Analysis Dataset
# Project: Sleep & Physical Strength in Older Adults (Aged >= 60)
# ==============================================================================

library(haven)
library(dplyr)
library(readr)

# 1. Load 2011-2012 (Cycle G) and 2013-2014 (Cycle H) SAS Transport (.xpt) files
demo_g <- read_xpt("data/raw/DEMO_G.xpt") |> mutate(cycle = "2011-2012")
demo_h <- read_xpt("data/raw/DEMO_H.xpt") |> mutate(cycle = "2013-2014")

slq_g  <- read_xpt("data/raw/SLQ_G.xpt")
slq_h  <- read_xpt("data/raw/SLQ_H.xpt")

mgx_g  <- read_xpt("data/raw/MGX_G.xpt")
mgx_h  <- read_xpt("data/raw/MGX_H.xpt")

bmx_g  <- read_xpt("data/raw/BMX_G.xpt")
bmx_h  <- read_xpt("data/raw/BMX_H.xpt")

paq_g  <- read_xpt("data/raw/PAQ_G.xpt")
paq_h  <- read_xpt("data/raw/PAQ_H.xpt")

# 2. Combine cycles and merge by unique participant ID (SEQN)
demo <- bind_rows(demo_g, demo_h) |>
  select(SEQN, cycle, RIDAGEYR, RIAGENDR, RIDRETH1, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA)

slq <- bind_rows(slq_g, slq_h) |>
  select(SEQN, SLD010H, SLQ050, SLQ060)

mgx <- bind_rows(mgx_g, mgx_h) |>
  select(
    SEQN, MGDEXSTS, MGDCGSZ,
    MGXH1T1, MGXH1T2, MGXH1T3,
    MGXH2T1, MGXH2T2, MGXH2T3
  )

bmx <- bind_rows(bmx_g, bmx_h) |>
  select(SEQN, BMXBMI, BMXWT, BMXHT)

paq <- bind_rows(paq_g, paq_h) |>
  select(SEQN, PAQ605, PAQ620, PAQ650, PAQ665, PAD680)

merged_all <- demo |>
  left_join(slq, by = "SEQN") |>
  left_join(mgx, by = "SEQN") |>
  left_join(bmx, by = "SEQN") |>
  left_join(paq, by = "SEQN")

# 3. Restrict to Older Adults Aged >= 60 and clean special NHANES codes
nhanes_60plus <- merged_all |>
  filter(RIDAGEYR >= 60) |>
  mutate(
    # Clean sleep duration: exclude 77 (Refused) and 99 (Don't Know)
    sleep_hours = if_else(SLD010H %in% c(77, 99), NA_real_, as.numeric(SLD010H)),
    
    # Sex and Age Group labels
    sex = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    age_group = case_when(
      RIDAGEYR >= 60 & RIDAGEYR <= 69 ~ "60-69 yrs",
      RIDAGEYR >= 70 & RIDAGEYR <= 79 ~ "70-79 yrs",
      RIDAGEYR >= 80 ~ "80+ yrs (Top-coded)",
      TRUE ~ NA_character_
    ),
    
    # Handgrip strength variables (kg)
    combined_grip_kg = as.numeric(MGDCGSZ),
    hand1_max_kg = pmax(MGXH1T1, MGXH1T2, MGXH1T3, na.rm = TRUE),
    hand2_max_kg = pmax(MGXH2T1, MGXH2T2, MGXH2T3, na.rm = TRUE),
    max_single_hand_kg = pmax(hand1_max_kg, hand2_max_kg, na.rm = TRUE),
    n_valid_trials = (!is.na(MGXH1T1)) + (!is.na(MGXH1T2)) + (!is.na(MGXH1T3)) +
                     (!is.na(MGXH2T1)) + (!is.na(MGXH2T2)) + (!is.na(MGXH2T3)),
    completed_all_6_trials = (n_valid_trials == 6L),
    
    # BMI and category
    bmi = as.numeric(BMXBMI),
    bmi_category = case_when(
      is.na(bmi) ~ "Missing",
      bmi < 18.5 ~ "Underweight (<18.5)",
      bmi < 25.0 ~ "Normal (18.5-24.9)",
      bmi < 30.0 ~ "Overweight (25.0-29.9)",
      bmi >= 30.0 ~ "Obese (>=30.0)"
    ),
    
    # Physical Activity (PAQ605: vigorous work, PAQ620: moderate work,
    # PAQ650: vigorous rec, PAQ665: moderate rec; 1 = Yes, 2 = No, 7/9 = missing)
    mod_vig_activity = case_when(
      PAQ605 == 1 | PAQ620 == 1 | PAQ650 == 1 | PAQ665 == 1 ~ "Active (Mod/Vig)",
      PAQ605 == 2 & PAQ620 == 2 & PAQ650 == 2 & PAQ665 == 2 ~ "Inactive",
      TRUE ~ "Unknown"
    ),
    sedentary_mins = if_else(PAD680 %in% c(7777, 9999), NA_real_, as.numeric(PAD680)),
    
    # 4-year combined MEC examination weight per CDC analytic guidelines
    WTMEC4YR = WTMEC2YR / 2.0,
    
    # Sample membership flags
    in_primary_sample = !is.na(sleep_hours) & !is.na(combined_grip_kg),
    in_complete_6trial_sample = !is.na(sleep_hours) & completed_all_6_trials
  )

# 4. Verify sample counts against audit
cat("Total NHANES 2011-2014 participants:", nrow(merged_all), "\n")
cat("Total eligible older adults (RIDAGEYR >= 60):", nrow(nhanes_60plus), "\n")
cat("Primary analytical sample (valid sleep + combined grip MGDCGSZ):", sum(nhanes_60plus$in_primary_sample), "\n")
cat("Secondary complete-case sample (valid sleep + all 6 grip trials):", sum(nhanes_60plus$in_complete_6trial_sample), "\n")

# 5. Save primary analytical dataset (n = 3,014) and full 60+ cohort (n = 3,632)
primary_df <- nhanes_60plus |> filter(in_primary_sample)
write_csv(primary_df, "data/nhanes_sleep_grip_primary_3014.csv")
write_csv(nhanes_60plus, "data/nhanes_sleep_grip_60plus_3632.csv")
cat("Saved data/nhanes_sleep_grip_primary_3014.csv (n =", nrow(primary_df), ")\n")
