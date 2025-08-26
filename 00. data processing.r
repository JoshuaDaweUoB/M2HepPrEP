# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, gtsummary, xfun)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load raw data
m2hepprep_raw <- read.csv("data/cleaned_m2_data_05012025 (1).csv")
m2hepprep_tx_raw <- read.csv("data/Initiation and adhrence.csv")

# sheet of baseline/screening rows
df_filtered <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline" | redcap_event_name == "Screening (visit 1)")
df_filtered_complete <- df_filtered[, colSums(is.na(df_filtered)) == 0]
write_xlsx(df_filtered_complete, "data/baseline_or_screening_complete.xlsx")

# informed consent
m2hepprep_consent <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  select(record_id, rc_informed)

# baseline vars
m2hepprep_baseline_vars <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  select(
    record_id, aiv_kid_evr_pa, aiv_adt_evr_pa, aiv_6m_pa, aiv_kid_evr_sex, aiv_adt_evr_sex, aiv_6m_sex,
    cla_2, cla_16a, cla_16c, nms_er, nms_hps_drg, nms_otp, nms_rsd, nms_auc, nms_opd, nms_mnt, nms_trp,
    srb_1m_m, dem_edu, nms_emp, nms_inc, nms_inc_cad, nms_inc_usd,
    nms_opd_med___0, nms_opd_med___1, nms_opd_med___2, nms_opd_med___3, nms_opd_med___4, nms_opd_med___5, nms_opd_med___6, nms_opd_med_ot,
    odu_6m, dem_hltins, sdu_srg, sub_frq1m,
    sub_6m1, sub_6m2, sub_6m3, sub_6m4, sub_6m5, sub_6m6, sub_6m7, sub_6m8, sub_6m9, sub_6m10, sub_6m11, sub_6m12, sub_6m13, sub_6m14, sub_6m15, sub_6m16, sub_6m17, sub_6m18, sub_6m19, sub_6m20, sub_6m21, sub_6m22, sub_6m23, sub_6m24, sub_6m25, sub_6m26, sub_6m27,
    sdu_wrk, sdu_wrk_6m_frq, idu_6mplc2___3, dem_gender_id, dem_gender,
    srb_3m, srb_3m_f, srb_1m_fc_pv, srb_1m_fc_pa, srb_1m_fc_npv, srb_1m_fc_npa, srb_1m_fc_cv, srb_1m_fc_ca, srb_1m_fc_mcv, srb_1m_fc_mco, srb_1m_fc_mca
  )

# baseline data
m2hepprep_baseline <- m2hepprep_raw %>%
  filter(redcap_event_name == "Screening (visit 1)") %>%
  select(record_id, id_paper, rand_arm, rand_date, is_eligible, sdem_visit, sdem_reside, sdem_lang_mia_2, sdem_age, sdem_oat, sdem_sev, sdem_sex, sdem_gender, sdem_prg_c, vcp_inject_6mo, sdem_hiv_etst, sdem_hiv_rtst_r, sdem_prp_cu, sdem_hcv, sdem_wil_fol, sdem_elig, sdem_hcv_etst, sdem_hcv_rtst, sdem_hcv_rtst_r, sdem_hiv_rtst, scr_c_hcv_res_retired, insti, sdem_slep6m, sdem_live6m_hls, sdem_live6m_shl1, sdem_live6m_trs, sdem_live6m_htl, sdem_live6m_hiv, sdem_live6m_sut, sdem_live6m_shl2, sdem_live6m_shl3, sdem_live6m_shl4, sdem_live6m_shl5, sdem_idu, sdem_idu6m___0, sdem_idu6m___1, sdem_idu6m___2, sdem_idu6m___3, sdem_idu6m___4, sdem_idu6m___5, sdem_idu6m___6, sdem_idu6m___7, sdem_dis_hcv, sdem_dis_hiv, sdem_dis_sex, sdem_dis_gay, sdem_dis_sub, sdem_dis_race, vir_dbs, vir_rna2)

# 3 month visit
m2hepprep_3m <- m2hepprep_raw %>%
  filter(redcap_event_name == "3 months") %>%
  select(record_id, adh2_visitdate, prep_prescribe, adh_prep, adh_noprep_reason___1, adh_noprep_reason___2, adh_noprep_reason___3, adh_noprep_reason___4, adh_noprep_reason___5, adh_noprep_reason___6, adh_noprep_reason___7, adh_noprep_reason___8, adh_noprep_reason___9, adh_noprep_reason___10, adh_noprep_reason___11, adh_noprep_reason___12, adh_noprep_reason___13, adh_noprep_reason___14, stop_prep, prep_last4week, prep_now)

# 6 month visit
m2hepprep_6m <- m2hepprep_raw %>%
  filter(redcap_event_name == "6 months") %>%
  select(record_id, adh2_visitdate, prep_prescribe, adh_prep, adh_noprep_reason___1, adh_noprep_reason___2, adh_noprep_reason___3, adh_noprep_reason___4, adh_noprep_reason___5, adh_noprep_reason___6, adh_noprep_reason___7, adh_noprep_reason___8, adh_noprep_reason___9, adh_noprep_reason___10, adh_noprep_reason___11, adh_noprep_reason___12, adh_noprep_reason___13, adh_noprep_reason___14, stop_prep, prep_last4week, prep_now)

# 9 month visit
m2hepprep_9m <- m2hepprep_raw %>%
  filter(redcap_event_name == "9 months") %>%
    select(record_id, adh2_visitdate, prep_prescribe, adh_prep, adh_noprep_reason___1, adh_noprep_reason___2, adh_noprep_reason___3, adh_noprep_reason___4, adh_noprep_reason___5, adh_noprep_reason___6, adh_noprep_reason___7, adh_noprep_reason___8, adh_noprep_reason___9, adh_noprep_reason___10, adh_noprep_reason___11, adh_noprep_reason___12, adh_noprep_reason___13, adh_noprep_reason___14, stop_prep, prep_last4week, prep_now)

# 12 month visit
m2hepprep_12m <- m2hepprep_raw %>%
  filter(redcap_event_name == "12 months") %>%
  select(record_id, adh2_visitdate, prep_prescribe, adh_prep, adh_noprep_reason___1, adh_noprep_reason___2, adh_noprep_reason___3, adh_noprep_reason___4, adh_noprep_reason___5, adh_noprep_reason___6, adh_noprep_reason___7, adh_noprep_reason___8, adh_noprep_reason___9, adh_noprep_reason___10, adh_noprep_reason___11, adh_noprep_reason___12, adh_noprep_reason___13, adh_noprep_reason___14, stop_prep, prep_last4week, prep_now)

# 15 month visit
m2hepprep_12m <- m2hepprep_raw %>%
  filter(redcap_event_name == "15 months") %>%
  select(record_id, adh2_visitdate, prep_prescribe, adh_prep, adh_noprep_reason___1, adh_noprep_reason___2, adh_noprep_reason___3, adh_noprep_reason___4, adh_noprep_reason___5, adh_noprep_reason___6, adh_noprep_reason___7, adh_noprep_reason___8, adh_noprep_reason___9, adh_noprep_reason___10, adh_noprep_reason___11, adh_noprep_reason___12, adh_noprep_reason___13, adh_noprep_reason___14, stop_prep, prep_last4week, prep_now)

# prep initiation
m2hepprep_tx_clean <- m2hepprep_tx_raw %>%
  select(record_id, rand_to_disp, within_6_months) %>%
  mutate(prep_init = factor(1, levels = c(0, 1), labels = c("No", "Yes"))) %>%
  filter(!is.na(record_id) & record_id != "")

# Create a list of visit names and their corresponding suffixes
visits <- list(
  "3 months" = "3m",
  "6 months" = "6m", 
  "9 months" = "9m",
  "12 months" = "12m",
  "15 months" = "15m"
)

# Variables to select (excluding redcap_event_name and record_id)
visit_vars <- c("adh2_visitdate", "prep_prescribe", "adh_prep",
                paste0("adh_noprep_reason___", 1:14),
                "stop_prep", "prep_last4week", "prep_now")

# Create dataframes in a loop
visit_data <- list()

for(visit_name in names(visits)) {
  suffix <- visits[[visit_name]]
  
  # Filter and select data
  df <- m2hepprep_raw %>%
    filter(redcap_event_name == visit_name) %>%
    select(redcap_event_name, record_id, all_of(visit_vars))
  
  # Rename columns (except redcap_event_name and record_id)
  new_names <- paste0(visit_vars, "_", suffix)
  names(df)[3:length(names(df))] <- new_names
  
  # Store in list
  visit_data[[paste0("m2hepprep_", suffix)]] <- df
}

# Extract individual dataframes if needed
m2hepprep_3m <- visit_data$m2hepprep_3m
m2hepprep_6m <- visit_data$m2hepprep_6m
m2hepprep_9m <- visit_data$m2hepprep_9m
m2hepprep_12m <- visit_data$m2hepprep_12m
m2hepprep_15m <- visit_data$m2hepprep_15m

# Combine all dataframes
m2hepprep_combined <- m2hepprep_baseline %>%
  left_join(m2hepprep_3m, by = "record_id") %>%
  left_join(m2hepprep_6m, by = "record_id") %>%
  left_join(m2hepprep_9m, by = "record_id") %>%
  left_join(m2hepprep_12m, by = "record_id") %>%
  left_join(m2hepprep_15m, by = "record_id") %>%
  left_join(m2hepprep_tx_clean, by = "record_id")

# Create prep_prescribe_any variable
m2hepprep_combined <- m2hepprep_combined %>%
  mutate(prep_prescribe_any = pmax(prep_prescribe_3m, prep_prescribe_6m, prep_prescribe_9m, prep_prescribe_12m, na.rm = TRUE))

# save data
write.csv(m2hepprep_combined, "data/m2hepprep_combined.csv", row.names = FALSE)

# combined baseline and prep dataframes
m2hepprep_prep_combined <- m2hepprep_baseline %>%
  left_join(m2hepprep_tx_clean, by = "record_id") %>%
  left_join(m2hepprep_consent, by = "record_id") %>%
  left_join(m2hepprep_baseline_vars, by = "record_id")

# drop rows with no consent and without eligibility
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  filter(is_eligible == "Yes") %>%
  filter(rc_informed == 1) %>%
  filter(rand_arm != "")

# create non-initiated prep level
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(prep_init = ifelse(is.na(prep_init), 0, prep_init))

# dichotomise variables
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    # Dichotomise sex (keep as Male/Female)
    sdem_sex_binary = case_when(
      sdem_sex == "Male" ~ "Male",
      sdem_sex == "Female" ~ "Female",
      TRUE ~ NA_character_
    ),
    # Dichotomise age (under 30 vs 30+)
    sdem_age_binary = case_when(
      as.numeric(sdem_age) < 40 ~ "Under 40",
      as.numeric(sdem_age) >= 40 ~ "40+",
      TRUE ~ NA_character_
    ),
    # Dichotomise housing into homeless vs not homeless
    sdem_slep6m_binary = case_when(
      sdem_slep6m %in% c("Homeless", "In a shelter") ~ "Homeless",
      sdem_slep6m %in% c("Drug treatment facility", "HIV/AIDS housing/group home", "Other", 
                         "Other residential facility or institution", "Owner", 
                         "Permanent single-room occupancy", "Rent", "Staying with friends/family", 
                         "Transitional") ~ "Not homeless",
      TRUE ~ NA_character_
    ),
    condom_1m = replace_na(
      case_when(
        srb_3m != "Yes" & srb_3m_f != "Yes" ~ 0,
        srb_1m_fc_pv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_pa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_npv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_npa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_cv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_ca %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mcv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mco %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mca %in% c("Never", "Rarely", "Some of the time") ~ 1,
        srb_1m_fc_pv %in% c("Very often", "Always") |
        srb_1m_fc_pa %in% c("Very often", "Always") |
        srb_1m_fc_npv %in% c("Very often", "Always") |
        srb_1m_fc_npa %in% c("Very often", "Always") |
        srb_1m_fc_cv %in% c("Very often", "Always") |
        srb_1m_fc_ca %in% c("Very often", "Always") |
        srb_1m_fc_mcv %in% c("Very often", "Always") |
        srb_1m_fc_mco %in% c("Very often", "Always") |
        srb_1m_fc_mca %in% c("Very often", "Always") ~ 2,
        TRUE ~ NA_real_
      ),
      0
    ),
    # Create syringe sharing binary variable
    syringe_share_6m_bin = case_when(
      sdem_idu6m___1 == "Use a needle that you knew or suspected someone else had used before" |
      #sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton" |
      sdem_idu6m___3 == "Ever skip cleaning your needle with bleach or boiling it after you were done" ~ "Yes",
      #sdem_idu6m___4 == "Let someone else use a needle after you used it" |
      #sdem_idu6m___5 == "Let someone else use the rinse water, etc" |
      #sdem_idu6m___6 == "Allow someone else to inject with drugs" 
      TRUE ~ "No"
    ),

    # Create syringe sharing binary variable
    syringe_loan_bin = case_when(
      (sdem_idu6m___4 == "Let someone else use a needle after you used it" | 
      sdem_idu6m___5 == "Let someone else use the rinse water, etc" | 
      sdem_idu6m___6 == "Allow someone else to inject with drugs") ~ "Yes",
      sdem_idu6m___1 == "Use a needle that you knew or suspected someone else had used before" |
      sdem_idu6m___0 == "Reuse a needle without cleaning it with bleach or boiling water first" |
      sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton" | 
      sdem_idu6m___3 == "Ever skip cleaning your needle with bleach or boiling it after you were done" | 
      sdem_idu6m___7 == "None of the above / NA" ~ "No",
      TRUE ~ NA_character_
    ),

    # other injecting risks
    syringe_other_bin = case_when(
      (sdem_idu6m___0 == "Reuse a needle without cleaning it with bleach or boiling water first" |
      sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton" | 
      sdem_idu6m___3 == "Ever skip cleaning your needle with bleach or boiling it after you were done") ~ "Yes",
      sdem_idu6m___4 == "Let someone else use a needle after you used it" | 
      sdem_idu6m___5 == "Let someone else use the rinse water, etc" | 
      sdem_idu6m___6 == "Allow someone else to inject with drugs" |
      sdem_idu6m___1 == "Use a needle that you knew or suspected someone else had used before" |
      sdem_idu6m___7 == "None of the above / NA" ~ "No",
      TRUE ~ NA_character_
    ), 

    # Create healthcare discrimination binary variable
    healthcare_disc_bin = case_when(
      # Discrimination = "Yes" if any of the discrimination variables are "Yes"
      (sdem_dis_hcv == "Yes" | sdem_dis_hiv == "Yes" | sdem_dis_sex == "Yes" | 
       sdem_dis_gay == "Yes" | sdem_dis_sub == "Yes" | sdem_dis_race == "Yes") ~ "Yes",
      # Discrimination = "No" if all are "No"
      (sdem_dis_hcv == "No" & sdem_dis_hiv == "No" & sdem_dis_sex == "No" & 
       sdem_dis_gay == "No" & sdem_dis_sub == "No" & sdem_dis_race == "No") ~ "No",
      TRUE ~ NA_character_
    ),
    # Create substance use discrimination binary variable
    sdem_dis_sub_bin = case_when(
      sdem_dis_sub == "Yes" ~ "Yes",
      sdem_dis_sub == "No" ~ "No",
      TRUE ~ NA_character_
    ),
    # Create incarceration binary variable
    incarc_6m_bin = case_when(
      as.numeric(cla_2) == 0 ~ "No",
      as.numeric(cla_2) > 0 ~ "Yes",
      TRUE ~ NA_character_
    ),
    # Clean violence variables - convert only empty strings to NA, keep "Refuse to answer"
    aiv_kid_evr_pa = case_when(
      aiv_kid_evr_pa == "" ~ NA_character_,
      TRUE ~ aiv_kid_evr_pa
    ),
    aiv_adt_evr_pa = case_when(
      aiv_adt_evr_pa == "" ~ NA_character_,
      TRUE ~ aiv_adt_evr_pa
    ),
    aiv_6m_pa = case_when(
      aiv_adt_evr_pa == "No" ~ "No",
      aiv_6m_pa == "" ~ NA_character_,
      TRUE ~ aiv_6m_pa
    ),
    aiv_kid_evr_sex = case_when(
      aiv_kid_evr_sex == "" ~ NA_character_,
      TRUE ~ aiv_kid_evr_sex
    ),
    aiv_adt_evr_sex = case_when(
      aiv_adt_evr_sex == "" ~ NA_character_,
      TRUE ~ aiv_adt_evr_sex
    ),
    aiv_6m_sex = case_when(
      aiv_adt_evr_sex == "No" ~ "No",
      aiv_6m_sex == "" ~ NA_character_,
      TRUE ~ aiv_6m_sex
    ),
    sex_work_ever = case_when(
      cla_16a == "" ~ NA_character_,
      TRUE ~ cla_16a,
    ),
    oat_ever = case_when(
      nms_opd == "" ~ NA_character_,
      TRUE ~ nms_opd,    
    ),
    bupe_current = case_when(
      nms_opd_med___0 == "Buprenorphine (oral)" ~ 1,  TRUE ~ 0
    ),
    lab_bupe_current = case_when(
      nms_opd_med___1 == "Buprenorphine (implant / inj.)" ~ 1,  TRUE ~ 0
    ),
    methadone_current = case_when(
      nms_opd_med___4 == "Methadone" ~ 1,  TRUE ~ 0
    ), 
    naltrexone_current = case_when(
      nms_opd_med___2 == "Naltrexone (oral)" ~ 1,  TRUE ~ 0
    ),
    other_oat_current = case_when(
      nms_opd_med___6 == "Other" ~ 1,  TRUE ~ 0
    ),
    oat_current = case_when(
      bupe_current == 1 | methadone_current == 1 | lab_bupe_current == 1 | other_oat_current == 1 ~ 1,
      TRUE ~ 0
    ),
    mental_health_prescribe_ever = case_when(
      nms_mnt == "" ~ NA_character_,
      TRUE ~ nms_mnt
    ),
    therapy_ever = case_when(
      nms_trp == "" ~ NA_character_,
      TRUE ~ nms_trp
    ),
    msm_ever = case_when(
    dem_gender == "Man" & (dem_gender_id == "Homosexual" | dem_gender_id == "Bisexual") ~ 1,
    dem_gender == "Man" & (dem_gender_id == "Heterosexual" | dem_gender_id == "Other") ~ 0,
    TRUE ~ NA_real_
    ),
    syringe_share_bin_ever = case_when(
      sdu_srg == "" ~ NA_character_,
      TRUE ~ sdu_srg
    ),
    # Create education status variable
    education_status_4cat = case_when(
      dem_edu %in% c("Middle school (Jr high school) or less", "Some high school, no diploma") ~ "Middle school or less",
      dem_edu %in% c("High school diploma/GED or equivalent", "Junior (2-year) college / CEGEP", "Technical/trade/vocational school", "Some college (4-year college or university)") ~ "High school diploma",
      dem_edu %in% c("College graduate (4-year college or university)", "Graduate or professional school") ~ "College graduate or higher",
      dem_edu == "Choose not to answer" ~ "No answer",
      dem_edu == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Create converted income variable
    income_con = case_when(
      !is.na(nms_inc_cad) ~ nms_inc_cad * 0.73,
      TRUE ~ nms_inc_usd
    ),
    # Create income categories
    income_4cat = case_when(
      income_con == 0 ~ "No income",
      income_con > 0 & income_con < 500 ~ "<500",
      income_con >= 500 & income_con <= 1500 ~ "500-1500",
      income_con > 1500 ~ ">1500",
      TRUE ~ NA_character_
    ),
    employment_current = case_when(
      nms_emp == "" ~ NA_character_,
      TRUE ~ nms_emp
    ),
    healthcare_coverage = case_when(
      dem_hltins == "" ~ NA_character_,
      TRUE ~ dem_hltins
    ),
    hr_use = case_when(
      sdem_sev == "" ~ NA_character_,
      TRUE ~ sdem_sev
    ),
    overdose_6m = case_when(
      odu_6m == "" ~ NA_character_,
      TRUE ~ odu_6m
    ),    
    days_used_1m = case_when(
      sub_frq1m == "" ~ NA_real_,
      TRUE ~ as.numeric(sub_frq1m)
    ),    
    days_used_1m_3cat = case_when(
      days_used_1m > 24 ~ "25-30", 
      days_used_1m < 25 & days_used_1m > 14 ~ "15-24",
      days_used_1m < 15 ~ "0-14",
      TRUE ~ NA_character_
    ),
    nsp_use = case_when(
      sdem_sev == "Syringe access program (SAP)" | sdem_sev == "Both" ~ 1,
      sdem_sev == "None" | sdem_sev == "Opioid agonist therapy (OAT) clinic" ~ 0
    ),

    inject_stims_6m = ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes" | sub_6m8 == "Yes" | sub_6m13 == "Yes", 1, 0),

    inject_opioids_6m = ifelse(sub_6m1 == "Yes" | sub_6m8 == "Yes" | sub_6m9 == "Yes" | sub_6m10 == "Yes" | sub_6m11 == "Yes" | sub_6m12 == "Yes" | sub_6m22 == "Yes", 1, 0),

    inject_heroin_6m = ifelse(sub_6m1 == "Yes", 1, 0),

    inject_cocaine_6m = ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes", 1, 0),

    inject_fent_6m = ifelse(sub_6m27 == "Yes", 1, 0),

    inject_meth_6m = ifelse(sub_6m13 == "Yes", 1, 0)
  )

# ARCH-IDU injection sub-score
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    subscore_cocaine = ifelse(inject_cocaine_6m, 1, 0),
    subscore_heroin = ifelse(inject_heroin_6m, 1, 0),
    subscore_cooker = ifelse(
      sdu_wrk_6m_frq %in% c("One time", "Less than 5 times", "6-10 times", "More than 10 times", "More than 100 times") |
      sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton",
      1, 0
    ),
    subscore_sharing = ifelse(syringe_share_6m_bin == "Yes", 1, 0),
    subscore_gallery = ifelse(idu_6mplc2___3 == "Crack house/shooting gallery", 1, 0),
    subscore_total = subscore_heroin + subscore_cocaine + subscore_cooker + subscore_sharing + subscore_gallery
  )

# ARCH-IDU injection sub-score sensitivity
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    subscore_stims = ifelse(inject_stims_6m, 1, 0),
    subscore_opioids = ifelse(inject_opioids_6m, 1, 0),
    subscore_cooker = ifelse(
      sdu_wrk_6m_frq %in% c("One time", "Less than 5 times", "6-10 times", "More than 10 times", "More than 100 times") |
      sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton",
      1, 0
    ),
    subscore_sharing = ifelse(syringe_share_6m_bin == "Yes", 1, 0),
    subscore_gallery = ifelse(idu_6mplc2___3 == "Crack house/shooting gallery", 1, 0),
    subscore_total = subscore_heroin + subscore_cocaine + subscore_cooker + subscore_sharing + subscore_gallery
  )

# tab five subscore variables
tab_subscores <- CreateTableOne(
  vars = c("subscore_heroin", "subscore_cocaine", "subscore_cooker", "subscore_sharing", "subscore_gallery"),
  data = m2hepprep_prep_combined
)
print(tab_subscores, showAllLevels = TRUE)

# ARCH-IDU risk score
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    arch_age = case_when(
      sdem_age > 49 ~ 0,
      sdem_age > 39 & sdem_age < 50 ~ 7,
      sdem_age > 29 & sdem_age < 40 ~ 24,
      sdem_age < 30 ~ 38,
    ),
    arch_oat = case_when(
      oat_current == 1 ~ 0,
      oat_current == 0 ~ 31,
      TRUE ~ NA_real_
    ),
    arch_injection = case_when(
      subscore_total == 0 ~ 0,
      subscore_total == 1 ~ 7,
      subscore_total == 2 ~ 21,
      subscore_total == 3 ~ 24,
      subscore_total == 4 ~ 24,
      subscore_total == 5 ~ 31,
    ),
    arch_total = arch_age + arch_oat + arch_injection,
    arch_bin = factor(
       case_when(
        arch_total < 46 ~ 0,
        arch_total > 45 ~ 1
    ),
    levels = c(0, 1),
    labels = c("Low risk", "High risk")
))

# save data
write.csv(m2hepprep_prep_combined, "data/m2hepprep_prep_combined.csv", row.names = FALSE)

# ARCH-IDU injection sub-score sensitivity (using opioids and stimulants instead of heroin and cocaine)
m2hepprep_prep_combined_sens <- m2hepprep_prep_combined %>%
  mutate(
    subscore_opioids_sens = ifelse(inject_opioids_6m, 1, 0),
    subscore_stims_sens = ifelse(inject_stims_6m, 1, 0),
    subscore_cooker_sens = ifelse(
      sdu_wrk_6m_frq %in% c("One time", "Less than 5 times", "6-10 times", "More than 10 times", "More than 100 times") |
      sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton",
      1, 0
    ),
    subscore_sharing_sens = ifelse(syringe_share_6m_bin == "Yes", 1, 0),
    subscore_gallery_sens = ifelse(idu_6mplc2___3 == "Crack house/shooting gallery", 1, 0),
    subscore_total_sens = subscore_opioids_sens + subscore_stims_sens + subscore_cooker_sens + subscore_sharing_sens + subscore_gallery_sens,
    arch_injection_sens = case_when(
      subscore_total_sens == 0 ~ 0,
      subscore_total_sens == 1 ~ 7,
      subscore_total_sens == 2 ~ 21,
      subscore_total_sens == 3 ~ 24,
      subscore_total_sens == 4 ~ 24,
      subscore_total_sens == 5 ~ 31,
    ),
    arch_total_sens = arch_age + arch_oat + arch_injection_sens,
    arch_bin_sens = factor(
      case_when(
        arch_total_sens < 46 ~ 0,
        arch_total_sens > 45 ~ 1
      ),
      levels = c(0, 1),
      labels = c("Low risk", "High risk")
    )
  )

# Save sensitivity dataframe
write.csv(m2hepprep_prep_combined_sens, "data/m2hepprep_prep_combined_sens.csv", row.names = FALSE)

# Save city-specific sensitivity dataframes
m2hepprep_prep_combined_montreal_sens <- m2hepprep_prep_combined_sens %>%
  filter(sdem_reside == "Greater Montreal area")
write.csv(m2hepprep_prep_combined_montreal_sens, "data/m2hepprep_prep_combined_montreal_sens.csv", row.names = FALSE)

m2hepprep_prep_combined_miami_sens <- m2hepprep_prep_combined_sens %>%
  filter(sdem_reside == "Greater Miami area")
write.csv(m2hepprep_prep_combined_miami_sens, "data/m2hepprep_prep_combined_miami_sens.csv", row.names = FALSE)

# save data
write.csv(m2hepprep_prep_combined_sensitivity, "data/m2hepprep_prep_combined_sensitivity.csv", row.names = FALSE)

# save montreal data
m2hepprep_prep_combined_montreal <- m2hepprep_prep_combined %>%
  filter(sdem_reside == "Greater Montreal area")

write.csv(m2hepprep_prep_combined_montreal, "data/m2hepprep_prep_combined_montreal.csv", row.names = FALSE)

# save miami data
m2hepprep_prep_combined_miami <- m2hepprep_prep_combined %>%
  filter(sdem_reside == "Greater Miami area")

write.csv(m2hepprep_prep_combined_miami, "data/m2hepprep_prep_combined_miami.csv", row.names = FALSE)

