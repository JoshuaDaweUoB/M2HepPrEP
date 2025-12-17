# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, gtsummary, xfun)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load raw data
m2hepprep_raw <- read.csv("data/cleaned_m2_data_05012025 (1).csv")
m2hepprep_tx_raw <- read.csv("data/Initiation and adhrence.csv")

# sheet of baseline/screening rows
df_filtered <- m2hepprep_raw %>%
  filter(redcap_event_name %in% c("Baseline", "Screening (visit 1)"))

df_filtered_complete <- df_filtered[, colSums(is.na(df_filtered)) == 0]

# ---------------------------
# informed consent
# ---------------------------
m2hepprep_consent <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  select(any_of(c("record_id", "rc_informed")))

# ---------------------------
# baseline vars
# ---------------------------

condom_vars <- c(
  # Female partners
  "srb_1m_fc_pv","srb_1m_fc_po","srb_1m_fc_pa",
  "srb_1m_fc_npv","srb_1m_fc_npo","srb_1m_fc_npa",
  "srb_1m_fc_cv","srb_1m_fc_co","srb_1m_fc_ca",
  "srb_1m_fc_mcv","srb_1m_fc_mco","srb_1m_fc_mca",
  "srb_1m_fc_hiv","srb_1m_fc_hcv",

  # Male partners
  "srb_1m_mc_pv","srb_1m_mc_po","srb_1m_mc_pa",
  "srb_1m_mc_npv","srb_1m_mc_npo","srb_1m_mc_npa",
  "srb_1m_mc_cv","srb_1m_mc_co","srb_1m_mc_ca",
  "srb_1m_mc_mcv","srb_1m_mc_mco","srb_1m_mc_mca",
  "srb_1m_mc_hiv","srb_1m_mc_hcv"
)

# check which condom_vars exist in your dataframe
condom_vars[condom_vars %in% colnames(m2hepprep_raw)]
# check which ones are missing
condom_vars[!condom_vars %in% colnames(m2hepprep_raw)]

baseline_vars <- c(
  "record_id",
  "aiv_kid_evr_pa","aiv_adt_evr_pa","aiv_6m_pa",
  "aiv_kid_evr_sex","aiv_adt_evr_sex","aiv_6m_sex",
  "cla_2","cla_16a","cla_16c",
  "nms_er","nms_hps_drg","nms_otp","nms_rsd","nms_auc","nms_opd","nms_mnt","nms_trp",
  "srb_1m_m","dem_edu","nms_emp","nms_inc","nms_inc_cad","nms_inc_usd",
  "srb_1m_fc_hiv","srb_3m_m",
  "nms_opd_med___0","nms_opd_med___1","nms_opd_med___2","nms_opd_med___3",
  "nms_opd_med___4","nms_opd_med___5","nms_opd_med___6","nms_opd_med_ot",
  "odu_6m","dem_hltins","sdu_srg","sub_frq1m",
  "srb_3m_sxp","srb_1m_m_hiv","srb_1m_f_hiv",
  "srb_1m_f_drg_cc","srb_1m_f_drg_hro","srb_1m_f_drg_main",
  "srb_1m_f_drg_aph","srb_1m_f_drg_psy",
  "cdu_17","cdu_15",
  paste0("sub_6m", 1:27),
  "sdu_wrk","sdu_wrk_6m_frq",
  "srb_3m","idu_6mplc2___3",
  "dem_gender_id","dem_gender",
  "srb_3m_prst","srb_prst04","srb_3m_f",
  "srb_1m_fc_pv","srb_1m_fc_pa","srb_1m_fc_npv","srb_1m_fc_npa",
  "srb_1m_fc_cv","srb_1m_fc_ca","srb_1m_fc_mcv","srb_1m_fc_mco","srb_1m_fc_mca",
  "srb_1m_mc_pv","srb_1m_mc_pa","srb_1m_mc_npv","srb_1m_mc_npa",
  "srb_1m_mc_cv","srb_1m_mc_ca","srb_1m_mc_mcv","srb_1m_mc_mco","srb_1m_mc_mca",
  "srb_1m_f",
  "srb_3m_f_sxp_pidu","srb_3m_f_sxp_npidu","srb_3m_f_sxp_cidu","srb_3m_f_sxp_mcidu",
  "srb_3m_m_sxp_pidu","srb_3m_m_sxp_npidu","srb_3m_m_sxp_cidu","srb_3m_m_sxp_mcidu",
  condom_vars
)

m2hepprep_baseline_vars <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  select(any_of(baseline_vars))

# ---------------------------
# screening/baseline visit data (SAFE)
# ---------------------------
screening_vars <- c(
  "record_id","id_paper","rand_arm","rand_date","is_eligible",
  "sdem_visit","sdem_reside","sdem_lang_mia_2","sdem_age","sdem_oat",
  "sdem_sev","sdem_sex","sdem_gender","sdem_prg_c","vcp_inject_6mo",
  "sdem_hiv_etst","sdem_hiv_rtst","sdem_hiv_rtst_r","sdem_prp_cu",
  "sdem_hcv","sdem_wil_fol","sdem_elig","sdem_hcv_etst",
  "sdem_hcv_rtst","sdem_hcv_rtst_r",
  "scr_c_hcv_res_retired","insti",
  "sdem_slep6m","sdem_live6m_hls","sdem_live6m_shl1","sdem_live6m_trs",
  "sdem_live6m_htl","sdem_live6m_hiv","sdem_live6m_sut",
  paste0("sdem_live6m_shl", 2:5),
  "sdem_idu",
  paste0("sdem_idu6m___", 0:7),
  "sdem_dis_hcv","sdem_dis_hiv","sdem_dis_sex","sdem_dis_gay",
  "sdem_dis_sub","sdem_dis_race",
  "vir_dbs","vir_rna2"
)

m2hepprep_baseline <- m2hepprep_raw %>%
  filter(redcap_event_name == "Screening (visit 1)") %>%
  select(any_of(screening_vars))

# ---------------------------
# prep initiation
# ---------------------------
m2hepprep_tx_clean <- m2hepprep_tx_raw %>%
  select(any_of(c("record_id", "rand_to_disp", "within_6_months"))) %>%
  mutate(prep_init = factor(1, levels = c(0, 1), labels = c("No", "Yes"))) %>%
  filter(!is.na(record_id) & record_id != "")

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

m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(prep_init = factor(prep_init, levels = c(0,2), labels = c("No", "Yes")),
         prep_init_num = ifelse(prep_init == "Yes", 1, 0))

# socio-structural risks
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    sdem_sex_binary = factor(
      case_when(
        sdem_sex == "Male" ~ "0",
        sdem_sex == "Female" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    sdem_age_binary = factor(
      case_when(
        as.numeric(sdem_age) < 40 ~ "0",
        as.numeric(sdem_age) >= 40 ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    sdem_reside_binary = factor(
      case_when(
        sdem_reside == "Greater Montreal area" ~ "0",
        sdem_reside == "Greater Miami area" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    sdem_slep6m_binary = factor(
      case_when(
        sdem_slep6m %in% c("Homeless", "In a shelter","Transitional") ~ "1",
        sdem_slep6m %in% c("Drug treatment facility", "HIV/AIDS housing/group home", "Other", 
                           "Other residential facility or institution", "Owner", 
                           "Permanent single-room occupancy", "Rent", "Staying with friends/family"
                           ) ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    healthcare_disc_bin = factor(
      case_when(
        (sdem_dis_hcv == "Yes" | sdem_dis_hiv == "Yes" | sdem_dis_sex == "Yes" | 
         sdem_dis_gay == "Yes" | sdem_dis_sub == "Yes" | sdem_dis_race == "Yes") ~ "1",
        (sdem_dis_hcv == "No" & sdem_dis_hiv == "No" & sdem_dis_sex == "No" & 
         sdem_dis_gay == "No" & sdem_dis_sub == "No" & sdem_dis_race == "No") ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    sdem_dis_sub_bin = factor(
      case_when(
        sdem_dis_sub == "Yes" ~ "1",
        sdem_dis_sub == "No" ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    incarc_6m_bin = factor(
      case_when(
        as.numeric(cla_2) == 0 ~ "0",
        as.numeric(cla_2) > 0 ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    aiv_kid_evr_pa = factor(
      case_when(
        aiv_kid_evr_pa == "" ~ NA_character_,
        TRUE ~ aiv_kid_evr_pa
      )
    ),
    aiv_adt_evr_pa = factor(
      case_when(
        aiv_adt_evr_pa == "" ~ NA_character_,
        TRUE ~ aiv_adt_evr_pa
      )
    ),
    aiv_6m_pa = factor(
      case_when(
        aiv_adt_evr_pa == "No" ~ "0",
        aiv_6m_pa == "" ~ NA_character_,
        aiv_6m_pa == "Yes" ~ "1",
        aiv_6m_pa == "No" ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    aiv_kid_evr_sex = factor(
      case_when(
        aiv_kid_evr_sex == "" ~ NA_character_,
        TRUE ~ aiv_kid_evr_sex
      )
    ),
    sexual_abuse_ever = factor(
      case_when(
        aiv_adt_evr_sex == "" ~ NA_character_,
        aiv_adt_evr_sex == "Refuse to answer" ~ NA_character_,
        aiv_adt_evr_sex == "No" ~ "0",
        aiv_adt_evr_sex == "Yes" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    sexual_abuse_6m = factor(
      case_when(
        aiv_adt_evr_sex == "No" ~ "0",
        aiv_6m_sex == "" ~ NA_character_,
        aiv_6m_sex == "Refuse to answer" ~ NA_character_,
        aiv_6m_sex == "No" ~ "0",
        aiv_6m_sex == "Yes" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    oat_ever = factor(
      case_when(
        nms_opd == "" ~ NA_character_,
        nms_opd == "No" ~ "0",
        nms_opd == "Yes" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    bupe_current = factor(
      case_when(
        nms_opd_med___0 == "Buprenorphine (oral)" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    lab_bupe_current = factor(
      case_when(
        nms_opd_med___1 == "Buprenorphine (implant / inj.)" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    methadone_current = factor(
      case_when(
        nms_opd_med___4 == "Methadone" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    naltrexone_current = factor(
      case_when(
        nms_opd_med___2 == "Naltrexone (oral)" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    other_oat_current = factor(
      case_when(
        nms_opd_med___6 == "Other" | nms_opd_med___2 == "Naltrexone (oral)" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    oat_current = factor(
      case_when(
        bupe_current == "1" | methadone_current == "1" | lab_bupe_current == "1" | other_oat_current == "1" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    mental_health_prescribe_ever = factor(
      case_when(
        nms_mnt == "" ~ NA_character_,
        TRUE ~ nms_mnt
      )
    ),
    therapy_ever = factor(
      case_when(
        nms_trp == "" ~ NA_character_,
        TRUE ~ nms_trp
      )
    ),
    msm_ever = factor(
      case_when(
        dem_gender == "Man" & srb_1m_m == "Yes" ~ "1",
        dem_gender == "Man" & srb_3m_m == "Yes" ~ "1",
        dem_gender == "Man" & (dem_gender_id == "Homosexual" | dem_gender_id == "Bisexual") ~ "1",
        dem_gender == "Man" & srb_1m_m == "No" ~ "0",
        dem_gender == "Man" & srb_3m_m == "No" ~ "0",
        dem_gender == "Man" & (dem_gender_id == "Heterosexual" | dem_gender_id == "Other") ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    education_status_4cat = factor(
      case_when(
        dem_edu %in% c("Middle school (Jr high school) or less", "Some high school, no diploma") ~ "Middle school or less",
        dem_edu %in% c("High school diploma/GED or equivalent", "Junior (2-year) college / CEGEP", "Technical/trade/vocational school", "Some college (4-year college or university)") ~ "High school diploma",
        dem_edu %in% c("College graduate (4-year college or university)", "Graduate or professional school") ~ "College graduate or higher",
        dem_edu == "Choose not to answer" ~ "No answer",
        dem_edu == "" ~ NA_character_,
        TRUE ~ NA_character_
      ),
      levels = c("Middle school or less", "High school diploma", "College graduate or higher", "No answer")
    ),
    employment_current = factor(
      case_when(
        nms_emp == "" ~ NA_character_,
        TRUE ~ nms_emp
      )
    ),
    healthcare_coverage = factor(
      case_when(
        dem_hltins == "" ~ NA_character_,
        dem_hltins == "Don't know" ~ NA_character_,
        TRUE ~ dem_hltins
      )
    ),
    hr_use = factor(
      case_when(
        sdem_sev == "" ~ NA_character_,
        TRUE ~ sdem_sev
      )
    ),
    nsp_use = factor(
      case_when(
        sdem_sev == "Syringe access program (SAP)" | sdem_sev == "Both" ~ "1",
        sdem_sev == "None" | sdem_sev == "Opioid agonist therapy (OAT) clinic" ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    )
  )

# injecting risks
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    syringe_share_6m_bin = factor(
      case_when(
        sdem_idu6m___1 == "Use a needle that you knew or suspected someone else had used before" |
        sdem_idu6m___3 == "Ever skip cleaning your needle with bleach or boiling it after you were done" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    syringe_loan_6m_bin = factor(
      case_when(
        (sdem_idu6m___4 == "Let someone else use a needle after you used it" | 
         sdem_idu6m___5 == "Let someone else use the rinse water, etc" | 
         sdem_idu6m___6 == "Allow someone else to inject with drugs") ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    syringe_cooker_6m_bin = factor(
      case_when(
         sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    syringe_reuse_6m_bin = factor(
      case_when(
         sdem_idu6m___0 == "Reuse a needle without cleaning it with bleach or boiling water first" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    syringe_share_bin_ever = factor(
      case_when(
        sdu_srg == "" ~ NA_character_,
        sdu_srg == "No" ~ "0",
        sdu_srg == "Yes" ~ "1",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    overdose_6m = factor(
      case_when(
        odu_6m == "" ~ NA_character_,
        odu_6m == "Yes" ~ "1",
        odu_6m == "No" ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    days_used_1m = case_when(
      sub_frq1m == "" ~ NA_real_,
      TRUE ~ as.numeric(sub_frq1m)
    ),
    days_used_1m_3cat = factor(
      case_when(
        days_used_1m > 24 ~ "2", 
        days_used_1m < 25 & days_used_1m > 14 ~ "1",
        days_used_1m < 15 ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1", "2")
    ),
    inject_stims_6m = factor(ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes" | sub_6m8 == "Yes" | sub_6m13 == "Yes", "1", "0"), levels = c("0", "1")),
    inject_opioids_6m = factor(ifelse(sub_6m1 == "Yes" | sub_6m8 == "Yes" | sub_6m9 == "Yes" | sub_6m10 == "Yes" | sub_6m11 == "Yes" | sub_6m12 == "Yes" | sub_6m22 == "Yes", "1", "0"), levels = c("0", "1")),
    inject_heroin_6m = factor(ifelse(sub_6m1 == "Yes", "1", "0"), levels = c("0", "1")),
    inject_cocaine_6m = factor(ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes", "1", "0"), levels = c("0", "1")),
    inject_fent_6m = factor(ifelse(sub_6m27 == "Yes", "1", "0"), levels = c("0", "1")),
    inject_meth_6m = factor(ifelse(sub_6m13 == "Yes", "1", "0"), levels = c("0", "1"))
  )

# sexual risks

# no sex past month and three months
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    any_sex_3m = case_when(
      srb_3m == "Yes" | srb_3m_m == "Yes" | srb_3m_f == "Yes" | srb_3m_prst == "Yes" ~ 1,
      srb_3m == "No"  | srb_3m_m == "No"  | srb_3m_f == "No" | srb_3m_prst == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    any_sex_1m = case_when(
      srb_1m_f == "Yes" | srb_1m_m == "Yes" ~ 1,
      srb_1m_f == "No"  | srb_1m_m == "No"  ~ 0,
      TRUE ~ NA_real_
    )
  )

table(m2hepprep_prep_combined$any_sex_3m, useNA = "ifany")
table(m2hepprep_prep_combined$any_sex_1m, useNA = "ifany")
tab <- with(m2hepprep_prep_combined,
            table(any_sex_1m, any_sex_3m, useNA = "ifany"))
addmargins(tab)  

# set empty strings to NA
cols_to_na <- c(
  "srb_3m_prst", "srb_prst04", "srb_1m_f_hiv", "sdem_sex", "srb_3m", "srb_1m_fc_hiv", "srb_3m_sxp",
  "srb_1m_f_drg_cc", "srb_1m_f_drg_hro", "srb_1m_f_drg_main", "srb_1m_f_drg_aph", "srb_1m_f_drg_psy",
  "srb_3m_f", "srb_1m_fc_pv", "srb_1m_fc_pa", "srb_1m_fc_npv", "srb_1m_fc_npa", "srb_1m_fc_cv",
  "srb_1m_fc_ca", "srb_1m_fc_mcv", "srb_1m_fc_mco", "srb_1m_fc_mca", "cla_16a", "srb_3m_f_sxp_pidu",
  "srb_3m_f_sxp_npidu", "srb_3m_f_sxp_cidu", "srb_3m_f_sxp_mcidu", "srb_3m_m_sxp_pidu", "srb_3m_m_sxp_npidu",
  "srb_3m_m_sxp_cidu", "srb_3m_m_sxp_mcidu"
)

# condom questions to binary
m2hepprep_prep_combined[cols_to_na] <- lapply(
  m2hepprep_prep_combined[cols_to_na],
  function(x) ifelse(x == "", NA, x)
)

m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(across(all_of(condom_vars),
                ~case_when(
                  . %in% c("Never", "Rarely", "Some of the time") ~ 0,
                  . %in% c("Always", "Very often") ~ 1,
                  TRUE ~ NA_real_
                )))

# binary indicators of condom use
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  rowwise() %>%
  mutate(
    any_inconsistent = any(c_across(all_of(condom_vars)) %in% c(0), na.rm = TRUE),
    any_consistent   = any(c_across(all_of(condom_vars)) %in% c(1), na.rm = TRUE)
  )

table(m2hepprep_prep_combined$any_inconsistent, useNA = "ifany")
table(m2hepprep_prep_combined$any_consistent, useNA = "ifany")

# derive condom_1m
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
    mutate(
    condom_1m = factor(
      case_when(
        any_sex_1m == 0 ~ 0,
        any_inconsistent == 1 ~ 3,
        any_consistent == 1 ~ 2,
        any_sex_3m == 0 ~ 1,
        TRUE ~ NA_real_
      ),
      levels = c(0,1,2,3),
      labels = c(
        "No sex past month",
        "No sex past 3 months",
        "Very often / Always",
        "Never / Rarely / Some of the time"
      )
    )
  )

table(m2hepprep_prep_combined$condom_1m, useNA = "ifany")

## other sexual risk variables
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    sexwork_3m = factor(
      case_when(
        srb_3m_prst == "Yes" ~ "1",
        any_sex_3m == 0      ~ "0",
        TRUE                 ~ NA_character_
      ),
      levels = c("0", "1")
    ),
    num_sex_partners_3m = factor(
      case_when(
        as.numeric(srb_3m_sxp) == 0 ~ "0",
        as.numeric(srb_3m_sxp) == 1 ~ "1",
        as.numeric(srb_3m_sxp) > 1 ~ "2",
        any_sex_3m == 0      ~ "0",
        TRUE                 ~ NA_character_
      ),
      levels = c("0", "1", "2")
    ),
    sex_work_ever = factor(
      case_when(
        cla_16a == "Yes" ~ "1",
        TRUE ~ "0"
      ),
      levels = c("0", "1")
    ),
    condom_intent_6m = factor(
      case_when(
        cdu_17 == "Disagree" | cdu_17 == "Strongly Disagree" ~ "0",
        cdu_17 == "Neither agree nor disagree" ~ "1",
        cdu_17 == "Agree" | cdu_17 == "Strongly Agree" ~ "2",
        TRUE ~ NA_character_  # catches any unexpected values
      ),
      levels = c("0", "1", "2")
    )
  )

# make sure condom use and buying sex are consistent
tab <- with(m2hepprep_prep_combined,
            table(condom_1m, sexwork_3m, useNA = "ifany"))

table(m2hepprep_prep_combined$condom_1m, useNA = "ifany")
table(m2hepprep_prep_combined$sexwork_3m, useNA = "ifany")
addmargins(tab)  








# save data
write.csv(m2hepprep_prep_combined, "data/m2hepprep_combined.csv", row.names = FALSE)
