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

# informed consent
m2hepprep_consent <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  select(record_id, rc_informed)

# baseline vars
m2hepprep_baseline_vars <- m2hepprep_raw %>%
  filter(redcap_event_name == "Baseline") %>%
  dplyr::select(
    record_id, aiv_kid_evr_pa, aiv_adt_evr_pa, aiv_6m_pa, aiv_kid_evr_sex, aiv_adt_evr_sex, aiv_6m_sex,
    cla_2, cla_16a, cla_16c, nms_er, nms_hps_drg, nms_otp, nms_rsd, nms_auc, nms_opd, nms_mnt, nms_trp,
    srb_1m_m, dem_edu, nms_emp, nms_inc, nms_inc_cad, nms_inc_usd, srb_1m_fc_hiv, srb_3m_m,
    nms_opd_med___0, nms_opd_med___1, nms_opd_med___2, nms_opd_med___3, nms_opd_med___4, nms_opd_med___5, nms_opd_med___6, nms_opd_med_ot,
    odu_6m, dem_hltins, sdu_srg, sub_frq1m, srb_3m_sxp, srb_1m_m_hiv, srb_1m_f_hiv, srb_1m_f_drg_cc, srb_1m_f_drg_hro, srb_1m_f_drg_main,
    srb_1m_f_drg_aph, srb_1m_f_drg_psy, 
    sub_6m1, sub_6m2, sub_6m3, sub_6m4, sub_6m5, sub_6m6, sub_6m7, sub_6m8, sub_6m9, sub_6m10, sub_6m11, sub_6m12, 
    sub_6m13, sub_6m14, sub_6m15, sub_6m16, sub_6m17, sub_6m18, sub_6m19, sub_6m20, sub_6m21, sub_6m22, sub_6m23, sub_6m24, sub_6m25, sub_6m26, sub_6m27,
    sdu_wrk, sdu_wrk_6m_frq, srb_3m, idu_6mplc2___3, dem_gender_id, dem_gender,
    srb_3m_prst, srb_prst04, srb_3m_f, srb_1m_fc_pv, srb_1m_fc_pa, srb_1m_fc_npv, srb_1m_fc_npa, srb_1m_fc_cv, srb_1m_fc_ca, srb_1m_fc_mcv, srb_1m_fc_mco, srb_1m_fc_mca,
    srb_1m_mc_pv, srb_1m_mc_pa, srb_1m_mc_npv, srb_1m_mc_npa, srb_1m_mc_cv, srb_1m_mc_ca, srb_1m_mc_mcv, srb_1m_mc_mco, srb_1m_mc_mca, srb_1m_f,
    srb_3m_f_sxp_pidu, srb_3m_f_sxp_npidu, srb_3m_f_sxp_cidu, srb_3m_f_sxp_mcidu, srb_3m_m_sxp_pidu, srb_3m_m_sxp_npidu, srb_3m_m_sxp_cidu, srb_3m_m_sxp_mcidu
  )

# baseline data
m2hepprep_baseline <- m2hepprep_raw %>%
  filter(redcap_event_name == "Screening (visit 1)") %>%
  select(record_id, id_paper, rand_arm, rand_date, is_eligible, sdem_visit, sdem_reside, sdem_lang_mia_2, sdem_age, sdem_oat, sdem_sev, sdem_sex, sdem_gender, sdem_prg_c, vcp_inject_6mo, sdem_hiv_etst, sdem_hiv_rtst_r, sdem_prp_cu, sdem_hcv, sdem_wil_fol, sdem_elig, sdem_hcv_etst, sdem_hcv_rtst, sdem_hcv_rtst_r, sdem_hiv_rtst, scr_c_hcv_res_retired, insti, sdem_slep6m, sdem_live6m_hls, sdem_live6m_shl1, sdem_live6m_trs, sdem_live6m_htl, sdem_live6m_hiv, sdem_live6m_sut, sdem_live6m_shl2, sdem_live6m_shl3, sdem_live6m_shl4, sdem_live6m_shl5, sdem_idu, sdem_idu6m___0, sdem_idu6m___1, sdem_idu6m___2, sdem_idu6m___3, sdem_idu6m___4, sdem_idu6m___5, sdem_idu6m___6, sdem_idu6m___7, sdem_dis_hcv, sdem_dis_hiv, sdem_dis_sex, sdem_dis_gay, sdem_dis_sub, sdem_dis_race, vir_dbs, vir_rna2)

# prep initiation
m2hepprep_tx_clean <- m2hepprep_tx_raw %>%
  select(record_id, rand_to_disp, within_6_months) %>%
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
        sdem_sex == "Male" ~ "Male",
        sdem_sex == "Female" ~ "Female",
        TRUE ~ NA_character_
      ),
      levels = c("Male", "Female")
    ),
    sdem_age_binary = factor(
      case_when(
        as.numeric(sdem_age) < 40 ~ "Under 40",
        as.numeric(sdem_age) >= 40 ~ "40+",
        TRUE ~ NA_character_
      ),
      levels = c("Under 40", "40+")
    ),
    sdem_slep6m_binary = factor(
      case_when(
        sdem_slep6m %in% c("Homeless", "In a shelter") ~ "Homeless",
        sdem_slep6m %in% c("Drug treatment facility", "HIV/AIDS housing/group home", "Other", 
                           "Other residential facility or institution", "Owner", 
                           "Permanent single-room occupancy", "Rent", "Staying with friends/family", 
                           "Transitional") ~ "Not homeless",
        TRUE ~ NA_character_
      ),
      levels = c("Homeless", "Not homeless")
    ),
    healthcare_disc_bin = factor(
      case_when(
        (sdem_dis_hcv == "Yes" | sdem_dis_hiv == "Yes" | sdem_dis_sex == "Yes" | 
         sdem_dis_gay == "Yes" | sdem_dis_sub == "Yes" | sdem_dis_race == "Yes") ~ "Yes",
        (sdem_dis_hcv == "No" & sdem_dis_hiv == "No" & sdem_dis_sex == "No" & 
         sdem_dis_gay == "No" & sdem_dis_sub == "No" & sdem_dis_race == "No") ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    sdem_dis_sub_bin = factor(
      case_when(
        sdem_dis_sub == "Yes" ~ "Yes",
        sdem_dis_sub == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    incarc_6m_bin = factor(
      case_when(
        as.numeric(cla_2) == 0 ~ "No",
        as.numeric(cla_2) > 0 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
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
        aiv_adt_evr_pa == "No" ~ "No",
        aiv_6m_pa == "" ~ NA_character_,
        TRUE ~ aiv_6m_pa
      ),
      levels = c("No", "Yes")
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
        TRUE ~ aiv_adt_evr_sex
      ),
      levels = c("No", "Yes")
    ),
    sexual_abuse_6m = factor(
      case_when(
        aiv_adt_evr_sex == "No" ~ "No",
        aiv_6m_sex == "" ~ NA_character_,
        aiv_6m_sex == "Refuse to answer" ~ NA_character_,
        TRUE ~ aiv_6m_sex
      ),
      levels = c("No", "Yes")
    ),
    oat_ever = factor(
      case_when(
        nms_opd == "" ~ NA_character_,
        TRUE ~ nms_opd
      ),
      levels = c("No", "Yes")
    ),
    bupe_current = factor(
      case_when(
        nms_opd_med___0 == "Buprenorphine (oral)" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    lab_bupe_current = factor(
      case_when(
        nms_opd_med___1 == "Buprenorphine (implant / inj.)" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    methadone_current = factor(
      case_when(
        nms_opd_med___4 == "Methadone" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    naltrexone_current = factor(
      case_when(
        nms_opd_med___2 == "Naltrexone (oral)" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    other_oat_current = factor(
      case_when(
        nms_opd_med___6 == "Other" | nms_opd_med___2 == "Naltrexone (oral)" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    oat_current = factor(
      case_when(
        bupe_current == "Yes" | methadone_current == "Yes" | lab_bupe_current == "Yes" | other_oat_current == "Yes" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
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
        dem_gender == "Man" & srb_1m_m == "Yes" ~ "Yes",
        dem_gender == "Man" & srb_3m_m == "Yes" ~ "Yes",
        dem_gender == "Man" & (dem_gender_id == "Homosexual" | dem_gender_id == "Bisexual") ~ "Yes",
        dem_gender == "Man" & srb_1m_m == "No" ~ "No",
        dem_gender == "Man" & srb_3m_m == "No" ~ "No",
        dem_gender == "Man" & (dem_gender_id == "Heterosexual" | dem_gender_id == "Other") ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
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
        sdem_sev == "Syringe access program (SAP)" | sdem_sev == "Both" ~ "Yes",
        sdem_sev == "None" | sdem_sev == "Opioid agonist therapy (OAT) clinic" ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    )
  )

# injecting risks
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    syringe_share_6m_bin = factor(
      case_when(
        sdem_idu6m___1 == "Use a needle that you knew or suspected someone else had used before" |
        sdem_idu6m___3 == "Ever skip cleaning your needle with bleach or boiling it after you were done" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    syringe_loan_6m_bin = factor(
      case_when(
        (sdem_idu6m___4 == "Let someone else use a needle after you used it" | 
         sdem_idu6m___5 == "Let someone else use the rinse water, etc" | 
         sdem_idu6m___6 == "Allow someone else to inject with drugs") ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    syringe_cooker_6m_bin = factor(
      case_when(
         sdem_idu6m___2 == "Use someone else's rinse water, cooker, or cotton" ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    syringe_reuse_6m_bin = factor(
      case_when(
         sdem_idu6m___0 == "Reuse a needle without cleaning it with bleach or boiling water first" ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    syringe_share_bin_ever = factor(
      case_when(
        sdu_srg == "" ~ NA_character_,
        TRUE ~ sdu_srg
      ),
      levels = c("No", "Yes")
    ),
    overdose_6m = factor(
      case_when(
        odu_6m == "" ~ NA_character_,
        TRUE ~ odu_6m
      ),
      levels = c("No", "Yes")
    ),
    days_used_1m = case_when(
      sub_frq1m == "" ~ NA_real_,
      TRUE ~ as.numeric(sub_frq1m)
    ),
    days_used_1m_3cat = factor(
      case_when(
        days_used_1m > 24 ~ "25-30", 
        days_used_1m < 25 & days_used_1m > 14 ~ "15-24",
        days_used_1m < 15 ~ "0-14",
        TRUE ~ NA_character_
      ),
      levels = c("0-14", "15-24", "25-30")
    ),
    inject_stims_6m = factor(ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes" | sub_6m8 == "Yes" | sub_6m13 == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    inject_opioids_6m = factor(ifelse(sub_6m1 == "Yes" | sub_6m8 == "Yes" | sub_6m9 == "Yes" | sub_6m10 == "Yes" | sub_6m11 == "Yes" | sub_6m12 == "Yes" | sub_6m22 == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    inject_heroin_6m = factor(ifelse(sub_6m1 == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    inject_cocaine_6m = factor(ifelse(sub_6m3 == "Yes" | sub_6m4 == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    inject_fent_6m = factor(ifelse(sub_6m27 == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    inject_meth_6m = factor(ifelse(sub_6m13 == "Yes", "Yes", "No"), levels = c("No", "Yes"))
  )

# sexual risks
cols_to_na <- c(
  "srb_3m_prst", "srb_prst04", "srb_1m_f_hiv", "sdem_sex", "srb_3m", "srb_1m_fc_hiv", "srb_3m_sxp",
  "srb_1m_f_drg_cc", "srb_1m_f_drg_hro", "srb_1m_f_drg_main", "srb_1m_f_drg_aph", "srb_1m_f_drg_psy",
  "srb_3m_f", "srb_1m_fc_pv", "srb_1m_fc_pa", "srb_1m_fc_npv", "srb_1m_fc_npa", "srb_1m_fc_cv",
  "srb_1m_fc_ca", "srb_1m_fc_mcv", "srb_1m_fc_mco", "srb_1m_fc_mca", "cla_16a", "srb_3m_f_sxp_pidu",
  "srb_3m_f_sxp_npidu", "srb_3m_f_sxp_cidu", "srb_3m_f_sxp_mcidu", "srb_3m_m_sxp_pidu", "srb_3m_m_sxp_npidu",
  "srb_3m_m_sxp_cidu", "srb_3m_m_sxp_mcidu"
)
m2hepprep_prep_combined[cols_to_na] <- lapply(
  m2hepprep_prep_combined[cols_to_na],
  function(x) ifelse(x == "", NA, x)
)

m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    sexwork_3m = factor(
      case_when(
        srb_3m_prst == "Yes" ~ "Yes",
        srb_3m_prst == "No" ~ "No",
        srb_3m != "Yes" ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    bought_sex_3m = factor(
      case_when(
        srb_prst04 == "Yes" ~ "Yes",
        srb_prst04 == "No" ~ "No",
        srb_3m != "Yes" ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    num_sex_partners_3m = factor(
      case_when(
        as.numeric(srb_3m_sxp) == 0 ~ "0",
        as.numeric(srb_3m_sxp) == 1 ~ "1",
        as.numeric(srb_3m_sxp) > 1 ~ "2+",
        srb_3m != "Yes" ~ "0",
        is.na(srb_3m_sxp) ~ NA_character_
      ),
      levels = c("0", "1", "2+")
    ),
    sex_with_hivpos_female_1m = factor(
      case_when(
        sdem_sex == "Male" & srb_1m_f_hiv %in% c("No", "Yes", "Don't know") ~ srb_1m_f_hiv,
        sdem_sex == "Male" & srb_3m != "Yes" ~ "0",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes", "Don't know", "0")
    ),
    condom_use_hivpos_female_1m = factor(
      case_when(
        sdem_sex == "Male" & (is.na(srb_3m) | srb_3m == "No") ~ "0",
        sdem_sex == "Male" ~ as.character(srb_1m_fc_hiv),
        TRUE ~ NA_character_
      ),
      levels = c("Never", "Rarely", "Some of the time", "Very often", "Always", "0")
    ),
    sex_with_pwid_3m = factor(
      ifelse(
        rowSums(
          cbind(
            as.numeric(srb_3m_f_sxp_pidu) > 0,
            as.numeric(srb_3m_f_sxp_npidu) > 0,
            as.numeric(srb_3m_f_sxp_cidu) > 0,
            as.numeric(srb_3m_f_sxp_mcidu) > 0,
            as.numeric(srb_3m_m_sxp_pidu) > 0,
            as.numeric(srb_3m_m_sxp_npidu) > 0,
            as.numeric(srb_3m_m_sxp_cidu) > 0,
            as.numeric(srb_3m_m_sxp_mcidu) > 0
          ), na.rm = TRUE
        ) > 0, "Yes", "No"
      ),
      levels = c("No", "Yes")
    ),
    any_sex_3m = factor(srb_3m, levels = c("No", "Yes")),
    sex_on_cocaine_1m = factor(srb_1m_f_drg_cc, levels = c("Never", "Rarely", "Some of the time", "Very often", "Always")),
    sex_on_heroin_1m = factor(srb_1m_f_drg_hro, levels = c("Never", "Rarely", "Some of the time", "Very often", "Always")),
    sex_on_other_main_drug_1m = factor(srb_1m_f_drg_main, levels = c("Never", "Rarely", "Some of the time", "Very often", "Always")),
    sex_on_amphetamines_1m = factor(srb_1m_f_drg_aph, levels = c("Never", "Rarely", "Some of the time", "Very often", "Always")),
    sex_on_other_psychoactive_1m = factor(srb_1m_f_drg_psy, levels = c("Never", "Rarely", "Some of the time", "Very often", "Always")),
    sex_on_any_drug_1m = factor(
      case_when(
        any_sex_3m != "Yes" ~ "No sex",
        srb_1m_f_drg_cc %in% c("Rarely", "Some of the time", "Very often", "Always") |
        srb_1m_f_drg_hro %in% c("Rarely", "Some of the time", "Very often", "Always") |
        srb_1m_f_drg_main %in% c("Rarely", "Some of the time", "Very often", "Always") |
        srb_1m_f_drg_aph %in% c("Rarely", "Some of the time", "Very often", "Always") |
        srb_1m_f_drg_psy %in% c("Rarely", "Some of the time", "Very often", "Always") ~ "Sex on drugs",
        rowSums(
          cbind(
            srb_1m_f_drg_cc %in% c("Never", NA),
            srb_1m_f_drg_hro %in% c("Never", NA),
            srb_1m_f_drg_main %in% c("Never", NA),
            srb_1m_f_drg_aph %in% c("Never", NA),
            srb_1m_f_drg_psy %in% c("Never", NA)
          )
        ) == 5 ~ "Sex without drugs",
        TRUE ~ NA_character_
      ),
      levels = c("Sex without drugs", "Sex on drugs", "No sex")
    ),
    condom_1m = factor(
      case_when(
        srb_1m_mc_pv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_pa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_npv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_npa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_cv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_ca %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_mcv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_mco %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_mc_mca %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_pv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_pa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_npv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_npa %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_cv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_ca %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mcv %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mco %in% c("Never", "Rarely", "Some of the time") |
        srb_1m_fc_mca %in% c("Never", "Rarely", "Some of the time") ~ "Inconsistent condom use",
        srb_1m_mc_pv %in% c("Very often", "Always") |
        srb_1m_mc_pa %in% c("Very often", "Always") |
        srb_1m_mc_npv %in% c("Very often", "Always") |
        srb_1m_mc_npa %in% c("Very often", "Always") |
        srb_1m_mc_cv %in% c("Very often", "Always") |
        srb_1m_mc_ca %in% c("Very often", "Always") |
        srb_1m_mc_mcv %in% c("Very often", "Always") |
        srb_1m_mc_mco %in% c("Very often", "Always") |
        srb_1m_mc_mca %in% c("Very often", "Always") |
        srb_1m_fc_pv %in% c("Very often", "Always") |
        srb_1m_fc_pa %in% c("Very often", "Always") |
        srb_1m_fc_npv %in% c("Very often", "Always") |
        srb_1m_fc_npa %in% c("Very often", "Always") |
        srb_1m_fc_cv %in% c("Very often", "Always") |
        srb_1m_fc_ca %in% c("Very often", "Always") |
        srb_1m_fc_mcv %in% c("Very often", "Always") |
        srb_1m_fc_mco %in% c("Very often", "Always") |
        srb_1m_fc_mca %in% c("Very often", "Always") ~ "Consistent condom use",
        srb_3m != "Yes" ~ "No sex past 3 months",
        srb_1m_m == "No" | srb_1m_f == "No" ~ "No sex past month",
        srb_1m_mc_pv == "Not Applicable" | srb_1m_mc_pa == "Not Applicable" |
        srb_1m_mc_npa == "Not Applicable" | srb_1m_mc_cv == "Not Applicable" |
        srb_1m_mc_mcv == "Not Applicable" | srb_1m_mc_mco == "Not Applicable" |
        srb_1m_fc_pv == "Not Applicable" | srb_1m_fc_pa == "Not Applicable" |
        srb_1m_fc_npa == "Not Applicable" | srb_1m_fc_cv == "Not Applicable" |
        srb_1m_fc_mcv == "Not Applicable" | srb_1m_fc_mco == "Not Applicable" ~ NA_character_,
        TRUE ~ NA_character_
      ),
      levels = c("Consistent condom use", "Inconsistent condom use", "No sex past month", "No sex past 3 months")
    ),
    sex_work_ever = factor(
      case_when(
        cla_16a == "No" ~ "No",
        cla_16a == "Yes" ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    )
  )

# save data
write.csv(m2hepprep_prep_combined, "data/m2hepprep_combined.csv", row.names = FALSE)
