# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load raw data
m2hepprep_raw <- read.csv("data/cleaned_m2_data_05012025 (1).csv")
View(m2hepprep_raw)

# baseline data
m2hepprep_baseline <- m2hepprep_raw %>%
  filter(redcap_event_name == "Screening (visit 1)") %>%
  select(record_id, id_paper, sdem_visit, sdem_reside, sdem_lang_mia_2, sdem_age, sdem_oat, sdem_sev, sdem_sex, sdem_gender, sdem_prg_c, vcp_inject_6mo, sdem_hiv_etst, sdem_hiv_rtst_r, sdem_prp_cu, sdem_hcv, sdem_wil_fol, sdem_elig, sdem_hcv_etst, sdem_hcv_rtst, sdem_hcv_rtst_r, sdem_hiv_rtst, scr_c_hcv_res_retired, insti, sdem_slep6m, sdem_live6m_hls, sdem_live6m_shl1, sdem_live6m_trs, sdem_live6m_htl, sdem_live6m_hiv, sdem_live6m_sut, sdem_live6m_shl2, sdem_live6m_shl3, sdem_live6m_shl4, sdem_live6m_shl5, sdem_idu, sdem_idu6m___0, sdem_idu6m___1, sdem_idu6m___2, sdem_idu6m___3, sdem_idu6m___4, sdem_idu6m___5, sdem_idu6m___6, sdem_idu6m___7, sdem_dis_hcv, sdem_dis_hiv, sdem_dis_sex, sdem_dis_gay, sdem_dis_sub, sdem_dis_race, vir_dbs, vir_rna2)

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

# treatment initiation
m2hepprep_treat <- m2hepprep_raw %>%
    filter(redcap_repeat_instrument == "treatment_initiation")
    
# treatment arm
m2hepprep_arm <- m2hepprep_raw %>%
  filter(redcap_repeat_instrument == "serious_adverse_event_form") %>%
  group_by(record_id) %>%
  mutate(record_id_seq = row_number()) %>%
  filter(record_id_seq == 1) %>%
  ungroup() %>%
  select(record_id, sae_txarm)

View(m2hepprep_arm)

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
  left_join(m2hepprep_15m, by = "record_id")

View(m2hepprep_combined)

# Create prep_prescribe_any variable
m2hepprep_combined <- m2hepprep_combined %>%
  mutate(prep_prescribe_any = pmax(prep_prescribe_3m, prep_prescribe_6m, prep_prescribe_9m, prep_prescribe_12m, na.rm = TRUE))

# Basic frequency table

# Define variables for the table
baseline_vars <- c("sdem_age", "sdem_oat", "sdem_sev", "sdem_sex", "sdem_gender", 
                   "sdem_prg_c", "vcp_inject_6mo", "sdem_hiv_etst", "sdem_hiv_rtst_r", 
                   "sdem_prp_cu", "sdem_hcv", "sdem_slep6m", "sdem_live6m_hls", 
                   "sdem_live6m_shl1", "sdem_live6m_trs", "sdem_live6m_htl", 
                   "sdem_live6m_hiv", "sdem_live6m_sut", "sdem_live6m_shl2", 
                   "sdem_live6m_shl3", "sdem_live6m_shl4", "sdem_live6m_shl5", 
                   "sdem_idu", "sdem_idu6m___0", "sdem_idu6m___1", "sdem_idu6m___2", 
                   "sdem_idu6m___3", "sdem_idu6m___4", "sdem_idu6m___5", "sdem_idu6m___6", 
                   "sdem_idu6m___7", "sdem_dis_hcv", "sdem_dis_hiv", "sdem_dis_sex", 
                   "sdem_dis_gay", "sdem_dis_sub", "sdem_dis_race")

# Create the table with row percentages
baseline_table <- CreateTableOne(vars = baseline_vars, 
                                strata = "prep_prescribe_any", 
                                data = m2hepprep_combined,
                                test = TRUE)


# Print with row percentages instead of column percentages
print(baseline_table, showAllLevels = TRUE, formatOptions = list(percent = "row"))

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "baseline_table.csv")