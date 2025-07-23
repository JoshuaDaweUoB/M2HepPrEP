# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_combined <- read.csv("data/m2hepprep_combined.csv")
m2hepprep_prep_combined <- read.csv("data/m2hepprep_prep_combined.csv")

# Basic frequency table

# Define variables for the table
baseline_vars <- c("rand_arm", "sdem_reside", "sdem_age", "sdem_oat", "sdem_sev", "sdem_sex", "sdem_gender", 
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
                                strata = "prep_init", 
                                data = m2hepprep_prep_combined,
                                test = TRUE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/baseline_table.csv")

# prep initiation as outcome
prep_init_model <- glm(prep_init ~ rand_arm + sdem_reside, 
                     data = m2hepprep_prep_combined, 
                     family = binomial(link = "logit"))

# model output
prep_init_model_results <- tidy(prep_init_model, exponentiate = TRUE, conf.int = TRUE)
print(prep_init_model_results)
