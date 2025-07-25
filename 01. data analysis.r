# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_prep_combined.csv")

# Frequency tables

# variables for tables
table1_vars <- c("sdem_sex_binary", "sdem_age", "sdem_age_binary", "education_status_4cat", "income_4cat", "employment_current", "sdem_slep6m_binary", "incarc_6m_bin")
table2_vars <- c("sdem_dis_sub_bin", "healthcare_disc_bin", "aiv_kid_evr_pa", "aiv_adt_evr_pa", "aiv_6m_pa", "aiv_kid_evr_sex", "aiv_adt_evr_sex", "aiv_6m_sex")
table3_vars <- c("hr_use", "oat_ever", "oral_bupe", "lab_bupe", "naltrexone", "methadone", "other_oat", "mental_health_prescribe_ever", "therapy_ever")
#table4_vars <- c("syringe_share_bin", "syringe_loan_bin", "syringe_other_bin",
#baseline_vars <- c( , "sex_work_ever", "oat_ever", "mental_health_prescribe_ever", "therapy_ever", "sexwmen_1m")

# create table 1
baseline_table <- CreateTableOne(vars = table1_vars, 
                                strata = "sdem_reside", 
                                data = m2hepprep_prep_combined,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/table1_vars.csv")

# create table 2
baseline_table <- CreateTableOne(vars = table2_vars, 
                                strata = "sdem_reside", 
                                data = m2hepprep_prep_combined,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/table2_vars.csv")

# create table 3
baseline_table <- CreateTableOne(vars = table3_vars, 
                                strata = "sdem_reside", 
                                data = m2hepprep_prep_combined,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/table3_vars.csv")









# Create the table with row percentages and Total column
baseline_table <- CreateTableOne(vars = baseline_vars, 
                                strata = "prep_init", 
                                data = m2hepprep_prep_combined,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/baseline_table_prep.csv")

# Create the table with row percentages and Total column
baseline_table <- CreateTableOne(vars = baseline_vars, 
                                strata = "prep_init", 
                                data = m2hepprep_prep_combined_montreal,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/baseline_table_prep_montreal.csv")

# Create the table with row percentages and Total column
baseline_table <- CreateTableOne(vars = baseline_vars, 
                                strata = "prep_init", 
                                data = m2hepprep_prep_combined_miami,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/baseline_table_prep_miami.csv")

# prep initiation as outcome
prep_init_model <- glm(prep_init ~ rand_arm + sdem_reside, 
                     data = m2hepprep_prep_combined, 
                     family = binomial(link = "logit"))

# model output
prep_init_model_results <- tidy(prep_init_model, exponentiate = TRUE, conf.int = TRUE)
print(prep_init_model_results)

# prep initiation as outcome
prep_init_model2 <- glm(prep_init ~ sdem_reside + sdem_sex_binary + sdem_age_binary + sdem_slep6m_binary, 
                     data = m2hepprep_prep_combined, 
                     family = binomial(link = "logit"))

# model output
prep_init_model_results2 <- tidy(prep_init_model2, exponentiate = TRUE, conf.int = TRUE)
print(prep_init_model_results2)

# prep initiation as outcome stratified by city
prep_init_model2 <- glm(prep_init ~ sdem_reside + sdem_sex_binary + sdem_age_binary + sdem_slep6m_binary + sdem_dis_sub, 
                     data = m2hepprep_prep_combined, 
                     family = binomial(link = "logit"))

# model output
prep_init_model_results2 <- tidy(prep_init_model2, exponentiate = TRUE, conf.int = TRUE)
print(prep_init_model_results2)

# Run separate models by city
cities <- unique(m2hepprep_prep_combined$insti)

for(city in cities) {
  cat("\n=== Results for", city, "===\n")
  
  # Filter data for this city
  city_data <- m2hepprep_prep_combined %>% filter(insti == city)
  
  # Run model
  city_model <- glm(prep_init ~ sdem_reside + sdem_sex_binary + sdem_age_binary + sdem_slep6m_binary, 
                   data = city_data, 
                   family = binomial(link = "logit"))
  
  # Get results
  city_results <- tidy(city_model, exponentiate = TRUE, conf.int = TRUE)
  print(city_results)
}