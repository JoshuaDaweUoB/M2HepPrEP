# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_prep_combined.csv")

# Set reference levels 
m2hepprep_prep_combined$education_status_4cat <- factor(m2hepprep_prep_combined$education_status_4cat, 
                                                        levels = c("Middle school or less", "High school diploma", "College graduate or higher", "No answer"))

m2hepprep_prep_combined$income_4cat <- factor(m2hepprep_prep_combined$income_4cat, 
                                             levels = c("No income", "<500", "500-1500", ">1500"))

m2hepprep_prep_combined$employment_current <- factor(m2hepprep_prep_combined$employment_current, 
                                                    levels = c("Employed", unique(m2hepprep_prep_combined$employment_current)[!unique(m2hepprep_prep_combined$employment_current) %in% c("Employed", NA)]))

m2hepprep_prep_combined$sdem_slep6m_binary <- factor(m2hepprep_prep_combined$sdem_slep6m_binary, 
                                                    levels = c("Homeless", "Not homeless"))

m2hepprep_prep_combined$incarc_6m_bin <- factor(m2hepprep_prep_combined$incarc_6m_bin, 
                                               levels = c("No", "Yes"))

m2hepprep_prep_combined$hr_use <- factor(m2hepprep_prep_combined$hr_use, 
                                        levels = c("Syringe access program (SAP)", "Opioid agonist therapy (OAT) clinic", "Both", "None"))

# Frequency tables

# variables for tables
table1_vars <- c("sdem_sex_binary", "sdem_age", "sdem_age_binary", "education_status_4cat", "income_4cat", "employment_current", "sdem_slep6m_binary", "incarc_6m_bin")
table2_vars <- c("healthcare_disc_bin", "sdem_dis_sub_bin", "aiv_kid_evr_pa", "aiv_kid_evr_sex", "aiv_adt_evr_pa", "aiv_6m_pa", "aiv_adt_evr_sex", "aiv_6m_sex")
table3_vars <- c("hr_use", "oat_ever", "oral_bupe", "lab_bupe", "naltrexone", "methadone", "other_oat", "mental_health_prescribe_ever", "therapy_ever")
table4_vars <- c("syringe_share_bin", "syringe_loan_bin", "syringe_other_bin", "syringe_share_bin_ever", "days_used_1m_3cat", "overdose_6m", "sex_work_ever", "sexwmen_1m")

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

# create table 4
baseline_table <- CreateTableOne(vars = table4_vars, 
                                strata = "sdem_reside", 
                                data = m2hepprep_prep_combined,
                                test = TRUE,
                                addOverall = TRUE,
                                includeNA = FALSE)

# Convert table to data frame with row percentages and save to Excel
table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
                  formatOptions = list(percent = "row"))
write.csv(table_df, "data/table4_vars.csv")

# Create binary version of sdem_reside for logistic regression
m2hepprep_prep_combined$sdem_reside_binary <- ifelse(m2hepprep_prep_combined$sdem_reside == "Greater Montreal area", 1, 0)

# Function to run logistic regression for a set of variables
run_logistic_regression <- function(vars, data, outcome = "sdem_reside_binary", table_name) {
  
  # Initialize empty dataframe to store results
  logistic_results <- data.frame()
  
  # Run logistic regression for each variable
  for(var in vars) {
    
    # Create formula
    formula <- as.formula(paste(outcome, "~", var))
    
    # Run model
    model <- glm(formula, data = data, family = binomial(link = "logit"))
    
    # Extract results with OR and CI
    results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
    # Add variable name and clean up
    results$variable <- var
    results <- results %>%
      filter(term != "(Intercept)") %>%  # Remove intercept
      select(variable, term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, 
             CI_lower = conf.low, 
             CI_upper = conf.high)
    
    # Combine with previous results
    logistic_results <- rbind(logistic_results, results)
  }
  
  # Format results
  logistic_results <- logistic_results %>%
    mutate(
      OR_CI_formatted = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
      p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(variable, term, OR_CI_formatted, p_formatted) %>%
    rename(Variable = variable,
           Level = term,
           `OR (95% CI)` = OR_CI_formatted,
           `P-value` = p_formatted)
  
  # Save to CSV
  filename <- paste0("data/", table_name, "_logistic_regression.csv")
  write.csv(logistic_results, filename, row.names = FALSE)
  
  # Print results
  print(paste("Results for", table_name))
  print(logistic_results)
  
  return(logistic_results)
}

# Run for all tables
table1_results <- run_logistic_regression(table1_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table1")
table2_results <- run_logistic_regression(table2_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table2")
table3_results <- run_logistic_regression(table3_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table3")
table4_results <- run_logistic_regression(table4_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table4")

# Function to run logistic regression with prep_init as outcome
run_prep_logistic_regression <- function(vars, data, outcome = "prep_init", table_name) {
  
  # Initialize empty dataframe to store results
  logistic_results <- data.frame()
  
  # Run logistic regression for each variable
  for(var in vars) {
    
    # Create formula
    formula <- as.formula(paste(outcome, "~", var))
    
    # Run model
    model <- glm(formula, data = data, family = binomial(link = "logit"))
    
    # Extract results with OR and CI
    results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
    # Add variable name and clean up
    results$variable <- var
    results <- results %>%
      filter(term != "(Intercept)") %>%  # Remove intercept
      select(variable, term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, 
             CI_lower = conf.low, 
             CI_upper = conf.high)
    
    # Combine with previous results
    logistic_results <- rbind(logistic_results, results)
  }
  
  # Format results
  logistic_results <- logistic_results %>%
    mutate(
      OR_CI_formatted = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
      p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(variable, term, OR_CI_formatted, p_formatted) %>%
    rename(Variable = variable,
           Level = term,
           `OR (95% CI)` = OR_CI_formatted,
           `P-value` = p_formatted)
  
  # Save to CSV
  filename <- paste0("data/", table_name, "_prep_logistic_regression.csv")
  write.csv(logistic_results, filename, row.names = FALSE)
  
  # Print results
  print(paste("PrEP initiation results for", table_name))
  print(logistic_results)
  
  return(logistic_results)
}

# Run for all tables with prep_init as outcome
table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined, "prep_init", "table1")
table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined, "prep_init", "table2")
table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined, "prep_init", "table3")
table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined, "prep_init", "table4")


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