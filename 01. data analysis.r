# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_prep_combined.csv")
m2hepprep_prep_combined_montreal <- read.csv("data/m2hepprep_prep_combined_montreal.csv")
m2hepprep_prep_combined_miami <- read.csv("data/m2hepprep_prep_combined_miami.csv")

# Function to set reference levels for all datasets
set_factor_levels <- function(data) {
  data$education_status_4cat <- factor(data$education_status_4cat, 
                                       levels = c("Middle school or less", "High school diploma", "College graduate or higher", "No answer"))
  
  data$income_4cat <- factor(data$income_4cat, 
                            levels = c("No income", "<500", "500-1500", ">1500"))
  
  data$employment_current <- factor(data$employment_current, 
                                   levels = c("Employed", unique(data$employment_current)[!unique(data$employment_current) %in% c("Employed", NA)]))
  
  data$sdem_slep6m_binary <- factor(data$sdem_slep6m_binary, 
                                   levels = c("Homeless", "Not homeless"))
  
  data$incarc_6m_bin <- factor(data$incarc_6m_bin, 
                              levels = c("No", "Yes"))
  
  data$hr_use <- factor(data$hr_use, 
                       levels = c("Syringe access program (SAP)", "Opioid agonist therapy (OAT) clinic", "Both", "None"))
  
  return(data)
}

# Apply factor levels to all datasets
m2hepprep_prep_combined <- set_factor_levels(m2hepprep_prep_combined)
m2hepprep_prep_combined_montreal <- set_factor_levels(m2hepprep_prep_combined_montreal)
m2hepprep_prep_combined_miami <- set_factor_levels(m2hepprep_prep_combined_miami)

# Frequency tables

# Create substance use summary table
substance_vars <- c("sub_6m1", "sub_6m2", "sub_6m3", "sub_6m4", "sub_6m5", "sub_6m6", "sub_6m7", "sub_6m8", "sub_6m9", "sub_6m10", "sub_6m11", "sub_6m12", "sub_6m13", "sub_6m14", "sub_6m15", "sub_6m16", "sub_6m17", "sub_6m18", "sub_6m19", "sub_6m20", "sub_6m21", "sub_6m22", "sub_6m23", "sub_6m24", "sub_6m25", "sub_6m26", "sub_6m27")

# Create summary table
substance_table <- CreateTableOne(vars = substance_vars, 
                                 data = m2hepprep_prep_combined,
                                 includeNA = TRUE)

# Convert table to data frame and save
substance_table_df <- print(substance_table, showAllLevels = TRUE, printToggle = FALSE)
write.csv(substance_table_df, "data/substance_use_summary.csv")

# Print the table
print(substance_table)

# If you want to stratify by site (Montreal vs Miami)
substance_table_stratified <- CreateTableOne(vars = substance_vars, 
                                            strata = "sdem_reside",
                                            data = m2hepprep_prep_combined,
                                            test = TRUE,
                                            addOverall = TRUE,
                                            includeNA = TRUE)

# Convert stratified table to data frame and save
substance_table_strat_df <- print(substance_table_stratified, showAllLevels = TRUE, printToggle = FALSE)
write.csv(substance_table_strat_df, "data/substance_use_summary_by_site.csv")

# variables for tables
table1_vars <- c("sdem_sex_binary", "sdem_age", "sdem_age_binary", "education_status_4cat", "income_4cat", "employment_current", "sdem_slep6m_binary", "incarc_6m_bin")
table2_vars <- c("healthcare_disc_bin", "sdem_dis_sub_bin", "aiv_kid_evr_pa", "aiv_kid_evr_sex", "aiv_adt_evr_pa", "aiv_6m_pa", "aiv_adt_evr_sex", "aiv_6m_sex")
table3_vars <- c("hr_use", "oat_ever", "oral_bupe", "lab_bupe", "naltrexone", "methadone", "other_oat", "mental_health_prescribe_ever", "therapy_ever")
table4_vars <- c("syringe_share_6m_bin", "syringe_loan_bin", "syringe_other_bin", "syringe_share_bin_ever", "days_used_1m_3cat", "overdose_6m", "sex_work_ever", "sexwmen_1m")

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
    
    # Check if variable has sufficient levels
    levels_count <- length(unique(data[[var]][!is.na(data[[var]])]))
    if(levels_count < 2) {
      cat("Skipping", var, "- insufficient variation (only", levels_count, "level)\n")
      next
    }
    
    # Create formula
    formula <- as.formula(paste(outcome, "~", var))
    
    # Run model with error handling
    tryCatch({
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
    }, error = function(e) {
      cat("Error with variable", var, ":", e$message, "\n")
    })
  }
  
  # Format results
  if(nrow(logistic_results) > 0) {
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
  } else {
    cat("No valid results for", table_name, "\n")
  }
  
  return(logistic_results)
}

# Run for all tables with prep_init as outcome
table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined, "prep_init", "table1")
table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined, "prep_init", "table2")
table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined, "prep_init", "table3")
table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined, "prep_init", "table4")

# run for montreal data
table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined_montreal, "prep_init", "table1_montreal")
table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined_montreal, "prep_init", "table2_montreal")
table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined_montreal, "prep_init", "table3_montreal")
table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined_montreal, "prep_init", "table4_montreal")

# run for miami data
table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined_miami, "prep_init", "table1_miami")
table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined_miami, "prep_init", "table2_miami")
table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined_miami, "prep_init", "table3_miami")
table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined_miami, "prep_init", "table4_miami")
