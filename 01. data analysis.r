# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf)

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

# make prep_init a factor
m2hepprep_prep_combined$prep_init[m2hepprep_prep_combined$prep_init == 2] <- 1
m2hepprep_prep_combined$prep_init <- factor(m2hepprep_prep_combined$prep_init, levels = c(0, 1), labels = c("No", "Yes"))
m2hepprep_prep_combined_montreal$prep_init <- factor(m2hepprep_prep_combined_montreal$prep_init, levels = c(0, 1), labels = c("No", "Yes"))
m2hepprep_prep_combined_miami$prep_init <- factor(m2hepprep_prep_combined_miami$prep_init, levels = c(0, 1), labels = c("No", "Yes"))

# # Frequency tables

# # Create substance use summary table
# substance_vars <- c("sub_6m1", "sub_6m2", "sub_6m3", "sub_6m4", "sub_6m5", "sub_6m6", "sub_6m7", "sub_6m8", "sub_6m9", "sub_6m10", "sub_6m11", "sub_6m12", "sub_6m13", "sub_6m14", "sub_6m15", "sub_6m16", "sub_6m17", "sub_6m18", "sub_6m19", "sub_6m20", "sub_6m21", "sub_6m22", "sub_6m23", "sub_6m24", "sub_6m25", "sub_6m26", "sub_6m27")

# # Create summary table
# substance_table <- CreateTableOne(vars = substance_vars, 
#                                  data = m2hepprep_prep_combined,
#                                  includeNA = TRUE)

# # Convert table to data frame and save
# substance_table_df <- print(substance_table, showAllLevels = TRUE, printToggle = FALSE)
# write.csv(substance_table_df, "data/substance_use_summary.csv")

# # Print the table
# print(substance_table)

# # stratify by site
# substance_table_stratified <- CreateTableOne(vars = substance_vars, 
#                                             strata = "sdem_reside",
#                                             data = m2hepprep_prep_combined,
#                                             test = TRUE,
#                                             addOverall = TRUE,
#                                             includeNA = TRUE)

# # Convert stratified table to data frame and save
# substance_table_strat_df <- print(substance_table_stratified, showAllLevels = TRUE, printToggle = FALSE)
# write.csv(substance_table_strat_df, "data/substance_use_summary_by_site.csv")

# # variables for tables
# table1_vars <- c("sdem_sex_binary", "sdem_age", "sdem_age_binary", "education_status_4cat", "income_4cat", "employment_current", "sdem_slep6m_binary", "incarc_6m_bin")
# table2_vars <- c("arch_bin", "healthcare_disc_bin", "sdem_dis_sub_bin", "aiv_kid_evr_pa", "aiv_kid_evr_sex", "aiv_adt_evr_pa", "aiv_6m_pa", "aiv_adt_evr_sex", "aiv_6m_sex")
# table3_vars <- c("hr_use", "oat_ever", "oral_bupe", "lab_bupe", "naltrexone", "methadone", "other_oat", "mental_health_prescribe_ever", "therapy_ever")
# table4_vars <- c("syringe_share_6m_bin", "syringe_loan_bin", "syringe_other_bin", "syringe_share_bin_ever", "days_used_1m_3cat", "overdose_6m", "sex_work_ever", "sexwmen_1m")

# # create table 1
# baseline_table <- CreateTableOne(vars = table1_vars, 
#                                 strata = "sdem_reside", 
#                                 data = m2hepprep_prep_combined,
#                                 test = TRUE,
#                                 addOverall = TRUE,
#                                 includeNA = FALSE)

# # Convert table to data frame with row percentages and save to Excel
# table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
#                   formatOptions = list(percent = "row"))
# write.csv(table_df, "data/table1_vars.csv")

# # create table 2
# baseline_table <- CreateTableOne(vars = table2_vars, 
#                                 strata = "sdem_reside", 
#                                 data = m2hepprep_prep_combined,
#                                 test = TRUE,
#                                 addOverall = TRUE,
#                                 includeNA = FALSE)

# # Convert table to data frame with row percentages and save to Excel
# table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
#                   formatOptions = list(percent = "row"))
# write.csv(table_df, "data/table2_vars.csv")

# # create table 3
# baseline_table <- CreateTableOne(vars = table3_vars, 
#                                 strata = "sdem_reside", 
#                                 data = m2hepprep_prep_combined,
#                                 test = TRUE,
#                                 addOverall = TRUE,
#                                 includeNA = FALSE)

# # Convert table to data frame with row percentages and save to Excel
# table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
#                   formatOptions = list(percent = "row"))
# write.csv(table_df, "data/table3_vars.csv")

# # create table 4
# baseline_table <- CreateTableOne(vars = table4_vars, 
#                                 strata = "sdem_reside", 
#                                 data = m2hepprep_prep_combined,
#                                 test = TRUE,
#                                 addOverall = TRUE,
#                                 includeNA = FALSE)

# # Convert table to data frame with row percentages and save to Excel
# table_df <- print(baseline_table, showAllLevels = TRUE, printToggle = FALSE, 
#                   formatOptions = list(percent = "row"))
# write.csv(table_df, "data/table4_vars.csv")

# Create binary version of sdem_reside for logistic regression
m2hepprep_prep_combined$sdem_reside_binary <- ifelse(m2hepprep_prep_combined$sdem_reside == "Greater Montreal area", 1, 0)

# # Function to run logistic regression for a set of variables
# run_logistic_regression <- function(vars, data, outcome = "sdem_reside_binary", table_name) {
  
#   # Initialize empty dataframe to store results
#   logistic_results <- data.frame()
  
#   # Run logistic regression for each variable
#   for(var in vars) {
    
#     # Create formula
#     formula <- as.formula(paste(outcome, "~", var))
    
#     # Run model
#     model <- glm(formula, data = data, family = binomial(link = "logit"))
    
#     # Extract results with OR and CI
#     results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
#     # Add variable name and clean up
#     results$variable <- var
#     results <- results %>%
#       filter(term != "(Intercept)") %>%  # Remove intercept
#       dplyr::select(variable, term, estimate, conf.low, conf.high, p.value) %>%
#          rename(OR = estimate, 
#          CI_lower = conf.low, 
#          CI_upper = conf.high)
    
#     # Combine with previous results
#     logistic_results <- rbind(logistic_results, results)
#   }
  
#   # Format results
#     logistic_results <- logistic_results %>%
#       mutate(
#         OR_CI_formatted = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
#         p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
#       ) %>%
#       dplyr::select(variable, term, OR_CI_formatted, p_formatted) %>%
#       rename(Variable = variable,
#              Level = term,
#              `OR (95% CI)` = OR_CI_formatted,
#              `P-value` = p_formatted)
  
#   # Save to CSV
#   filename <- paste0("data/", table_name, "_logistic_regression.csv")
#   write.csv(logistic_results, filename, row.names = FALSE)
  
#   # Print results
#   print(paste("Results for", table_name))
#   print(logistic_results)
  
#   return(logistic_results)
# }

# # Run for all tables
# table1_results <- run_logistic_regression(table1_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table1")
# table2_results <- run_logistic_regression(table2_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table2")
# table3_results <- run_logistic_regression(table3_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table3")
# table4_results <- run_logistic_regression(table4_vars, m2hepprep_prep_combined, "sdem_reside_binary", "table4")

# # Function to run logistic regression with prep_init as outcome
# run_prep_logistic_regression <- function(vars, data, outcome = "prep_init", table_name) {
  
#   # Initialize empty dataframe to store results
#   logistic_results <- data.frame()
  
#   # Run logistic regression for each variable
#   for(var in vars) {
    
#     # Check if variable has sufficient levels
#     levels_count <- length(unique(data[[var]][!is.na(data[[var]])]))
#     if(levels_count < 2) {
#       cat("Skipping", var, "- insufficient variation (only", levels_count, "level)\n")
#       next
#     }
    
#     # Create formula
#     formula <- as.formula(paste(outcome, "~", var))
    
#     # Run model with error handling
#     tryCatch({
#       model <- glm(formula, data = data, family = binomial(link = "logit"))
      
#       # Extract results with OR and CI
#       results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
      
#       # Add variable name and clean up
#       results$variable <- var
#         results <- results %>%
#         filter(term != "(Intercept)") %>%  # Remove intercept
#         dplyr::select(variable, term, estimate, conf.low, conf.high, p.value) %>%
#         rename(OR = estimate,
#              CI_lower = conf.low, 
#              CI_upper = conf.high)
      
#       # Combine with previous results
#       logistic_results <- rbind(logistic_results, results)
#     }, error = function(e) {
#       cat("Error with variable", var, ":", e$message, "\n")
#     })
#   }
  
#   # Format results
# if(nrow(logistic_results) > 0) {
#   logistic_results <- logistic_results %>%
#     mutate(
#       OR_CI_formatted = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
#       p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
#     ) %>%
#     dplyr::select(variable, term, OR_CI_formatted, p_formatted) %>%
#     rename(Variable = variable,
#            Level = term,
#            `OR (95% CI)` = OR_CI_formatted,
#            `P-value` = p_formatted)
    
#     # Save to CSV
#     filename <- paste0("data/", table_name, "_prep_logistic_regression.csv")
#     write.csv(logistic_results, filename, row.names = FALSE)
    
#     # Print results
#     print(paste("PrEP initiation results for", table_name))
#     print(logistic_results)
#   } else {
#     cat("No valid results for", table_name, "\n")
#   }
  
#   return(logistic_results)
# }

# # Run for all tables with prep_init as outcome
# table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined, "prep_init", "table1")
# table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined, "prep_init", "table2")
# table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined, "prep_init", "table3")
# table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined, "prep_init", "table4")

# # run for montreal data
# table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined_montreal, "prep_init", "table1_montreal")
# table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined_montreal, "prep_init", "table2_montreal")
# table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined_montreal, "prep_init", "table3_montreal")
# table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined_montreal, "prep_init", "table4_montreal")

# # run for miami data
# table1_prep_results <- run_prep_logistic_regression(table1_vars, m2hepprep_prep_combined_miami, "prep_init", "table1_miami")
# table2_prep_results <- run_prep_logistic_regression(table2_vars, m2hepprep_prep_combined_miami, "prep_init", "table2_miami")
# table3_prep_results <- run_prep_logistic_regression(table3_vars, m2hepprep_prep_combined_miami, "prep_init", "table3_miami")
# table4_prep_results <- run_prep_logistic_regression(table4_vars, m2hepprep_prep_combined_miami, "prep_init", "table4_miami")

# arch_bin characteristics
library(tidyr)
library(dplyr)

# Specify arch_vars
arch_vars <- c(
  "arch_age", "arch_oat", "subscore_opioids", "subscore_stimulants", "subscore_cooker", "subscore_sharing", "subscore_gallery"
)

# Ensure arch_total is numeric and others are factors
m2hepprep_prep_combined$arch_total <- as.numeric(m2hepprep_prep_combined$arch_total)
factor_vars <- setdiff(arch_vars, "arch_total")
m2hepprep_prep_combined[factor_vars] <- lapply(m2hepprep_prep_combined[factor_vars], as.factor)

# Overall categorical summary for other arch_vars
arch_cat_summary_overall <- do.call(rbind, lapply(factor_vars, function(var) {
  tab <- table(m2hepprep_prep_combined[[var]], useNA = "ifany")
  percent <- round(100 * tab / sum(tab), 1)
  data.frame(
    Variable = var,
    Level = names(tab),
    Overall_Count = as.numeric(tab),
    Overall_Percent = as.numeric(percent)
  )
}))

# Stratified categorical summary for other arch_vars by city (long format)
arch_cat_summary_by_city_long <- do.call(rbind, lapply(factor_vars, function(var) {
  tab <- table(m2hepprep_prep_combined[[var]], m2hepprep_prep_combined$sdem_reside, useNA = "ifany")
  res <- as.data.frame(tab)
  res$Percent <- round(100 * res$Freq / tapply(res$Freq, res$Var2, sum)[res$Var2], 1)
  res$Variable <- var
  res[, c("Variable", "Var1", "Var2", "Freq", "Percent")]
}))
names(arch_cat_summary_by_city_long)[2:3] <- c("Level", "City")
names(arch_cat_summary_by_city_long)[4] <- "Count"

# Pivot to wide format: separate columns for each city
arch_cat_summary_by_city_wide <- arch_cat_summary_by_city_long %>%
  pivot_wider(
    id_cols = c(Variable, Level),
    names_from = City,
    values_from = c(Count, Percent),
    values_fill = 0
  )

# Concatenate count and percent for each city and overall
city_names <- unique(arch_cat_summary_by_city_long$City)
for(city in city_names) {
  count_col <- paste0("Count_", city)
  percent_col <- paste0("Percent_", city)
  new_col <- paste0(city, "_CountPercent")
  arch_cat_summary_by_city_wide[[new_col]] <- paste0(
    arch_cat_summary_by_city_wide[[count_col]], " (", arch_cat_summary_by_city_wide[[percent_col]], "%)"
  )
}

# Merge overall and city-wide summaries
arch_cat_summary_combined <- arch_cat_summary_by_city_wide %>%
  left_join(arch_cat_summary_overall %>%
              mutate(Overall_CountPercent = paste0(Overall_Count, " (", Overall_Percent, "%)")) %>%
              select(Variable, Level, Overall_CountPercent),
            by = c("Variable", "Level"))

# Arrange columns: Variable, Level, Overall, Montreal, Miami, etc.
final_cols <- c("Variable", "Level", "Overall_CountPercent", paste0(city_names, "_CountPercent"))
arch_cat_summary_combined <- arch_cat_summary_combined[, final_cols]

# Add an empty row between each variable in the combined summary
arch_cat_summary_combined_with_space <- arch_cat_summary_combined %>%
  group_by(Variable) %>%
  do(bind_rows(., tibble(Variable = "", Level = "", Overall_CountPercent = "", !!!setNames(rep("", length(city_names)), paste0(city_names, "_CountPercent"))))) %>%
  ungroup()

# Save with empty rows
write.csv(arch_cat_summary_combined_with_space, "data/arch_categorical_summary_combined.csv", row.names = FALSE)

# Ensure arch_total is numeric
m2hepprep_prep_combined$arch_total <- as.numeric(m2hepprep_prep_combined$arch_total)

# Overall mean and SD
overall_mean <- mean(m2hepprep_prep_combined$arch_total, na.rm = TRUE)
overall_sd <- sd(m2hepprep_prep_combined$arch_total, na.rm = TRUE)

# By city mean and SD
arch_total_by_city <- m2hepprep_prep_combined %>%
  group_by(sdem_reside) %>%
  summarise(
    Mean = mean(arch_total, na.rm = TRUE),
    SD = sd(arch_total, na.rm = TRUE)
  ) %>%
  rename(City = sdem_reside)

# Combine overall and city results
arch_total_summary <- bind_rows(
  tibble(City = "Overall", Mean = overall_mean, SD = overall_sd),
  arch_total_by_city
)

# Save to CSV
write.csv(arch_total_summary, "data/arch_total_score_mean_sd_by_city.csv", row.names = FALSE)

## associations with prep initiation

# Run logistic regression for each arch_var predicting prep_init
arch_regression_results <- lapply(arch_vars, function(var) {
  # Only run if variable has at least 2 levels
  if(length(unique(m2hepprep_prep_combined[[var]][!is.na(m2hepprep_prep_combined[[var]])])) < 2) return(NULL)
  formula <- as.formula(paste("prep_init ~", var))
  model <- glm(formula, data = m2hepprep_prep_combined, family = binomial(link = "logit"))
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  results$Variable <- var
  results
})

# Combine results and format
arch_regression_results_df <- bind_rows(arch_regression_results) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high), # use hyphen
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Variable, term, OR_CI, p_formatted) %>%
  rename(Level = term, `OR (95% CI)` = OR_CI, `P-value` = p_formatted)

# Save to CSV
write.csv(arch_regression_results_df, "data/arch_vars_prepinit_logistic_regression.csv", row.names = FALSE)










# For categorical variable arch_bin, add frequency table
arch_bin_table <- m2hepprep_prep_combined %>%
  count(arch_bin, name = "Count") %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1),
         Missing = sum(is.na(m2hepprep_prep_combined$arch_bin)))

# Save summary tables
write.csv(arch_summary, "data/arch_score_summary.csv", row.names = FALSE)
write.csv(arch_bin_table, "data/arch_bin_freq.csv", row.names = FALSE)

# Table: PrEP initiation by arch_bin, overall and by site

# Overall table
prep_arch_table_overall <- CreateTableOne(
  vars = "prep_init",
  strata = "arch_bin",
  data = m2hepprep_prep_combined,
  test = TRUE,
  addOverall = TRUE,
  includeNA = FALSE
)
prep_arch_df_overall <- print(prep_arch_table_overall, showAllLevels = TRUE, printToggle = FALSE, formatOptions = list(percent = "row"))
write.csv(prep_arch_df_overall, "data/prep_initiation_by_arch_bin_overall.csv")


# Stratified by site
prep_arch_table_by_site <- CreateTableOne(
  vars = "prep_init",
  strata = c("arch_bin", "sdem_reside"),
  data = m2hepprep_prep_combined,
  test = TRUE,
  addOverall = TRUE,
  includeNA = FALSE
)
prep_arch_df_by_site <- print(prep_arch_table_by_site, showAllLevels = TRUE, printToggle = FALSE, formatOptions = list(percent = "row"))
write.csv(prep_arch_df_by_site, "data/prep_initiation_by_arch_bin_by_site.csv")



# Overall regression (already present)
model_overall <- glm(prep_init ~ arch_bin * sdem_reside + rand_arm, 
                    data = m2hepprep_prep_combined, 
                    family = binomial(link = "logit"))
results_overall <- tidy(model_overall, exponentiate = TRUE, conf.int = TRUE)
print(results_overall)
write.csv(results_overall, "data/prepinit_archbin_sdemreside_interaction_logistic.csv", row.names = FALSE)

# Stratified regressions by sdem_reside
for(site in unique(m2hepprep_prep_combined$sdem_reside)) {
  cat("\nLogistic regression for site:", site, "\n")
  data_site <- m2hepprep_prep_combined %>% filter(sdem_reside == site)
  # Only run if there are both levels of prep_init present
  if(length(unique(data_site$prep_init[!is.na(data_site$prep_init)])) > 1) {
    model_site <- glm(prep_init ~ arch_bin + rand_arm, data = data_site, family = binomial(link = "logit"))
    results_site <- tidy(model_site, exponentiate = TRUE, conf.int = TRUE)
    print(results_site)
    # Save to CSV
    write.csv(results_site, paste0("data/prepinit_archbin_logistic_", gsub(" ", "_", tolower(site)), ".csv"), row.names = FALSE)
  } else {
    cat("Not enough variation in prep_init for site:", site, "\n")
  }
}

# Overall regression WITHOUT interaction
model_overall_no_interaction <- glm(prep_init ~ arch_bin + rand_arm +sdem_reside, 
                                   data = m2hepprep_prep_combined, 
                                   family = binomial(link = "logit"))
results_overall_no_interaction <- tidy(model_overall_no_interaction, exponentiate = TRUE, conf.int = TRUE)
print(results_overall_no_interaction)
write.csv(results_overall_no_interaction, "data/prepinit_archbin_sdemreside_logistic_no_interaction.csv", row.names = FALSE)


# Interaction with city
# Logistic regression with interaction between arch_bin and sdem_reside
model <- glm(prep_init ~ arch_bin * sdem_reside + rand_arm, 
             data = m2hepprep_prep_combined, 
             family = binomial(link = "logit"))

# Extract results with OR and CI
results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)

# Print results
print(results)

# Optionally, save results to CSV
write.csv(results, "data/prepinit_archbin_sdemreside_interaction_logistic.csv", row.names = FALSE)

# Interaction with city
# Logistic regression with interaction between arch_bin and sdem_reside
model <- glm(prep_init ~ arch_bin * sdem_reside + rand_arm + sdem_slep6m_binary + sdem_sex_binary + healthcare_disc_bin + incarc_6m_bin, 
             data = m2hepprep_prep_combined, 
             family = binomial(link = "logit"))

# Extract results with OR and CI
results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)

# Print results
print(results)

# Optionally, save results to CSV
write.csv(results, "data/prepinit_archbin_sdemreside_interaction_logistic_adj.csv", row.names = FALSE)

# Convert prep_init to numeric (0/1) for Poisson regression
m2hepprep_prep_combined$prep_init_num <- as.numeric(m2hepprep_prep_combined$prep_init) - 1

# Unadjusted model (interaction)
model_pr <- glm(prep_init_num ~ arch_bin * sdem_reside + rand_arm, 
                data = m2hepprep_prep_combined, 
                family = poisson(link = "log"))

cov_pr <- sandwich::vcovHC(model_pr, type = "HC0")
results_pr <- broom::tidy(lmtest::coeftest(model_pr, cov_pr), exponentiate = TRUE, conf.int = TRUE)
print(results_pr)
write.csv(results_pr, "data/prepinit_archbin_sdemreside_interaction_PR.csv", row.names = FALSE)

# Adjusted model (interaction + covariates)
model_pr_adj <- glm(prep_init_num ~ arch_bin * sdem_reside + rand_arm + sdem_slep6m_binary + sdem_sex_binary + healthcare_disc_bin + incarc_6m_bin, 
                    data = m2hepprep_prep_combined, 
                    family = poisson(link = "log"))

cov_pr_adj <- sandwich::vcovHC(model_pr_adj, type = "HC0")
results_pr_adj <- broom::tidy(lmtest::coeftest(model_pr_adj, cov_pr_adj), exponentiate = TRUE, conf.int = TRUE)
print(results_pr_adj)
write.csv(results_pr_adj, "data/prepinit_archbin_sdemreside_interaction_PR_adj.csv", row.names = FALSE)


# Unadjusted exact logistic regression (interaction)
model_exact <- logistf(prep_init ~ arch_bin * sdem_reside + rand_arm, 
                       data = m2hepprep_prep_combined)

results_exact <- data.frame(
  term = names(model_exact$coefficients),
  estimate = exp(model_exact$coefficients),
  conf.low = exp(model_exact$ci.lower),
  conf.high = exp(model_exact$ci.upper),
  p.value = model_exact$prob
)
print(results_exact)
write.csv(results_exact, "data/prepinit_archbin_sdemreside_interaction_logistic_exact.csv", row.names = FALSE)

# Adjusted exact logistic regression (interaction + covariates)
model_exact_adj <- logistf(prep_init ~ arch_bin * sdem_reside + rand_arm + sdem_slep6m_binary + sdem_sex_binary + healthcare_disc_bin + incarc_6m_bin, 
                           data = m2hepprep_prep_combined)

results_exact_adj <- data.frame(
  term = names(model_exact_adj$coefficients),
  estimate = exp(model_exact_adj$coefficients),
  conf.low = exp(model_exact_adj$ci.lower),
  conf.high = exp(model_exact_adj$ci.upper),
  p.value = model_exact_adj$prob
)
print(results_exact_adj)
write.csv(results_exact_adj, "data/prepinit_archbin_sdemreside_interaction_logistic_exact_adj.csv", row.names = FALSE)