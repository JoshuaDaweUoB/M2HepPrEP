# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl)

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

# Create binary version of sdem_reside for logistic regression
m2hepprep_prep_combined$sdem_reside_binary <- ifelse(m2hepprep_prep_combined$sdem_reside == "Greater Montreal area", 1, 0)

# Make condom_1m a factor before running models
m2hepprep_prep_combined$condom_1m <- factor(m2hepprep_prep_combined$condom_1m, levels = c(0, 1, 2), labels = c("Never/Rarely/Some", "Very often/Always", "No sex"))
m2hepprep_prep_combined_montreal$condom_1m <- factor(m2hepprep_prep_combined_montreal$condom_1m, levels = c(0, 1, 2), labels = c("Never/Rarely/Some", "Very often/Always", "No sex"))
m2hepprep_prep_combined_miami$condom_1m <- factor(m2hepprep_prep_combined_miami$condom_1m, levels = c(0, 1, 2), labels = c("Never/Rarely/Some", "Very often/Always", "No sex"))

# arch_bin characteristics

# Specify arch_vars
arch_vars <- c(
  "arch_age", "arch_oat", "subscore_cocaine", "subscore_heroin", "subscore_cooker", "subscore_sharing", "subscore_gallery", "arch_bin", "hr_use", "nsp_use", "inject_stims_6m", "inject_opioids_6m", "inject_fent_6m", "inject_meth_6m", "days_used_1m_3cat", "bupe_current", "methadone_current", "naltrexone_current", "other_oat_current"
)

m2hepprep_prep_combined$sdem_age <- as.numeric(as.character(m2hepprep_prep_combined$sdem_age))
# Summarise sdem_age overall and by city
overall_age_summary <- m2hepprep_prep_combined %>%
  summarise(
    Mean = mean(sdem_age, na.rm = TRUE),
    SD = sd(sdem_age, na.rm = TRUE),
    Median = median(sdem_age, na.rm = TRUE),
    Min = min(sdem_age, na.rm = TRUE),
    Max = max(sdem_age, na.rm = TRUE),
    N = sum(!is.na(sdem_age))
  )

age_by_city <- m2hepprep_prep_combined %>%
  group_by(sdem_reside) %>%
  summarise(
    Mean = mean(sdem_age, na.rm = TRUE),
    SD = sd(sdem_age, na.rm = TRUE),
    Median = median(sdem_age, na.rm = TRUE),
    Min = min(sdem_age, na.rm = TRUE),
    Max = max(sdem_age, na.rm = TRUE),
    N = sum(!is.na(sdem_age))
  )

print(overall_age_summary)
print(age_by_city)

# T-test for sdem_age by city
t_test_age_city <- t.test(sdem_age ~ sdem_reside, data = m2hepprep_prep_combined)
print(t_test_age_city)

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
sociostructural_risks <- c("sdem_sex_binary", "sdem_age_binary", "sdem_slep6m_binary", "incarc_6m_bin", "healthcare_coverage", "hr_use", "oat_current", "bupe_current", "methadone_current", "naltrexone_current", "other_oat_current")

injecting_risks <- c("inject_opioids_6m", "inject_stims_6m", "inject_heroin_6m", "inject_cocaine_6m", "inject_meth_6m", "inject_fent_6m", "syringe_share_6m_bin", "syringe_loan_bin", "days_used_1m_3cat", "overdose_6m")

sexual_risks <- c("condom_1m", "sexwork_3m", "sex_work_ever", "bought_sex_3m", "aiv_adt_evr_sex", "aiv_6m_sex", "any_sex_3m", "num_sex_partners_3m", "sex_on_any_drug_1m")

# Lists of variables
risk_lists <- list(
  sociostructural_risks = sociostructural_risks,
  injecting_risks = injecting_risks,
  sexual_risks = sexual_risks
)

# Run logistic regression for each risk variable predicting prep_init, adjusted for sdem_reside and rand_arm
arch_regression_results <- lapply(risk_lists, function(var_list) {
  lapply(var_list, function(var) {
    # Only run if variable has at least 2 levels
    if(length(unique(m2hepprep_prep_combined[[var]][!is.na(m2hepprep_prep_combined[[var]])])) < 2) return(NULL)
    formula <- as.formula(paste("prep_init ~", var, "+ rand_arm + sdem_reside"))
    model <- glm(formula, data = m2hepprep_prep_combined, family = binomial(link = "logit"))
    results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    results$Variable <- var
    results
  })
})

# Combine results and format
arch_regression_results_df <- bind_rows(unlist(arch_regression_results, recursive = FALSE)) %>%
  filter(
    term != "(Intercept)",
    !grepl("^sdem_reside", term),
    !grepl("^rand_arm", term)
  ) %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Variable, term, OR_CI, p_formatted) %>%
  rename(Level = term, `OR (95% CI)` = OR_CI, `P-value` = p_formatted)

# Save to CSV (only variables, not rand_arm or sdem_reside)
write.csv(arch_regression_results_df, "data/arch_vars_prepinit_logistic_regression.csv", row.names = FALSE)

# Run logistic regression for each risk variable predicting prep_init, with interaction between exposure and city, adjusted for rand_arm
arch_regression_results_interaction <- lapply(risk_lists, function(var_list) {
  lapply(var_list, function(var) {
    # Only run if variable has at least 2 levels
    if(length(unique(m2hepprep_prep_combined[[var]][!is.na(m2hepprep_prep_combined[[var]])])) < 2) return(NULL)
    formula <- as.formula(paste("prep_init ~", var, "* sdem_reside + rand_arm"))
    model <- glm(formula, data = m2hepprep_prep_combined, family = binomial(link = "logit"))
    results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    results$Variable <- var
    results
  })
})

# Combine results and format
arch_regression_results_df_interaction <- bind_rows(unlist(arch_regression_results_interaction, recursive = FALSE)) %>%
  filter(
    term != "(Intercept)",
    !grepl("^rand_arm", term)
  ) %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Variable, term, OR_CI, p_formatted) %>%
  rename(Level = term, `OR (95% CI)` = OR_CI, `P-value` = p_formatted)

# Save to CSV (includes interaction terms with city)
write.csv(arch_regression_results_df_interaction, "data/table_vars_prepinit_logistic_regression_cityinteraction.csv", row.names = FALSE)

## interaction term by city

# Load regression results
results <- read.csv("data/table_vars_prepinit_logistic_regression_cityinteraction.csv")

# Helper to safely pull one value (returns NA if missing)
safe_extract <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df)) || length(df[[col]]) == 0) {
    return(NA_character_)
  } else {
    return(as.character(df[[col]][1]))
  }
}

# Parse OR and CI from string "OR (LCL-UCL)"
parse_or_ci <- function(or_ci_string) {
  if (is.na(or_ci_string) || nchar(or_ci_string) == 0) return(c(NA, NA, NA))
  m <- regmatches(or_ci_string, regexec("([0-9\\.]+) \\(([0-9\\.]+)-([0-9\\.]+)\\)", or_ci_string))
  if (length(m[[1]]) != 4) return(c(NA, NA, NA))
  as.numeric(m[[1]][2:4])
}

# Combine all risk lists into one vector
all_vars <- unlist(risk_lists)

# Build formatted table for all levels of each variable in all risk lists
table_out <- lapply(all_vars, function(var) {
  levels_var <- unique(results$Level[results$Variable == var])
  # Drop city/interaction terms
  levels_var <- levels_var[!grepl("sdem_reside", levels_var)]
  # If no levels, create one NA level to force a row
  if (length(levels_var) == 0) levels_var <- NA_character_
  do.call(rbind, lapply(levels_var, function(lvl) {
    montreal    <- results[results$Variable == var & results$Level == lvl, ]
    interaction <- results[results$Variable == var & 
                           results$Level == paste0(lvl, ":sdem_resideGreater Montreal area"), ]
    # Parse ORs
    m_vals <- parse_or_ci(safe_extract(montreal, "OR..95..CI."))
    i_vals <- parse_or_ci(safe_extract(interaction, "OR..95..CI."))
    # Compute Miami OR & CI
    miami_ci <- NA_character_
    if (!all(is.na(m_vals)) & !all(is.na(i_vals))) {
      log_or1 <- log(m_vals[1]); log_or2 <- log(i_vals[1])
      se1 <- (log(m_vals[3]) - log(m_vals[2])) / (2*1.96)
      se2 <- (log(i_vals[3]) - log(i_vals[2])) / (2*1.96)
      log_or_miami <- log_or1 + log_or2
      se_miami <- sqrt(se1^2 + se2^2)
      or_miami <- exp(log_or_miami)
      lcl <- exp(log_or_miami - 1.96*se_miami)
      ucl <- exp(log_or_miami + 1.96*se_miami)
      miami_ci <- sprintf("%.2f (%.2f-%.2f)", or_miami, lcl, ucl)
    }
    data.frame(
      Characteristic     = var,
      Level              = ifelse(is.na(lvl), NA_character_, lvl),
      Montreal_OR_CI     = safe_extract(montreal, "OR..95..CI."),
      Miami_OR_CI        = miami_ci,
      Interaction_OR_CI  = safe_extract(interaction, "OR..95..CI."),
      stringsAsFactors   = FALSE
    )
  }))
})

# Combine into one data frame
table_out_df <- do.call(rbind, table_out)

# Save CSV/Excel
write_xlsx(table_out_df, path = "data/prepinit_cityinteraction_formatted_table.xlsx")








# Run logistic regression for each arch_var predicting prep_init, adjusted for sdem_reside and rand_arm, using _sensitivity dataframe
arch_regression_results <- lapply(arch_vars, function(var) {
  # Only run if variable has at least 2 levels
  if(length(unique(m2hepprep_prep_combined_sensitivity[[var]][!is.na(m2hepprep_prep_combined_sensitivity[[var]])])) < 2) return(NULL)
  formula <- as.formula(paste("prep_init ~", var, "+ rand_arm + sdem_reside"))
  model <- glm(formula, data = m2hepprep_prep_combined_sensitivity, family = binomial(link = "logit"))
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  results$Variable <- var
  results
})

# Combine results and format
arch_regression_results_df <- bind_rows(arch_regression_results) %>%
  filter(
    term != "(Intercept)",
    !grepl("^sdem_reside", term),
    !grepl("^rand_arm", term)
  ) %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Variable, term, OR_CI, p_formatted) %>%
  rename(Level = term, `OR (95% CI)` = OR_CI, `P-value` = p_formatted)

# Save to Excel (only variables, not rand_arm or sdem_reside)
write_xlsx(arch_regression_results_df, "data/arch_vars_prepinit_logistic_regression_sensitivity.xlsx")

# Table: Number and proportion of participants who initiated PrEP by each arch_var level
arch_prepinit_summary <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined[[var]], m2hepprep_prep_combined$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, Count = Freq) %>%
  select(Variable, Level, PrepInit, Count)

arch_prepinit_summary_wide <- arch_prepinit_summary %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_wide, "data/arch_prepinit_summary.csv", row.names = FALSE)


# Table: Number and proportion of participants who initiated PrEP by each arch_var level, stratified by sdem_reside

arch_prepinit_summary_strat <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined[[var]], m2hepprep_prep_combined$prep_init, m2hepprep_prep_combined$sdem_reside, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, City = Var3, Count = Freq) %>%
  select(Variable, Level, City, PrepInit, Count)

arch_prepinit_summary_strat_wide <- arch_prepinit_summary_strat %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_strat_wide, "data/arch_prepinit_summary_stratified.csv", row.names = FALSE)


# Reverse reference category for arch_bin
m2hepprep_prep_combined$arch_bin <- relevel(m2hepprep_prep_combined$arch_bin, ref = setdiff(levels(m2hepprep_prep_combined$arch_bin), levels(m2hepprep_prep_combined$arch_bin)[1])[1])

# Helper function to add N and outcome counts to results
add_n_to_results <- function(results, model_data, formula, outcome_var = "prep_init") {
  vars <- all.vars(as.formula(formula))[-1]
  results$N <- NA
  results$N_outcome_yes <- NA
  results$N_outcome_no <- NA
  for (v in vars) {
    idx <- grep(v, results$term)
    non_missing <- !is.na(model_data[[v]]) & !is.na(model_data[[outcome_var]])
    results$N[idx] <- sum(non_missing)
    if (is.factor(model_data[[v]]) || is.character(model_data[[v]])) {
      for (level in unique(model_data[[v]][non_missing])) {
        level_idx <- grep(paste0(v, level), results$term)
        results$N_outcome_yes[level_idx] <- sum(model_data[[v]] == level & as.character(model_data[[outcome_var]]) == "Yes", na.rm = TRUE)
        results$N_outcome_no[level_idx]  <- sum(model_data[[v]] == level & as.character(model_data[[outcome_var]]) == "No", na.rm = TRUE)
      }
    }
  }
  results
}

# Overall regression
model_overall <- glm(prep_init ~ arch_bin + sdem_reside + rand_arm, 
                    data = m2hepprep_prep_combined, 
                    family = binomial(link = "logit"))
results_overall <- tidy(model_overall, exponentiate = TRUE, conf.int = TRUE)
results_overall <- add_n_to_results(results_overall, m2hepprep_prep_combined, "prep_init ~ arch_bin * sdem_reside + rand_arm")
print(results_overall)
write.csv(results_overall, "data/prepinit_archbin_sdemresiden_logistic.csv", row.names = FALSE)

# Overall adjusted regression
model_overall_adjust <- glm(prep_init ~ arch_bin + sdem_reside + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin, 
                    data = m2hepprep_prep_combined, 
                    family = binomial(link = "logit"))
results_overall_adjust <- tidy(model_overall_adjust, exponentiate = TRUE, conf.int = TRUE)
results_overall_adjust <- add_n_to_results(results_overall_adjust, m2hepprep_prep_combined, "prep_init ~ arch_bin + sdem_reside + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin")
print(results_overall_adjust)
write.csv(results_overall_adjust, "data/prepinit_archbin_adjusted_logistic.csv", row.names = FALSE)

## montreal
montreal_data <- m2hepprep_prep_combined %>% filter(sdem_reside == "Greater Montreal area")

model_overall_montreal <- glm(prep_init ~ arch_bin + rand_arm, 
                             data = montreal_data, 
                             family = binomial(link = "logit"))
results_overall_montreal <- tidy(model_overall_montreal, exponentiate = TRUE, conf.int = TRUE)
results_overall_montreal <- add_n_to_results(results_overall_montreal, montreal_data, "prep_init ~ arch_bin + rand_arm")
print(results_overall_montreal)
write.csv(results_overall_montreal, "data/prepinit_archbin_logistic_montreal.csv", row.names = FALSE)

## montreal adjusted
model_overall_adjusted_montreal <- glm(prep_init ~ arch_bin + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin, 
                             data = montreal_data, 
                             family = binomial(link = "logit"))
results_overall_adjusted_montreal <- tidy(model_overall_adjusted_montreal, exponentiate = TRUE, conf.int = TRUE)
results_overall_adjusted_montreal <- add_n_to_results(results_overall_adjusted_montreal, montreal_data, "prep_init ~ arch_bin + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin")
print(results_overall_adjusted_montreal)
write.csv(results_overall_adjusted_montreal, "data/prepinit_archbin_logistic_adjusted_montreal.csv", row.names = FALSE)

## miami
miami_data <- m2hepprep_prep_combined %>% filter(sdem_reside == "Greater Miami area")

model_overall_miami <- glm(prep_init ~ arch_bin + rand_arm, 
                          data = miami_data, 
                          family = binomial(link = "logit"))
results_overall_miami <- tidy(model_overall_miami, exponentiate = TRUE, conf.int = TRUE)
results_overall_miami <- add_n_to_results(results_overall_miami, miami_data, "prep_init ~ arch_bin + rand_arm")
print(results_overall_miami)
write.csv(results_overall_miami, "data/prepinit_archbin_logistic_miami.csv", row.names = FALSE)

# miami adjusted
model_overall_adjusted_miami <- glm(prep_init ~ arch_bin + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin, 
                          data = miami_data, 
                          family = binomial(link = "logit"))
results_overall_adjusted_miami <- tidy(model_overall_adjusted_miami, exponentiate = TRUE, conf.int = TRUE)
results_overall_adjusted_miami <- add_n_to_results(results_overall_adjusted_miami, miami_data, "prep_init ~ arch_bin + rand_arm + condom_1m + sdem_slep6m_binary + sdem_sex_binary + incarc_6m_bin")
print(results_overall_adjusted_miami)
write.csv(results_overall_adjusted_miami, "data/prepinit_archbin_logistic_adjusted_miami.csv", row.names = FALSE)

# Proportion of participants who initiated PrEP at six months
prep_initiation_table <- table(m2hepprep_prep_combined$prep_init)
prop_prep_initiation <- round(100 * prep_initiation_table["Yes"] / sum(prep_initiation_table), 1)
cat("Proportion of participants who initiated PrEP at six months:", prop_prep_initiation, "%\n")

## Sensitivity analysis: associations with prep initiation

# Load sensitivity data
m2hepprep_prep_combined_sens <- read.csv("data/m2hepprep_prep_combined_sens.csv")

# Ensure prep_init is coded correctly in sensitivity dataframe
m2hepprep_prep_combined_sens$prep_init[m2hepprep_prep_combined_sens$prep_init == 2] <- 1
m2hepprep_prep_combined_sens$prep_init <- factor(m2hepprep_prep_combined_sens$prep_init, levels = c(0, 1), labels = c("No", "Yes"))

# Run logistic regression for each arch_var predicting prep_init, adjusted for sdem_reside and rand_arm (sensitivity)
arch_regression_results_sens <- lapply(arch_vars, function(var) {
  if(length(unique(m2hepprep_prep_combined_sens[[var]][!is.na(m2hepprep_prep_combined_sens[[var]])])) < 2) return(NULL)
  formula <- as.formula(paste("prep_init ~", var, "+ rand_arm + sdem_reside"))
  model <- glm(formula, data = m2hepprep_prep_combined_sens, family = binomial(link = "logit"))
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  results$Variable <- var
  results
})

arch_regression_results_df_sens <- bind_rows(arch_regression_results_sens) %>%
  filter(
    term != "(Intercept)",
    !grepl("^sdem_reside", term),
    !grepl("^rand_arm", term)
  ) %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    p_formatted = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(Variable, term, OR_CI, p_formatted) %>%
  rename(Level = term, `OR (95% CI)` = OR_CI, `P-value` = p_formatted)

write.csv(arch_regression_results_df_sens, "data/arch_vars_prepinit_logistic_regression_sensitivity.csv", row.names = FALSE)

# Table: Number and proportion of participants who initiated PrEP by each arch_var level (sensitivity)
arch_prepinit_summary_sens <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined_sens[[var]], m2hepprep_prep_combined_sens$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, Count = Freq) %>%
  select(Variable, Level, PrepInit, Count)

arch_prepinit_summary_wide_sens <- arch_prepinit_summary_sens %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_wide_sens, "data/arch_prepinit_summary_sens.csv", row.names = FALSE)

# Table: Number and proportion of participants who initiated PrEP by each arch_var level, stratified by sdem_reside (sensitivity)
arch_prepinit_summary_strat_sens <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined_sens[[var]], m2hepprep_prep_combined_sens$prep_init, m2hepprep_prep_combined_sens$sdem_reside, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, City = Var3, Count = Freq) %>%
  select(Variable, Level, City, PrepInit, Count)

arch_prepinit_summary_strat_wide_sens <- arch_prepinit_summary_strat_sens %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_strat_wide_sens, "data/arch_prepinit_summary_stratified_sens.csv", row.names = FALSE)

## Sensitivity analysis: associations with prep initiation stratified by intervention arm

# Filter sensitivity data by intervention arm
m2hepprep_prep_combined_intervention <- m2hepprep_prep_combined_sens %>%
  filter(rand_arm == "On-site")

m2hepprep_prep_combined_nointervention <- m2hepprep_prep_combined_sens %>%
  filter(rand_arm == "Off-site")

# Table: Number and proportion of participants who initiated PrEP by each arch_var level, for intervention arm
arch_prepinit_summary_intervention <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined_intervention[[var]], m2hepprep_prep_combined_intervention$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, Count = Freq) %>%
  select(Variable, Level, PrepInit, Count)

arch_prepinit_summary_wide_intervention <- arch_prepinit_summary_intervention %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_wide_intervention, "data/arch_prepinit_summary_intervention.csv", row.names = FALSE)

# Table: Number and proportion of participants who initiated PrEP by each arch_var level, for no intervention arm
arch_prepinit_summary_nointervention <- lapply(arch_vars, function(var) {
  tab <- table(m2hepprep_prep_combined_nointervention[[var]], m2hepprep_prep_combined_nointervention$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  rename(Level = Var1, PrepInit = Var2, Count = Freq) %>%
  select(Variable, Level, PrepInit, Count)

arch_prepinit_summary_wide_nointervention <- arch_prepinit_summary_nointervention %>%
  pivot_wider(names_from = PrepInit, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = No + Yes,
    perc_yes = round(Yes / Total * 100, 1),
    perc_no = round(No / Total * 100, 1),
    yes_perc = paste0(Yes, " (", perc_yes, "%)"),
    no_perc = paste0(No, " (", perc_no, "%)")
  )

write.csv(arch_prepinit_summary_wide_nointervention, "data/arch_prepinit_summary_nointervention.csv", row.names = FALSE)


# On-site arm
onsite_data <- m2hepprep_prep_combined %>% filter(rand_arm == "On-site")
model_onsite <- glm(prep_init ~ arch_bin + sdem_reside, data = onsite_data, family = binomial(link = "logit"))
results_onsite <- broom::tidy(model_onsite, exponentiate = TRUE, conf.int = TRUE)
write.csv(results_onsite, "data/prepinit_archbin_sdemreside_logistic_onsite.csv", row.names = FALSE)

# Off-site arm
offsite_data <- m2hepprep_prep_combined %>% filter(rand_arm == "Off-site")
model_offsite <- glm(prep_init ~ arch_bin + sdem_reside, data = offsite_data, family = binomial(link = "logit"))
results_offsite <- broom::tidy(model_offsite, exponentiate = TRUE, conf.int = TRUE)
write.csv(results_offsite, "data/prepinit_archbin_sdemreside_logistic_offsite.csv", row.names = FALSE)
