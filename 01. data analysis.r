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

# Create binary version of sdem_reside for logistic regression
m2hepprep_prep_combined$sdem_reside_binary <- ifelse(m2hepprep_prep_combined$sdem_reside == "Greater Montreal area", 1, 0)

# arch_bin characteristics

# Specify arch_vars
arch_vars <- c(
  "arch_age", "arch_oat", "subscore_opioids", "subscore_stimulants", "subscore_cooker", "subscore_sharing", "subscore_gallery", "arch_bin", "hr_use", "nsp_use", "msm_ever"
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

# Run logistic regression for each arch_var predicting prep_init, adjusted for sdem_reside and rand_arm
arch_regression_results <- lapply(arch_vars, function(var) {
  # Only run if variable has at least 2 levels
  if(length(unique(m2hepprep_prep_combined[[var]][!is.na(m2hepprep_prep_combined[[var]])])) < 2) return(NULL)
  formula <- as.formula(paste("prep_init ~", var, "+ rand_arm + sdem_reside"))
  model <- glm(formula, data = m2hepprep_prep_combined, family = binomial(link = "logit"))
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

# Save to CSV (only variables, not rand_arm or sdem_reside)
write.csv(arch_regression_results_df, "data/arch_vars_prepinit_logistic_regression.csv", row.names = FALSE)

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

# Overall regression (already present)
model_overall <- glm(prep_init ~ arch_bin * sdem_reside + rand_arm, 
                    data = m2hepprep_prep_combined, 
                    family = binomial(link = "logit"))
results_overall <- tidy(model_overall, exponentiate = TRUE, conf.int = TRUE)
print(results_overall)
write.csv(results_overall, "data/prepinit_archbin_sdemreside_interaction_logistic.csv", row.names = FALSE)

## montreal
montreal_data <- m2hepprep_prep_combined %>% filter(sdem_reside == "Greater Montreal area")

model_overall_montreal <- glm(prep_init ~ arch_bin + rand_arm, 
                             data = montreal_data, 
                             family = binomial(link = "logit"))
results_overall_montreal <- tidy(model_overall_montreal, exponentiate = TRUE, conf.int = TRUE)
print(results_overall_montreal)
write.csv(results_overall_montreal, "data/prepinit_archbin_logistic_montreal.csv", row.names = FALSE)

## miami
miami_data <- m2hepprep_prep_combined %>% filter(sdem_reside == "Greater Miami area")

model_overall_miami <- glm(prep_init ~ arch_bin + rand_arm, 
                          data = miami_data, 
                          family = binomial(link = "logit"))
results_overall_miami <- tidy(model_overall_miami, exponentiate = TRUE, conf.int = TRUE)
print(results_overall_miami)
write.csv(results_overall_miami, "data/prepinit_archbin_logistic_miami.csv", row.names = FALSE)