# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

risk_lists <- list(
  sociostructural_risks = c(
    "sdem_sex_binary",
    "sdem_age_binary",
    "sdem_slep6m_binary",
    "incarc_6m_bin",
    "oat_ever",
    "oat_current",
    "methadone_current",
    "bupe_current",
    "oat_other_current",
    "healthcare_coverage"
  ),
  
  injecting_risks = c(
    "inject_opioids_6m",
    "inject_stims_6m",
    "inject_heroin_6m",
    "inject_cocaine_6m",
    "inject_meth_6m",
    "inject_fent_6m",
    "syringe_share_6m_bin",
    "syringe_loan_bin",
    "days_used_1m_3cat",
    "overdose_6m"
  ),
  
  sexual_risks = c(
    "any_sex_3m",
    "num_sex_partners_3m",
    "condom_1m",
    "sex_work_ever",
    "sexwork_3m",
    "bought_sex_3m",
    "sexual_abuse_ever",
    "sexual_abuse_6m",
    "sex_on_any_drug_1m"
  )
)

risk_labels <- c(
  # Sociostructural
  sdem_sex_binary       = "Sex",
  sdem_age_binary       = "Age (years)",
  sdem_slep6m_binary    = "Housing status",
  incarc_6m_bin         = "Incarceration history",
  oat_ever              = "Lifetime OAT",
  oat_current           = "Recent OAT",
  methadone_current     = "Recent methadone",
  bupe_current          = "Recent buprenorphine",
  oat_other_current     = "Recent other OAT",
  healthcare_coverage   = "Healthcare coverage",
  
  # Injecting risks
  inject_opioids_6m     = "Injected opioids",
  inject_stims_6m       = "Injected stimulants",
  inject_heroin_6m      = "Injected heroin",
  inject_cocaine_6m     = "Injected cocaine",
  inject_meth_6m        = "Injected amphetamines",
  inject_fent_6m        = "Injected fentanyl",
  syringe_share_6m_bin  = "Used others' injecting equipment/syringes",
  syringe_loan_bin      = "Shared own injecting equipment/syringes",
  days_used_1m_3cat     = "Injecting frequency",
  overdose_6m           = "Overdose",
  
  # Sexual risks
  any_sex_3m            = "Any sex past 3 months",
  num_sex_partners_3m   = "Number of sex partners past 3 months",
  condom_1m             = "Condom use past 1 month",
  sex_work_ever         = "Ever engaged in sex work",
  sexwork_3m            = "Sex work past 3 months",
  bought_sex_3m         = "Bought sex past 3 months",
  sexual_abuse_ever     = "Ever experienced sexual abuse",
  sexual_abuse_6m       = "Sexual abuse past 6 months",
  sex_on_any_drug_1m    = "Sex on any drug past 1 month"
)

# --- Convert prep_init to numeric for Poisson
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(prep_init_num = ifelse(prep_init == "Yes", 1, 0))

# --- Helper for descriptive summaries with concatenated x (x.x)
get_prepinit_summary <- function(var, data) {
  tab <- table(data[[var]], data$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  
  df <- df %>%
    group_by(Var1) %>%
    summarise(
      Total = sum(Freq),
      NInitiated = sum(Freq[Var2 == "Yes"], na.rm = TRUE),
      PercInitiated = round(100 * NInitiated / Total, 1),
      .groups = "drop"
    ) %>%
    rename(Level = Var1) %>%
    mutate(x = paste0(NInitiated, " (", PercInitiated, ")"))  # <-- concatenated column
  
  df$Variable <- var
  return(df %>% select(Variable, Level, Total, x))  # return concatenated column instead of separate N/Perc
}

# --- Fit Poisson regression with robust SE
fit_poisson_pr <- function(var, data) {
  # skip if fewer than 2 levels
  if(length(unique(na.omit(data[[var]]))) < 2) return(NULL)
  
  formula <- as.formula(paste("prep_init_num ~", var, "+ sdem_reside"))
  mod <- glm(formula, data = data, family = poisson(link = "log"))
  
  robust_se <- sqrt(diag(vcovHC(mod, type = "HC0")))
  coefs <- coeftest(mod, vcov. = vcovHC(mod, type = "HC0"))
  
  df <- data.frame(
    term = rownames(coefs),
    estimate = exp(coefs[, "Estimate"]),
    conf.low = exp(coefs[, "Estimate"] - 1.96 * robust_se),
    conf.high = exp(coefs[, "Estimate"] + 1.96 * robust_se),
    p.value = coefs[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  df <- df %>%
    mutate(
      Variable = var,
      Level = gsub(paste0("^", var), "", term),
      Level = ifelse(Level == "", levels(data[[var]])[1], Level)
    ) %>%
    filter(term != "(Intercept)")
  
  return(df)
}


# Function to add empty row between variables
add_empty_rows <- function(df) {
  df_list <- split(df, df$Variable)
  df_list <- lapply(df_list, function(x) rbind(x, setNames(as.list(rep(NA, ncol(x))), colnames(x))))
  df <- bind_rows(df_list)
  return(df)
}

# --- Main loop
arch_regression_results_dfs <- lapply(names(risk_lists), function(group_name) {
  message("\n--- Processing group: ", group_name, " ---")
  
  # Initialize list to store results for each variable
  out_list <- list()
  
  for (var in risk_lists[[group_name]]) {
    if(var %in% names(m2hepprep_prep_combined)) {
      reg_df <- fit_poisson_pr(var, m2hepprep_prep_combined)
      desc_df <- get_prepinit_summary(var, m2hepprep_prep_combined)
      
      all_levels <- data.frame(
        Variable = var,
        Level = levels(m2hepprep_prep_combined[[var]]),
        stringsAsFactors = FALSE
      )
      
      combined <- all_levels %>%
        left_join(reg_df %>%
                    mutate(`PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
                           `P-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) %>%
                    select(Variable, Level, `PR (95% CI)`, `P-value`),
                  by = c("Variable", "Level")) %>%
        left_join(desc_df, by = c("Variable", "Level")) %>%
        mutate(
          `PR (95% CI)` = ifelse(is.na(`PR (95% CI)`), "ref.", `PR (95% CI)`),
          `P-value` = ifelse(is.na(`P-value`), "", `P-value`)
        )
      
      # Add empty row
      combined <- rbind(combined, setNames(as.list(rep(NA, ncol(combined))), colnames(combined)))
      
      out_list[[var]] <- combined
    }
  }
  
  # Bind all variables in original order
  out <- bind_rows(out_list)
  return(out)
})

names(arch_regression_results_dfs) <- names(risk_lists)

# Save to Excel with each group as a sheet
write_xlsx(arch_regression_results_dfs, "data/arch_vars_prepinit_poisson_results.xlsx")










# --- Helper for descriptive summaries
get_prepinit_summary <- function(var, data) {
  tab <- table(data[[var]], data$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  
  df <- df %>%
    group_by(Var1) %>%
    summarise(
      Total = sum(Freq),
      NInitiated = sum(Freq[Var2 == "Yes"], na.rm = TRUE),
      PercInitiated = round(100 * NInitiated / Total, 1),
      .groups = "drop"
    ) %>%
    rename(Level = Var1)
  
  df$Variable <- var
  return(df)
}

# --- Fit Poisson regression with robust SE, including interaction
fit_poisson_pr_interaction <- function(var, data) {
  if(length(levels(data[[var]])) < 2) return(NULL)
  
  formula <- as.formula(paste("prep_init_num ~ sdem_reside *", var))
  mod <- glm(formula, data = data, family = poisson(link = "log"))
  
  robust_se <- sqrt(diag(vcovHC(mod, type = "HC0")))
  coefs <- coeftest(mod, vcov. = vcovHC(mod, type = "HC0"))
  
  df <- data.frame(
    term = rownames(coefs),
    estimate = exp(coefs[, "Estimate"]),
    conf.low = exp(coefs[, "Estimate"] - 1.96 * robust_se),
    conf.high = exp(coefs[, "Estimate"] + 1.96 * robust_se),
    p.value = coefs[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  # Label terms clearly
  df <- df %>%
    mutate(
      Variable = var,
      Level = ifelse(grepl(":", term), paste0("Interaction: ", term),
                     gsub(paste0("^", var), "", term)),
      Level = ifelse(Level == "", levels(data[[var]])[1], Level)
    ) %>%
    filter(term != "(Intercept)")
  
  return(df)
}

# --- Main loop
arch_regression_results_dfs <- lapply(names(risk_lists), function(group_name) {
  message("\n--- Processing group: ", group_name, " ---")
  
  # Fit Poisson regressions with interaction
  group_results <- lapply(risk_lists[[group_name]], function(var) {
    if(var %in% names(m2hepprep_prep_combined)) {
      fit_poisson_pr_interaction(var, m2hepprep_prep_combined)
    } else NULL
  })
  group_results <- bind_rows(group_results)
  
  # Descriptive summaries for all variables
  summary_df <- bind_rows(
    lapply(risk_lists[[group_name]], function(var) {
      if(var %in% names(m2hepprep_prep_combined)) {
        get_prepinit_summary(var, m2hepprep_prep_combined)
      } else NULL
    })
  )
  
  # Join model output and descriptive counts
  out <- group_results %>%
    left_join(summary_df, by = c("Variable", "Level")) %>%
    mutate(
      `PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      `P-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Variable, Level, `PR (95% CI)`, `P-value`, Total, NInitiated, PercInitiated)
  
  return(out)
})

# Keep group names
names(arch_regression_results_dfs) <- names(risk_lists)

# Save to Excel
write_xlsx(arch_regression_results_dfs, "data/arch_vars_prepinit_poisson_interaction_results.xlsx")











