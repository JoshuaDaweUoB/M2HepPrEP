# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl)

# --- Convert prep_init to numeric for Poisson
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(prep_init_num = ifelse(prep_init == "Yes", 1, 0))

# --- Helper for descriptive summaries
get_prepinit_summary <- function(var, data) {
  tab <- table(data[[var]], data$prep_init, useNA = "ifany")
  df <- as.data.frame(tab)
  
  df <- df %>%
    group_by(Var1) %>%
    summarise(
      Total = sum(Freq),
      NInitiated = sum(Freq[Var2 == "Yes"], na.rm = TRUE),  # Count of actually initiated
      PercInitiated = round(100 * NInitiated / Total, 1),
      .groups = "drop"
    ) %>%
    rename(Level = Var1)
  
  df$Variable <- var
  return(df)
}

# --- Fit Poisson regression with robust SE
fit_poisson_pr <- function(var, data) {
  # Skip variables with only one observed level
  if(length(levels(data[[var]])) < 2) return(NULL)
  
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
  
  # Map term to Variable and Level
  df <- df %>%
    mutate(
      Variable = var,
      Level = gsub(paste0("^", var), "", term),
      Level = ifelse(Level == "", levels(data[[var]])[1], Level)
    ) %>%
    filter(term != "(Intercept)")
  
  return(df)
}

# --- Main loop
arch_regression_results_dfs <- lapply(names(risk_lists), function(group_name) {
  message("\n--- Processing group: ", group_name, " ---")
  
  # Fit regressions
  group_results <- lapply(risk_lists[[group_name]], function(var) {
    if(var %in% names(m2hepprep_prep_combined)) {
      fit_poisson_pr(var, m2hepprep_prep_combined)
    } else NULL
  })
  group_results <- bind_rows(group_results)
  
  # All levels for this group
  all_levels <- bind_rows(
    lapply(risk_lists[[group_name]], function(var) {
      if(var %in% names(m2hepprep_prep_combined)) {
        data.frame(
          Variable = var,
          Level = levels(m2hepprep_prep_combined[[var]]),
          stringsAsFactors = FALSE
        )
      } else NULL
    })
  )
  
  # Descriptive summaries
  summary_df <- bind_rows(
    lapply(risk_lists[[group_name]], function(var) {
      if(var %in% names(m2hepprep_prep_combined)) {
        get_prepinit_summary(var, m2hepprep_prep_combined)
      } else NULL
    })
  )
  
  # Join everything
  out <- all_levels %>%
    left_join(group_results %>%
                mutate(`PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
                       `P-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) %>%
                select(Variable, Level, `PR (95% CI)`, `P-value`),
              by = c("Variable", "Level")) %>%
    left_join(summary_df, by = c("Variable", "Level")) %>%
    group_by(Variable) %>%
    mutate(
      `PR (95% CI)` = ifelse(is.na(`PR (95% CI)`), "Reference", `PR (95% CI)`),
      `P-value` = ifelse(is.na(`P-value`), "", `P-value`)
    ) %>%
    ungroup()
  
  return(out)
})

names(arch_regression_results_dfs) <- names(risk_lists)

# Save
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











