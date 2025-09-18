# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl, poLCA)

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
    "msm_ever",
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
  msm_ever              = "Men who have sex with men",
  sexual_abuse_ever     = "Ever experienced sexual abuse",
  sexual_abuse_6m       = "Sexual abuse past 6 months",
  sex_on_any_drug_1m    = "Sex on any drug past 1 month"
)

# --- Convert prep_init to numeric for Poisson
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(prep_init_num = ifelse(prep_init == "Yes", 1, 0))

# miami 
m2hepprep_prep_combined_miami <- m2hepprep_prep_combined %>%
  dplyr::filter(sdem_reside == "Greater Miami area")

# montreal 
m2hepprep_prep_combined_montreal <- m2hepprep_prep_combined %>%
  dplyr::filter(sdem_reside == "Greater Montreal area")

## latent class analysis

# sexual and injecting risk variables
lca_vars <- c(
  "inject_heroin_6m", "inject_cocaine_6m", "inject_meth_6m", "inject_fent_6m",
  "syringe_share_6m_bin", "syringe_loan_6m_bin", "syringe_cooker_6m_bin", "syringe_reuse_6m_bin", "days_used_1m_3cat",
  "num_sex_partners_3m", "condom_1m", "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m"
)

# missing values for all LCA variables to "No"
for (v in lca_vars) {
  if (v %in% names(m2hepprep_prep_combined)) {
    # If factor, add "No" as a level if missing
    if (is.factor(m2hepprep_prep_combined[[v]])) {
      m2hepprep_prep_combined[[v]] <- forcats::fct_expand(m2hepprep_prep_combined[[v]], "No")
    }
    m2hepprep_prep_combined[[v]][is.na(m2hepprep_prep_combined[[v]])] <- "No"
  }
}

# lca data data
lca_data <- m2hepprep_prep_combined %>%
  dplyr::select(all_of(lca_vars))

# convert to factors
lca_data[] <- lapply(lca_data, as.factor)

# lca formula
lca_formula <- as.formula(paste("cbind(", paste(lca_vars, collapse = ","), ") ~ 1"))

# sample size and missingness
cat("Sample size for LCA:", nrow(lca_data), "\n")
print(sapply(lca_data, function(x) sum(is.na(x))))

# LCA for 1 to 5 classes
lca_results <- list()
fit_stats <- data.frame(
  NClasses = integer(),
  NFreeParameters = integer(),
  AIC = numeric(),
  BIC = numeric(),
  SABIC = numeric(),
  Entropy = numeric(),
  LL = numeric(),
  ChiSquare = numeric(),
  df = integer(),
  NClass1 = integer(),
  NClass2 = integer(),
  NClass3 = integer(),
  NClass4 = integer(),
  NClass5 = integer(),
  stringsAsFactors = FALSE
)

# set seed
set.seed(123)

# calculate fit statistics
for (k in 1:5) {
  lca_model <- poLCA(lca_formula, data = lca_data, nclass = k, maxiter = 1000, na.rm = TRUE, verbose = FALSE)
  lca_results[[k]] <- lca_model
  # SABIC
  n <- nrow(lca_data)
  sabic <- lca_model$bic - log(n) * (lca_model$npar - 1) / 2
  # entropy
  entropy <- NA
  if (!is.null(lca_model$posterior)) {
    p <- lca_model$posterior
    entropy <- 1 - sum(p * log(p + 1e-10)) / (n * log(k))
  }
  # class sizes
  class_sizes <- table(lca_model$predclass)
  # clean missing classes
  class_counts <- rep(NA, 5)
  class_counts[1:length(class_sizes)] <- as.numeric(class_sizes)
  fit_stats <- rbind(fit_stats, data.frame(
    NClasses = k,
    NFreeParameters = lca_model$npar,
    AIC = lca_model$aic,
    BIC = lca_model$bic,
    SABIC = sabic,
    Entropy = round(entropy, 2),
    LL = lca_model$llik,
    ChiSquare = lca_model$Gsq,
    df = lca_model$resid.df,
    NClass1 = class_counts[1],
    NClass2 = class_counts[2],
    NClass3 = class_counts[3],
    NClass4 = class_counts[4],
    NClass5 = class_counts[5]
  ))
}

print(fit_stats)
write.csv(fit_stats, "data/lca_fit_stats.csv", row.names = FALSE)

best_model <- fit_stats[which.min(fit_stats$BIC), ]
print(best_model)

# class membership
lca_data$class <- lca_results[[3]]$predclass

# class proportions and item-response probabilities
print(lca_results[[3]])

# Ylabel classes as LIHS, HIMS, LILS
table(lca_data$class)

# add to original dataset
m2hepprep_prep_combined$class <- NA
m2hepprep_prep_combined$class[as.numeric(rownames(lca_data))] <- lca_data$class

# tab prep_init by class
table(m2hepprep_prep_combined$class, m2hepprep_prep_combined$prep_init, useNA = "ifany")

# calc proportions
prop.table(table(m2hepprep_prep_combined$class, m2hepprep_prep_combined$prep_init), 1)

# Recode class_factor levels to descriptive labels and set class 2 as reference
m2hepprep_prep_combined$class_factor <- factor(
  m2hepprep_prep_combined$class,
  levels = c(1, 2, 3),
  labels = c("High Injection/Low Sexual Risk", "Low Overall Risk", "Moderate Injection/High Sexual Risk")
)
m2hepprep_prep_combined$class_factor <- relevel(m2hepprep_prep_combined$class_factor, ref = "Low Overall Risk")

# Poisson regression: prep_init ~ class
mod_class <- glm(prep_init_num ~ class_factor + sdem_reside, data = m2hepprep_prep_combined, family = poisson(link = "log"))

# Exponentiate estimates and save results as before
robust_se <- sqrt(diag(vcovHC(mod_class, type = "HC0")))
coefs <- coeftest(mod_class, vcov. = vcovHC(mod_class, type = "HC0"))
exp_coef <- exp(coefs[, "Estimate"])
conf_low <- exp(coefs[, "Estimate"] - 1.96 * robust_se)
conf_high <- exp(coefs[, "Estimate"] + 1.96 * robust_se)
p_val <- coefs[, "Pr(>|z|)"]

poisson_class_results <- data.frame(
  term = rownames(coefs),
  estimate = exp_coef,
  conf.low = conf_low,
  conf.high = conf_high,
  p.value = p_val,
  stringsAsFactors = FALSE
)

# Class 2: "Low Overall Risk"
# Class 1: "High Injection/Low Sexual Risk"
# Class 3: "Moderate Injection/High Sexual Risk"

# Save to Excel as a sheet
write_xlsx(list("Poisson_class_results" = poisson_class_results), "data/poisson_class_results.xlsx")

# Make sure class membership is a factor
m2hepprep_prep_combined$class_factor <- factor(m2hepprep_prep_combined$class)

# Fit multinomial logistic regression: sociostructural risks as predictors of class
mod_multinom <- multinom(class_factor ~ sdem_reside + sdem_sex_binary + sdem_age_binary + sdem_slep6m_binary +
                           incarc_6m_bin + oat_ever + methadone_current +
                           bupe_current + healthcare_coverage,
                         data = m2hepprep_prep_combined)

# Get coefficients and standard errors
coefs <- summary(mod_multinom)$coefficients
ses <- summary(mod_multinom)$standard.errors

# Calculate RRRs and 95% CIs
rrr <- exp(coefs)
ci_low <- exp(coefs - 1.96 * ses)
ci_high <- exp(coefs + 1.96 * ses)

# Get p-values
z <- coefs / ses
p <- 2 * (1 - pnorm(abs(z)))

# Format as a table
results_table <- data.frame(
  Predictor = rep(colnames(coefs), each = nrow(coefs)),
  Class = rep(rownames(coefs), times = ncol(coefs)),
  RRR = as.vector(rrr),
  CI_low = as.vector(ci_low),
  CI_high = as.vector(ci_high),
  p_value = as.vector(p)
)

# Print nicely
knitr::kable(results_table, digits = 2, caption = "Multinomial regression: Sociostructural risks as predictors of class membership")

# Save Poisson regression results (with interaction) to Excel spreadsheet
write_xlsx(results_table, "data/lca_predictors.xlsx")























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

fit_poisson_pr <- function(var, data) {
  # skip if fewer than 2 levels
  if(length(unique(na.omit(data[[var]]))) < 2) return(NULL)
  
  formula <- as.formula(paste("prep_init_num ~", var))  # <-- no sdem_reside
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

# --- Main loop (Miami only) ---
arch_regression_results_dfs_miami <- lapply(names(risk_lists), function(group_name) {
  
  out_list <- list()
  
  for (var in risk_lists[[group_name]]) {
    if (var %in% names(m2hepprep_prep_combined_miami)) {
      
      # Fit Poisson regression
      formula <- as.formula(paste("prep_init_num ~", var))
      mod <- glm(formula, data = m2hepprep_prep_combined_miami, family = poisson(link = "log"))
      robust_se <- sqrt(diag(vcovHC(mod, type = "HC0")))
      coefs <- coeftest(mod, vcov. = vcovHC(mod, type = "HC0"))
      
      reg_df <- data.frame(
        term = rownames(coefs),
        estimate = exp(coefs[, "Estimate"]),
        conf.low = exp(coefs[, "Estimate"] - 1.96 * robust_se),
        conf.high = exp(coefs[, "Estimate"] + 1.96 * robust_se),
        p.value = coefs[, "Pr(>|z|)"],
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          Variable = var,
          Level = gsub(paste0("^", var), "", term),
          Level = ifelse(Level == "", levels(m2hepprep_prep_combined_miami[[var]])[1], Level)
        ) %>%
        filter(term != "(Intercept)")
      
      # Descriptive summary
      desc_df <- get_prepinit_summary(var, m2hepprep_prep_combined_miami)
      
      # Combine regression and descriptive summaries
      all_levels <- data.frame(
        Variable = var,
        Level = levels(m2hepprep_prep_combined_miami[[var]]),
        stringsAsFactors = FALSE
      )
      
      combined <- all_levels %>%
        left_join(
          reg_df %>%
            mutate(
              `PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
              `P-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
            ) %>%
            select(Variable, Level, `PR (95% CI)`, `P-value`),
          by = c("Variable", "Level")
        ) %>%
        left_join(desc_df, by = c("Variable", "Level")) %>%
        mutate(
          `PR (95% CI)` = ifelse(is.na(`PR (95% CI)`), "ref.", `PR (95% CI)`),
          `P-value` = ifelse(is.na(`P-value`), "", `P-value`)
        )
      
      # Add empty row between variables
      combined <- rbind(combined, setNames(as.list(rep(NA, ncol(combined))), colnames(combined)))
      
      out_list[[var]] <- combined
    }
  }
  
  bind_rows(out_list)
})

names(arch_regression_results_dfs_miami) <- names(risk_lists)

# Save results to Excel
write_xlsx(arch_regression_results_dfs_miami, "data/arch_vars_prepinit_poisson_results_miami.xlsx")

# --- Main loop (Montreal only) ---
arch_regression_results_dfs_montreal <- lapply(names(risk_lists), function(group_name) {
  
  out_list <- list()
  
  for (var in risk_lists[[group_name]]) {
    if (var %in% names(m2hepprep_prep_combined_montreal)) {
      
      # Fit Poisson regression
      formula <- as.formula(paste("prep_init_num ~", var))
      mod <- glm(formula, data = m2hepprep_prep_combined_montreal, family = poisson(link = "log"))
      robust_se <- sqrt(diag(vcovHC(mod, type = "HC0"))
      )
      coefs <- coeftest(mod, vcov. = vcovHC(mod, type = "HC0"))
      
      reg_df <- data.frame(
        term = rownames(coefs),
        estimate = exp(coefs[, "Estimate"]),
        conf.low = exp(coefs[, "Estimate"] - 1.96 * robust_se),
        conf.high = exp(coefs[, "Estimate"] + 1.96 * robust_se),
        p.value = coefs[, "Pr(>|z|)"],
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          Variable = var,
          Level = gsub(paste0("^", var), "", term),
          Level = ifelse(Level == "", levels(m2hepprep_prep_combined_montreal[[var]])[1], Level)
        ) %>%
        filter(term != "(Intercept)")
      
      # Descriptive summary
      desc_df <- get_prepinit_summary(var, m2hepprep_prep_combined_montreal)
      
      # Combine regression and descriptive summaries
      all_levels <- data.frame(
        Variable = var,
        Level = levels(m2hepprep_prep_combined_montreal[[var]]),
        stringsAsFactors = FALSE
      )
      
      combined <- all_levels %>%
        left_join(
          reg_df %>%
            mutate(
              `PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
              `P-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
            ) %>%
            select(Variable, Level, `PR (95% CI)`, `P-value`),
          by = c("Variable", "Level")
        ) %>%
        left_join(desc_df, by = c("Variable", "Level")) %>%
        mutate(
          `PR (95% CI)` = ifelse(is.na(`PR (95% CI)`), "ref.", `PR (95% CI)`),
          `P-value` = ifelse(is.na(`P-value`), "", `P-value`)
        )
      
      # Add empty row between variables
      combined <- rbind(combined, setNames(as.list(rep(NA, ncol(combined))), colnames(combined)))
      
      out_list[[var]] <- combined
    }
  }
  
  bind_rows(out_list)
})

names(arch_regression_results_dfs_montreal) <- names(risk_lists)

# Save results to Excel
write_xlsx(arch_regression_results_dfs_montreal, "data/arch_vars_prepinit_poisson_results_montreal.xlsx")








































# Save to Excel with each group as a sheet
write_xlsx(arch_regression_results_dfs_miami, "data/arch_vars_prepinit_poisson_results_miami.xlsx")

# --- Main loop (Montreal only)
arch_regression_results_dfs_montreal <- lapply(names(risk_lists), function(group_name) {
  message("\n--- Processing group: ", group_name, " ---")
  
  # Initialize list to store results for each variable
  out_list <- list()
  
  for (var in risk_lists[[group_name]]) {
    if(var %in% names(m2hepprep_prep_combined_montreal)) {
      reg_df <- fit_poisson_pr(var, m2hepprep_prep_combined_montreal)
      desc_df <- get_prepinit_summary(var, m2hepprep_prep_combined_montreal)
      
      all_levels <- data.frame(
        Variable = var,
        Level = levels(m2hepprep_prep_combined_montreal[[var]]),
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

names(arch_regression_results_dfs_montreal) <- names(risk_lists)

# Save to Excel with each group as a sheet
write_xlsx(arch_regression_results_dfs_montreal, "data/arch_vars_prepinit_poisson_results_montreal.xlsx")




































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











