# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl, poLCA, ggplot2)

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
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin", "syringe_loan_6m_bin", "syringe_reuse_6m_bin", "days_used_1m_3cat",
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


# ==============================================================================
# LATENT CLASS ANALYSIS
# ==============================================================================

# Prepare LCA data
lca_data <- m2hepprep_prep_combined %>%
  dplyr::select(all_of(lca_vars)) %>%
  mutate(across(everything(), as.factor))

# Create LCA formula
lca_formula <- as.formula(paste("cbind(", paste(lca_vars, collapse = ","), ") ~ 1"))

# Check data quality
cat("Sample size for LCA:", nrow(lca_data), "\n")
cat("Missing values by variable:\n")
print(sapply(lca_data, function(x) sum(is.na(x))))

# ==============================================================================
# MODEL FITTING AND SELECTION
# ==============================================================================

# Initialize storage for results
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

# Set seed for reproducibility
set.seed(123)

# Fit LCA models for 1-5 classes
for (k in 1:5) {
  cat("Fitting", k, "class model...\n")
  
  # Fit model
  lca_model <- poLCA(lca_formula, data = lca_data, nclass = k, 
                     maxiter = 1000, na.rm = TRUE, verbose = FALSE)
  lca_results[[k]] <- lca_model
  
  # Calculate fit statistics
  n <- nrow(lca_data)
  sabic <- lca_model$bic - log(n) * (lca_model$npar - 1) / 2
  
  # Calculate entropy
  entropy <- NA
  if (!is.null(lca_model$posterior)) {
    p <- lca_model$posterior
    entropy <- 1 - sum(p * log(p + 1e-10)) / (n * log(k))
  }
  
  # Get class sizes
  class_sizes <- table(lca_model$predclass)
  class_counts <- rep(NA, 5)
  class_counts[1:length(class_sizes)] <- as.numeric(class_sizes)
  
  # Store results
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

# Display and save fit statistics
print(fit_stats)
write.csv(fit_stats, "data/lca_fit_stats.csv", row.names = FALSE)

# ==============================================================================
# MODEL SELECTION PLOT
# ==============================================================================

elbow_plot <- ggplot(fit_stats, aes(x = NClasses)) +
  geom_line(aes(y = AIC, color = "AIC"), linewidth = 1.2, alpha = 0.8) +
  geom_point(aes(y = AIC, color = "AIC"), size = 4, alpha = 0.9) +
  geom_line(aes(y = BIC, color = "BIC"), linewidth = 1.2, alpha = 0.8) +
  geom_point(aes(y = BIC, color = "BIC"), size = 4, alpha = 0.9) +
  geom_line(aes(y = SABIC, color = "SABIC"), linewidth = 1.2, alpha = 0.8) +
  geom_point(aes(y = SABIC, color = "SABIC"), size = 4, alpha = 0.9) +
  
  scale_color_manual(values = c("AIC" = "#2E86AB", "BIC" = "#A23B72", "SABIC" = "#F18F01")) +
  
  labs(
    x = "Number of Latent Classes",
    y = "Information Criteria",
    title = "Latent Class Analysis Model Selection",
    subtitle = "Lower values indicate better model fit",
    color = "Fit Criteria"
  ) +
  
  scale_x_continuous(breaks = 1:5, labels = 1:5) +
  scale_y_continuous(labels = scales::comma_format()) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30", margin = margin(b = 15)),
    axis.title = element_text(size = 11, color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, color = "grey20"),
    legend.text = element_text(size = 10, color = "grey30"),
    legend.margin = margin(t = 10),
    legend.box.spacing = unit(0.5, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  )

print(elbow_plot)
ggsave("figures/lca_elbow_plot.png", elbow_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# FINAL MODEL SELECTION AND CLASS ASSIGNMENT
# ==============================================================================

# Select best model (lowest BIC)
best_model <- fit_stats[which.min(fit_stats$BIC), ]
cat("Best model based on BIC:\n")
print(best_model)

# Use 3-class solution (based on your analysis)
final_model <- lca_results[[3]]
cat("\nFinal 3-class model results:\n")
print(final_model)

# Assign class membership to main dataset
m2hepprep_prep_combined$class <- NA
m2hepprep_prep_combined$class[1:nrow(lca_data)] <- final_model$predclass

# Create factor variable with descriptive labels
m2hepprep_prep_combined$class_factor <- factor(
  m2hepprep_prep_combined$class,
  levels = c(1, 2, 3),
  labels = c(
    "High Injecting/Low Sexual Risk",
    "Moderate Injecting/High Sexual Risk",
    "Low Overall Risk"
  )
) %>%
  relevel(ref = "High Injecting/Low Sexual Risk")

# Display class distribution and PrEP initiation by class
cat("Class distribution:\n")
print(table(m2hepprep_prep_combined$class_factor, useNA = "ifany"))

cat("\nPrEP initiation by class:\n")
print(table(m2hepprep_prep_combined$class, m2hepprep_prep_combined$prep_init, useNA = "ifany"))

cat("\nPrEP initiation proportions by class:\n")
print(prop.table(table(m2hepprep_prep_combined$class, m2hepprep_prep_combined$prep_init), 1))

# Poisson regression Model 1: prep_init ~ class (unadjusted)
mod_class <- glm(prep_init_num ~ class_factor + sdem_reside + rand_arm, data = m2hepprep_prep_combined, family = poisson(link = "log"))

# Exponentiate estimates and save results for Model 1
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

# Poisson regression Model 2: prep_init ~ class (adjusted)
mod_class_adjusted <- glm(prep_init_num ~ class_factor + sdem_reside + rand_arm + 
                         sdem_sex_binary + sdem_age + oat_current + incarc_6m_bin + sdem_slep6m_binary, 
                         data = m2hepprep_prep_combined, family = poisson(link = "log"))

# Exponentiate estimates and save results for Model 2
robust_se_adj <- sqrt(diag(vcovHC(mod_class_adjusted, type = "HC0")))
coefs_adj <- coeftest(mod_class_adjusted, vcov. = vcovHC(mod_class_adjusted, type = "HC0"))
exp_coef_adj <- exp(coefs_adj[, "Estimate"])
conf_low_adj <- exp(coefs_adj[, "Estimate"] - 1.96 * robust_se_adj)
conf_high_adj <- exp(coefs_adj[, "Estimate"] + 1.96 * robust_se_adj)
p_val_adj <- coefs_adj[, "Pr(>|z|)"]

poisson_class_adjusted_results <- data.frame(
  term = rownames(coefs_adj),
  estimate = exp_coef_adj,
  conf.low = conf_low_adj,
  conf.high = conf_high_adj,
  p.value = p_val_adj,
  stringsAsFactors = FALSE
)

# Save both models to Excel as separate sheets
write_xlsx(list(
  "Poisson_class_unadjusted" = poisson_class_results,
  "Poisson_class_adjusted" = poisson_class_adjusted_results
), "data/poisson_class_results.xlsx")

