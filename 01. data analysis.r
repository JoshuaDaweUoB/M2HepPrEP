# load packages
pacman::p_load(tidyr, readr, readxl, lubridate, tableone, broom, lmtest, sandwich, logistf, writexl, poLCA, ggplot2, mice, VIM, dplyr)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# run processing script
source("code/00. data processing.r")
message("data processing script run")

# risk lists
risk_lists <- list(
  sociostructural_risks = c(
    "sdem_sex_binary",
    "sdem_age_binary",
    "sdem_slep6m_binary",
    "incarc_6m_bin",
    "oat_current",
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

# Define LCA variables first
lca_vars <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin", "syringe_loan_6m_bin", "syringe_reuse_6m_bin", "days_used_1m_3cat",
  "num_sex_partners_3m", "condom_1m", "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m"
)

lca_vars_bin <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin", "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m"
)

# missing values for all LCA variables to "No"
for (v in lca_vars_bin) {
  if (v %in% names(m2hepprep_prep_combined)) {
    # If factor, add "No" as a level if missing
    if (is.factor(m2hepprep_prep_combined[[v]])) {
      m2hepprep_prep_combined[[v]] <- forcats::fct_expand(m2hepprep_prep_combined[[v]], "No")
    }
    m2hepprep_prep_combined[[v]][is.na(m2hepprep_prep_combined[[v]])] <- "No"
  }
}

# Check missing patterns before imputation
cat("Missing data patterns:\n")
print(VIM::aggr(m2hepprep_prep_combined[lca_vars], col = c('navyblue','red'), 
                numbers = TRUE, sortVars = TRUE))

# Create dataset with LCA variables and auxiliary variables for imputation
# Fix the select() syntax by adding proper commas
imputation_data <- m2hepprep_prep_combined %>%
  dplyr::select(all_of(lca_vars), 
                sdem_age, sdem_sex_binary, sdem_reside, oat_current, 
                incarc_6m_bin, sdem_slep6m_binary)

# Check missing percentages
missing_percent <- sapply(imputation_data[lca_vars], function(x) round(sum(is.na(x))/length(x) * 100, 1))
cat("Missing percentages by variable:\n")
print(missing_percent)

# Set up imputation method for each variable type
imputation_methods <- mice::make.method(imputation_data)

# Specify imputation methods
binary_vars <- c("inject_meth_6m", "inject_cocaine_6m", "syringe_share_6m_bin", 
                "syringe_cooker_6m_bin", "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
                "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m")

categorical_vars <- c("days_used_1m_3cat", "num_sex_partners_3m", "condom_1m")

# Set methods
imputation_methods[binary_vars] <- "logreg"
imputation_methods[categorical_vars] <- "polyreg"

# Don't impute auxiliary variables
auxiliary_vars <- c("sdem_age", "sdem_sex_binary", "sdem_reside", "oat_current", 
                   "incarc_6m_bin", "sdem_slep6m_binary")
imputation_methods[auxiliary_vars] <- ""

cat("Imputation methods:\n")
print(imputation_methods[lca_vars])

# Perform multiple imputation
set.seed(123)
cat("Performing multiple imputation (5 datasets)...\n")

imputed_data <- mice(imputation_data, 
                    method = imputation_methods,
                    m = 5,
                    maxit = 10,
                    printFlag = FALSE,
                    seed = 123)

cat("Multiple imputation completed successfully!\n")

## latent class analysis

## LATENT CLASS ANALYSIS WITH MULTIPLE IMPUTATION

# ==============================================================================
# PREPARE IMPUTED DATASETS FOR LCA
# ==============================================================================

# Create completed datasets for LCA from multiple imputation
cat("Creating completed datasets from multiple imputation...\n")
imputed_datasets <- list()
for(i in 1:5) {
  completed_data <- complete(imputed_data, i)
  # Keep only LCA variables and convert to factors
  imputed_datasets[[i]] <- completed_data[lca_vars] %>%
    mutate(across(everything(), as.factor))
}

# Create LCA formula
lca_formula <- as.formula(paste("cbind(", paste(lca_vars, collapse = ","), ") ~ 1"))

# Check data quality for first imputed dataset
cat("Sample size for LCA:", nrow(imputed_datasets[[1]]), "\n")
cat("Missing values by variable (should be 0 for all):\n")
print(sapply(imputed_datasets[[1]], function(x) sum(is.na(x))))

# ==============================================================================
# MODEL FITTING AND SELECTION ON IMPUTED DATA
# ==============================================================================

# Initialize storage for results across all imputed datasets
lca_imputed_results <- list()
fit_stats_all <- list()

# Function to calculate fit statistics
calculate_fit_stats <- function(lca_model, k, n) {
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
  
  return(data.frame(
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

# Set seed for reproducibility
set.seed(123)

# Run LCA on each imputed dataset
for(imp in 1:5) {
  cat("Running LCA on imputed dataset", imp, "...\n")
  
  lca_results_imp <- list()
  fit_stats_imp <- data.frame(
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
  
  # Fit LCA models for 1-5 classes on this imputed dataset
  for (k in 1:5) {
    cat("  Fitting", k, "class model...\n")
    
    # Fit model
    lca_model <- poLCA(lca_formula, data = imputed_datasets[[imp]], nclass = k, 
                       maxiter = 1000, na.rm = TRUE, verbose = FALSE)
    lca_results_imp[[k]] <- lca_model
    
    # Calculate fit statistics
    n <- nrow(imputed_datasets[[imp]])
    fit_stats_imp <- rbind(fit_stats_imp, calculate_fit_stats(lca_model, k, n))
  }
  
  lca_imputed_results[[imp]] <- lca_results_imp
  fit_stats_all[[imp]] <- fit_stats_imp
}

# Pool fit statistics across imputations (take mean)
pooled_fit_stats <- fit_stats_all[[1]]
for(col in c("AIC", "BIC", "SABIC", "Entropy", "LL", "ChiSquare")) {
  pooled_fit_stats[[col]] <- rowMeans(sapply(fit_stats_all, function(x) x[[col]]), na.rm = TRUE)
}

# Display and save pooled fit statistics
cat("\nPooled fit statistics across", length(fit_stats_all), "imputations:\n")
print(pooled_fit_stats)
write.csv(pooled_fit_stats, "data/lca_fit_stats_imputed.csv", row.names = FALSE)

# ==============================================================================
# MODEL SELECTION PLOT
# ==============================================================================

elbow_plot <- ggplot(pooled_fit_stats, aes(x = NClasses)) +
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
    title = "Latent Class Analysis Model Selection (Multiple Imputation)",
    subtitle = "Lower values indicate better model fit (pooled across 5 imputations)",
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
ggsave("figures/lca_elbow_plot_imputed.png", elbow_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# FINAL MODEL SELECTION AND CLASS ASSIGNMENT
# ==============================================================================

# Select best model (lowest BIC)
best_model <- pooled_fit_stats[which.min(pooled_fit_stats$BIC), ]
cat("Best model based on pooled BIC:\n")
print(best_model)

# ---------------------------
# Pool class assignments across imputations
# ---------------------------
cat("\nPooling 3-class solutions across imputations using majority vote...\n")

n_participants <- nrow(m2hepprep_prep_combined)
n_imputations <- length(lca_imputed_results)

# Initialize matrix for class assignments
class_assignments <- matrix(NA, nrow = n_participants, ncol = n_imputations)

# Fill matrix with predicted classes from each imputation
for(i in 1:n_imputations){
  pred <- lca_imputed_results[[i]][[3]]$predclass
  n_lca <- if(is.vector(pred)) length(pred) else nrow(pred)
  class_assignments[1:n_lca, i] <- pred[1:n_lca]
}

# Majority vote across imputations
final_class_assignment <- apply(class_assignments, 1, function(x){
  if(all(is.na(x))) return(NA)
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
})

# Agreement rate
agreement_rate <- mean(apply(class_assignments, 1, function(x) length(unique(x)) == 1))
cat("Class assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n")

# ---------------------------
# Recode classes based on LCA sizes: 153, 121, 168
# ---------------------------
raw_counts <- table(final_class_assignment)

# Desired order of class sizes
desired_sizes <- c(153, 121, 168)

# Map raw class numbers to labels by matching counts
class_map <- c()
tmp_counts <- raw_counts
for(size in desired_sizes){
  # Find class number with closest count
  class_number <- as.numeric(names(tmp_counts))[which.min(abs(tmp_counts - size))]
  class_map <- c(class_map, class_number)
  tmp_counts <- tmp_counts[!names(tmp_counts) %in% class_number]
}

# Assign descriptive labels in correct order
class_labels <- c(
  "High Injecting/Low Sexual Risk",     # Class 1 (153 participants)
  "Moderate Injecting/High Sexual Risk",# Class 2 (121 participants)
  "Low Overall Risk"                     # Class 3 (168 participants)
)
final_class_assignment_recoded <- factor(final_class_assignment,
                                         levels = class_map,
                                         labels = class_labels) %>%
  relevel(ref = "High Injecting/Low Sexual Risk")

# Assign to dataset
m2hepprep_prep_combined$class_imputed <- final_class_assignment
m2hepprep_prep_combined$class_factor_imputed <- final_class_assignment_recoded

# ==============================================================================
# FINAL MODEL SELECTION AND CLASS ASSIGNMENT
# ==============================================================================

# Select best model (lowest BIC)
best_model <- pooled_fit_stats[which.min(pooled_fit_stats$BIC), ]
cat("Best model based on pooled BIC:\n")
print(best_model)

# ---------------------------
# Pool class assignments across imputations
# ---------------------------
cat("\nPooling 3-class solutions across imputations using majority vote...\n")

n_imputations <- length(lca_imputed_results)

# Use the number of participants included in the LCA (not the full dataset)
n_lca <- nrow(lca_imputed_results[[1]][[3]]$predclass)

# Initialize matrix for class assignments
class_assignments <- matrix(NA, nrow = n_lca, ncol = n_imputations)

# Fill matrix with predicted classes
for(i in 1:n_imputations){
  pred <- lca_imputed_results[[i]][[3]]$predclass
  class_assignments[, i] <- pred
}

# Majority vote across imputations
final_class_assignment <- apply(class_assignments, 1, function(x){
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
})

# Agreement rate
agreement_rate <- mean(apply(class_assignments, 1, function(x) length(unique(x)) == 1))
cat("Class assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n")

# ---------------------------
# Assign classes with descriptive labels
# ---------------------------
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined[1:n_lca, ]
m2hepprep_prep_combined_lca$class_imputed <- final_class_assignment

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  final_class_assignment,
  levels = 1:3,
  labels = c(
    "High Injecting/Low Sexual Risk",     
    "Moderate Injecting/High Sexual Risk",
    "Low Overall Risk"
  )
) %>% relevel(ref = "High Injecting/Low Sexual Risk")

# ---------------------------
# Inspect class distribution
# ---------------------------
cat("\nClass distribution (should match pooled_fit_stats):\n")
print(table(m2hepprep_prep_combined_lca$class_factor_imputed))

# ==============================================================================
# POISSON REGRESSION WITH IMPUTED CLASS ASSIGNMENTS
# ==============================================================================

# Unadjusted model
mod_class_imp <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

robust_se_imp <- sqrt(diag(vcovHC(mod_class_imp, type = "HC0")))
coefs_imp <- coeftest(mod_class_imp, vcov. = vcovHC(mod_class_imp, type = "HC0"))

poisson_class_results_imp <- data.frame(
  term = rownames(coefs_imp),
  estimate = exp(coefs_imp[, "Estimate"]),
  conf.low = exp(coefs_imp[, "Estimate"] - 1.96 * robust_se_imp),
  conf.high = exp(coefs_imp[, "Estimate"] + 1.96 * robust_se_imp),
  p.value = coefs_imp[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# Adjusted model
mod_class_adjusted_imp <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

robust_se_adj_imp <- sqrt(diag(vcovHC(mod_class_adjusted_imp, type = "HC0")))
coefs_adj_imp <- coeftest(mod_class_adjusted_imp, vcov. = vcovHC(mod_class_adjusted_imp, type = "HC0"))

poisson_class_adjusted_results_imp <- data.frame(
  term = rownames(coefs_adj_imp),
  estimate = exp(coefs_adj_imp[, "Estimate"]),
  conf.low = exp(coefs_adj_imp[, "Estimate"] - 1.96 * robust_se_adj_imp),
  conf.high = exp(coefs_adj_imp[, "Estimate"] + 1.96 * robust_se_adj_imp),
  p.value = coefs_adj_imp[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# ==============================================================================
# CREATE PREP INITIATION SUMMARY BY CLASS
# ==============================================================================

prep_by_class <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$prep_init
)

prep_by_class_prop <- prop.table(prep_by_class, 1)

prep_summary <- data.frame(
  class = rownames(prep_by_class),
  n_no = prep_by_class[, "No"],
  n_yes = prep_by_class[, "Yes"],
  prop_no = round(prep_by_class_prop[, "No"], 3),
  prop_yes = round(prep_by_class_prop[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# ==============================================================================
# SAVE RESULTS TO EXCEL
# ==============================================================================

write_xlsx(list(
  "Poisson_class_unadjusted_imputed" = poisson_class_results_imp,
  "Poisson_class_adjusted_imputed" = poisson_class_adjusted_results_imp,
  "Pooled_fit_statistics" = pooled_fit_stats,
  "PrEP_by_class" = prep_summary
), "data/poisson_class_results_imputed.xlsx")

cat("\nAnalysis completed using multiple imputation!\n")
cat("Results have been saved successfully to: data/poisson_class_results_imputed.xlsx\n")
