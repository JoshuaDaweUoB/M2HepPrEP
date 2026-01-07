# ============================================================
# Load libraries and data
# ============================================================

# load libraries
pacman::p_load(dplyr, tidyr, writexl, poLCA, ggplot2, clue, MASS)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_combined.csv")

options(error = stop)

# ============================================================
# Define LCA variables
# ============================================================

lca_vars <- c(
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "num_sex_partners_3m", "condom_1m",
  "sexwork_3m"
)

# ============================================================
# Complete case coding
# ============================================================

# ensure poLCA encoding
m2hepprep_prep_combined[lca_vars] <-
  lapply(m2hepprep_prep_combined[lca_vars], function(x) {
    x <- factor(x)
    as.integer(x)
  })

# enforce complete cases on LCA indicators
m2hepprep_prep_combined_cc <-
  m2hepprep_prep_combined %>%
  drop_na(all_of(lca_vars))

n <- nrow(m2hepprep_prep_combined_cc)

cat("\nSample size after complete-case restriction:", n, "\n")

# ============================================================
# Run LCA
# ============================================================

lca_formula <- as.formula(
  paste("cbind(", paste(lca_vars, collapse = ", "), ") ~ 1")
)

set.seed(123)

fit_stats <- vector("list", 5)

n <- nrow(m2hepprep_prep_combined)

for (k in 1:5) {

  cat("Running LCA with", k, "classes...\n")

  lca_model <- poLCA(
    lca_formula,
    data    = m2hepprep_prep_combined,
    nclass  = k,
    maxiter = 1000,
    nrep    = 20,
    verbose = FALSE
  )

# ============================================================
# Calculate fit statistics
# ============================================================
sabic <- lca_model$bic - log(n) * (lca_model$npar - 1) / 2

  entropy <- NA
  if (!is.null(lca_model$posterior)) {
    p <- lca_model$posterior
    entropy <- 1 - sum(p * log(p + 1e-10)) / (n * log(k))
  }

  class_sizes <- table(lca_model$predclass)
  class_counts <- rep(NA, 5)
  class_counts[seq_along(class_sizes)] <- as.numeric(class_sizes)

  fit_stats[[k]] <- data.frame(
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
  )
}

fit_stats <- do.call(rbind, fit_stats)

print(fit_stats)

# write to Excel
write_xlsx(fit_stats, "data/lca_fit_stats_CC.xlsx")

# ============================================================
# Elbow plot
# ============================================================

elbow_plot <- ggplot(fit_stats, aes(x = NClasses)) +
  geom_line(aes(y = AIC, color = "AIC"), linewidth = 1.2) +
  geom_point(aes(y = AIC, color = "AIC"), size = 3) +
  geom_line(aes(y = BIC, color = "BIC"), linewidth = 1.2) +
  geom_point(aes(y = BIC, color = "BIC"), size = 3) +
  geom_line(aes(y = SABIC, color = "SABIC"), linewidth = 1.2) +
  geom_point(aes(y = SABIC, color = "SABIC"), size = 3) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Number of latent classes",
    y = "Information criterion",
    title = "Complete-Case LCA Sensitivity Analysis",
    color = "Criterion"
  ) +
  theme_minimal()

print(elbow_plot)

# ============================================================
# Final LCA with four classes
# ============================================================

k_final <- 4
set.seed(123)

lca_cc_final <- poLCA(
  lca_formula,
  data    = m2hepprep_prep_combined_cc,
  nclass  = k_final,
  maxiter = 1000,
  nrep    = 20,
  verbose = FALSE
)

lca_cc_final

# ==============================================================================
# Single dataset with final class assignments from lca_cc_final
# ==============================================================================

# Use the complete-case dataset
df_ref <- m2hepprep_prep_combined_cc

# Add final class assignments from lca_cc_final
df_ref$Class <- factor(
  lca_cc_final$predclass,
  labels = c(
    "Class 1", "Class 2", "Class 3", "Class 4", "Class 5"
  )[1:length(unique(lca_cc_final$predclass))]
)

# List of LCA variables
lca_vars_selected <- c(
  "syringe_share_6m_bin",
  "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin",
  "syringe_reuse_6m_bin",
  "sexwork_3m",
  "num_sex_partners_3m",
  "condom_1m"
)

# Convert LCA variables to factors
df_ref <- df_ref %>%
  mutate(across(all_of(lca_vars_selected), as.factor))

# Apply variable labels using recode
df_ref <- df_ref %>%
  mutate(
    syringe_share_6m_bin  = recode(syringe_share_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_cooker_6m_bin = recode(syringe_cooker_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_loan_6m_bin   = recode(syringe_loan_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_reuse_6m_bin  = recode(syringe_reuse_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    sexwork_3m            = recode(sexwork_3m, "1" = "No", "2" = "Yes", .default = NA_character_),
    num_sex_partners_3m   = recode(num_sex_partners_3m, "1" = "None", "2" = "One", "3" = "Two or more", .default = NA_character_),
    condom_1m             = recode(condom_1m,
                                   "1" = "No sex past month",
                                   "2" = "No sex past 3 months",
                                   "3" = "Very often / Always",
                                   "4" = "Never / Rarely / Some of the time",
                                   .default = NA_character_)
  )

# Counts and percentages per class
freq_by_class <- lca_vars_selected %>%
  lapply(function(var) {
    df_ref %>%
      dplyr::group_by(Class, !!rlang::sym(var)) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
      dplyr::rename(Level_Label = !!rlang::sym(var)) %>%
      dplyr::mutate(Variable = var) %>%
      dplyr::select(Variable, Level_Label, Class, Count)
  }) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(Variable, Class) %>%
  dplyr::mutate(
    Total_Class = sum(Count),
    Percent = round(Count / Total_Class * 100, 1)
  ) %>%
  dplyr::ungroup()

# Reshape so variables are rows
wide_table <- freq_by_class %>%
  tidyr::pivot_wider(
    id_cols = c(Variable, Level_Label),
    names_from = Class,
    values_from = c(Count, Percent),
    names_sep = "_"
  ) %>%
  dplyr::arrange(Variable, Level_Label)

# Save table
writexl::write_xlsx(wide_table, "data/class_patterns_categorical_wide_finalCC.xlsx")

# ==============================================================================
# Assign final class from lca_cc_final to the complete-case dataset
# ==============================================================================

m2hepprep_prep_combined_lca <- m2hepprep_prep_combined_cc

# numeric class assignment
m2hepprep_prep_combined_lca$class_imputed <- lca_cc_final$predclass

# factor with meaningful labels
m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  lca_cc_final$predclass,
  levels = 1:k_final,  # ensure matches the number of classes in final model
  labels = c(
    "Low Injecting / High Sexual Risk",        # Class 1
    "Low Overall Risk",                        # Class 2
    "High Injecting / High Sexual Risk",       # Class 3
    "High Injecting / Low Sexual Risk"         # Class 4
  )[1:k_final]  # in case k_final < 4
)

# ---------------------------
# Inspect class distribution
# ---------------------------
cat("\nClass distribution (final complete-case LCA):\n")
print(table(m2hepprep_prep_combined_lca$class_factor_imputed))

# save data
write.csv(
  m2hepprep_prep_combined_lca,
  "data/m2hepprep_combined_lca_finalCC.csv",
  row.names = FALSE
)

# ==============================================================================
# Poisson regression with final complete-case LCA
# ==============================================================================

# Load necessary packages for robust SEs
pacman::p_load(sandwich, lmtest, writexl, dplyr)

# Use the complete-case dataset with class assignments
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined_cc

# Add numeric and factor class variables from final LCA
m2hepprep_prep_combined_lca$class_imputed <- lca_cc_final$predclass

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  lca_cc_final$predclass,
  levels = 1:k_final,
  labels = c(
    "Low Injecting / High Sexual Risk",        # Class 1
    "Low Overall Risk",                        # Class 2
    "High Injecting / High Sexual Risk",       # Class 3
    "High Injecting / Low Sexual Risk"         # Class 4
  )[1:k_final]
)

# Set reference class
m2hepprep_prep_combined_lca$class_factor_imputed <- relevel(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  ref = "High Injecting / Low Sexual Risk"
)

# ---------------------------
# Unadjusted Poisson regression
# ---------------------------
mod_class_finalCC <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

robust_se_finalCC <- sqrt(diag(vcovHC(mod_class_finalCC, type = "HC0")))
coefs_finalCC <- coeftest(mod_class_finalCC, vcov. = vcovHC(mod_class_finalCC, type = "HC0"))

poisson_class_results_finalCC <- data.frame(
  term = rownames(coefs_finalCC),
  estimate = exp(coefs_finalCC[, "Estimate"]),
  conf.low = exp(coefs_finalCC[, "Estimate"] - 1.96 * robust_se_finalCC),
  conf.high = exp(coefs_finalCC[, "Estimate"] + 1.96 * robust_se_finalCC),
  p.value = coefs_finalCC[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# ---------------------------
# Adjusted Poisson regression
# ---------------------------
mod_class_adj_finalCC <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

robust_se_adj_finalCC <- sqrt(diag(vcovHC(mod_class_adj_finalCC, type = "HC0")))
coefs_adj_finalCC <- coeftest(mod_class_adj_finalCC, vcov. = vcovHC(mod_class_adj_finalCC, type = "HC0"))

poisson_class_adjusted_results_finalCC <- data.frame(
  term = rownames(coefs_adj_finalCC),
  estimate = exp(coefs_adj_finalCC[, "Estimate"]),
  conf.low = exp(coefs_adj_finalCC[, "Estimate"] - 1.96 * robust_se_adj_finalCC),
  conf.high = exp(coefs_adj_finalCC[, "Estimate"] + 1.96 * robust_se_adj_finalCC),
  p.value = coefs_adj_finalCC[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# ---------------------------
# PrEP initiation summary by class
# ---------------------------
prep_by_class_finalCC <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$prep_init
)

# Ensure both "No" and "Yes" columns exist
if (!all(c("No", "Yes") %in% colnames(prep_by_class_finalCC))) {
  missing_cols <- setdiff(c("No", "Yes"), colnames(prep_by_class_finalCC))
  for (col in missing_cols) prep_by_class_finalCC <- cbind(prep_by_class_finalCC, 0)
  colnames(prep_by_class_finalCC) <- c("No", "Yes")
}

prep_by_class_prop_finalCC <- prop.table(prep_by_class_finalCC, 1)

prep_summary_finalCC <- data.frame(
  class = rownames(prep_by_class_finalCC),
  n_no = prep_by_class_finalCC[, "No"],
  n_yes = prep_by_class_finalCC[, "Yes"],
  prop_no = round(prep_by_class_prop_finalCC[, "No"], 3),
  prop_yes = round(prep_by_class_prop_finalCC[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# ---------------------------
# Save results to Excel
# ---------------------------
write_xlsx(
  list(
    "Poisson_class_unadjusted_finalCC" = poisson_class_results_finalCC,
    "Poisson_class_adjusted_finalCC"   = poisson_class_adjusted_results_finalCC,
    "PrEP_by_class"                    = prep_summary_finalCC
  ),
  "data/poisson_class_results_finalCC.xlsx"
)

# ==============================================================================
# Risk perception and PrEP initiation — COMPLETE CASE
# ==============================================================================

pacman::p_load(sandwich, lmtest, nnet, dplyr, writexl)

# Use complete-case dataset with final LCA assignments
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined_cc

# Add numeric and factor class variables from lca_cc_final
m2hepprep_prep_combined_lca$class_imputed <- lca_cc_final$predclass
m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  lca_cc_final$predclass,
  levels = 1:k_final,
  labels = c(
    "High Injecting / Low Sexual Risk",  # Class 1
    "High Injecting / High Sexual Risk", # Class 2
    "Low Overall Risk",                  # Class 3
    "Low Injecting / High Sexual Risk"   # Class 4
  )[1:k_final]
)

# ---------------------------
# Poisson regression: HIV risk perception -> PrEP initiation
# ---------------------------

# Set reference level
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <- 
  factor(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat)
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <- 
  relevel(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
          ref = "Unlikely/Very Unlikely")

# Unadjusted model
mod_prep_3cat_finalCC <- glm(
  prep_init_num ~ hiv_risk_perception_3cat + sdem_reside + rand_arm,
  data   = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

vc_3cat_finalCC <- vcovHC(mod_prep_3cat_finalCC, type = "HC0")
ct_3cat_finalCC <- coeftest(mod_prep_3cat_finalCC, vcov. = vc_3cat_finalCC)
se_3cat_finalCC <- sqrt(diag(vc_3cat_finalCC))

res_prep_3cat_finalCC <- data.frame(
  term      = rownames(ct_3cat_finalCC),
  estimate  = exp(ct_3cat_finalCC[, "Estimate"]),
  conf.low  = exp(ct_3cat_finalCC[, "Estimate"] - 1.96 * se_3cat_finalCC),
  conf.high = exp(ct_3cat_finalCC[, "Estimate"] + 1.96 * se_3cat_finalCC),
  p.value   = ct_3cat_finalCC[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# Adjusted model
mod_prep_3cat_adj_finalCC <- glm(
  prep_init_num ~ hiv_risk_perception_3cat + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data   = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

vc_3cat_adj_finalCC <- vcovHC(mod_prep_3cat_adj_finalCC, type = "HC0")
ct_3cat_adj_finalCC <- coeftest(mod_prep_3cat_adj_finalCC, vcov. = vc_3cat_adj_finalCC)
se_3cat_adj_finalCC <- sqrt(diag(vc_3cat_adj_finalCC))

res_prep_3cat_adj_finalCC <- data.frame(
  term      = rownames(ct_3cat_adj_finalCC),
  estimate  = exp(ct_3cat_adj_finalCC[, "Estimate"]),
  conf.low  = exp(ct_3cat_adj_finalCC[, "Estimate"] - 1.96 * se_3cat_adj_finalCC),
  conf.high = exp(ct_3cat_adj_finalCC[, "Estimate"] + 1.96 * se_3cat_adj_finalCC),
  p.value   = ct_3cat_adj_finalCC[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# ---------------------------
# Class membership models: HIV risk perception -> LCA class
# ---------------------------

# Reference class
m2hepprep_prep_combined_lca$class_factor_imputed <- 
  droplevels(relevel(m2hepprep_prep_combined_lca$class_factor_imputed,
                     ref = "Low Overall Risk"))

# Unadjusted multinomial model
mod_cls_3cat_finalCC <- nnet::multinom(
  class_factor_imputed ~ hiv_risk_perception_3cat + sdem_reside + rand_arm,
  data  = m2hepprep_prep_combined_lca,
  trace = FALSE
)

sm3_finalCC <- summary(mod_cls_3cat_finalCC)

res_cls_3cat_finalCC <- do.call(
  rbind,
  lapply(rownames(sm3_finalCC$coefficients), function(cls) {
    est <- sm3_finalCC$coefficients[cls, ]
    se  <- sm3_finalCC$standard.errors[cls, ]
    data.frame(
      contrast  = paste0(cls, " vs Low Overall Risk"),
      term      = names(est),
      estimate  = exp(est),
      conf.low  = exp(est - 1.96 * se),
      conf.high = exp(est + 1.96 * se),
      p.value   = 2 * pnorm(-abs(est / se)),
      stringsAsFactors = FALSE
    )
  })
)

# Adjusted multinomial model
mod_cls_3cat_adj_finalCC <- nnet::multinom(
  class_factor_imputed ~ hiv_risk_perception_3cat + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data  = m2hepprep_prep_combined_lca,
  trace = FALSE
)

sm3_adj_finalCC <- summary(mod_cls_3cat_adj_finalCC)

res_cls_3cat_adj_finalCC <- do.call(
  rbind,
  lapply(rownames(sm3_adj_finalCC$coefficients), function(cls) {
    est <- sm3_adj_finalCC$coefficients[cls, ]
    se  <- sm3_adj_finalCC$standard.errors[cls, ]
    data.frame(
      contrast  = paste0(cls, " vs Low Overall Risk"),
      term      = names(est),
      estimate  = exp(est),
      conf.low  = exp(est - 1.96 * se),
      conf.high = exp(est + 1.96 * se),
      p.value   = 2 * pnorm(-abs(est / se)),
      stringsAsFactors = FALSE
    )
  })
)

# ---------------------------
# PrEP initiation summary by HIV risk perception
# ---------------------------
prep_by_rp_3cat_finalCC <- table(
  m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
  m2hepprep_prep_combined_lca$prep_init
)

if (!all(c("No", "Yes") %in% colnames(prep_by_rp_3cat_finalCC))) {
  missing_cols <- setdiff(c("No", "Yes"), colnames(prep_by_rp_3cat_finalCC))
  for (col in missing_cols) prep_by_rp_3cat_finalCC <- cbind(prep_by_rp_3cat_finalCC, 0)
  colnames(prep_by_rp_3cat_finalCC) <- c("No", "Yes")
}

prep_by_rp_3cat_prop_finalCC <- prop.table(prep_by_rp_3cat_finalCC, 1)

prep_summary_rp_3cat_finalCC <- data.frame(
  risk_perception = rownames(prep_by_rp_3cat_finalCC),
  n_no    = prep_by_rp_3cat_finalCC[, "No"],
  n_yes   = prep_by_rp_3cat_finalCC[, "Yes"],
  prop_no = round(prep_by_rp_3cat_prop_finalCC[, "No"], 3),
  prop_yes= round(prep_by_rp_3cat_prop_finalCC[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# ---------------------------
# Risk perception distribution within class
# ---------------------------
rp_by_class_3cat_finalCC <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$hiv_risk_perception_3cat
)

rp_by_class_3cat_prop_finalCC <- prop.table(rp_by_class_3cat_finalCC, 1)

rp_class_summary_3cat_finalCC <- do.call(
  rbind,
  lapply(rownames(rp_by_class_3cat_finalCC), function(cls) {
    data.frame(
      class = cls,
      risk_perception = colnames(rp_by_class_3cat_finalCC),
      n = as.numeric(rp_by_class_3cat_finalCC[cls, ]),
      prop = round(as.numeric(rp_by_class_3cat_prop_finalCC[cls, ]), 3),  # fixed
      stringsAsFactors = FALSE
    )
  })
)

# ---------------------------
# Save results
# ---------------------------
write_xlsx(
  list(
    PrepInit_RP_3cat_Unadj      = res_prep_3cat_finalCC,
    PrepInit_RP_3cat_Adj        = res_prep_3cat_adj_finalCC,
    ClassMem_RP_3cat_Unadj      = res_cls_3cat_finalCC,
    ClassMem_RP_3cat_Adj        = res_cls_3cat_adj_finalCC,
    PrepInit_Summary_RP_3cat    = prep_summary_rp_3cat_finalCC,
    RP_Distribution_By_Class     = rp_class_summary_3cat_finalCC
  ),
  "data/risk_perception_models_3cat_finalCC.xlsx"
)
