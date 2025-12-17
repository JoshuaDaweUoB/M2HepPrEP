# ============================================================
# LOAD LIBRARIES and data
# ============================================================
pacman::p_load(dplyr, mice, writexl, readxl, poLCA, ggplot2, clue, sandwich, lmtest)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# Source the data processing script with echo = TRUE to print all lines
source("code/00. data processing.R", local = TRUE, echo = TRUE)

# ============================================================
# DEFINE LCA VARIABLES
# ============================================================
lca_vars <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "days_used_1m_3cat",
  "num_sex_partners_3m", "condom_1m", "condom_intent_6m",
  "sexwork_3m", "sexual_abuse_6m"
)

lca_vars_bin <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "sexwork_3m", "sexual_abuse_6m"
)

lca_vars_cat <- c(
  "days_used_1m_3cat",
  "num_sex_partners_3m",
  "condom_1m",
  "condom_intent_6m"
)

# ============================================================
# AUXILIARY VARIABLES
# ============================================================
auxiliary_vars <- c(
  "sdem_age_binary",
  "sdem_sex_binary",
  "sdem_reside",
  "oat_current",
  "incarc_6m_bin",
  "sdem_slep6m_binary"
)

# ============================================================
# CREATE IMPUTATION DATASET
# ============================================================
imputation_data <- m2hepprep_prep_combined[
  c(lca_vars, auxiliary_vars)
]

# ============================================================
# FIX VARIABLE TYPES FOR mice()
# ============================================================

# Binary LCA vars → numeric 0/1
imputation_data <- imputation_data %>%
  mutate(across(
    all_of(lca_vars_bin),
    ~ as.numeric(as.character(.))
  ))

# Categorical LCA vars → factor
imputation_data <- imputation_data %>%
  mutate(across(all_of(lca_vars_cat), as.factor))

# Auxiliary vars
imputation_data <- imputation_data %>%
  mutate(
    sdem_reside = factor(sdem_reside),
    across(
      c(sdem_age_binary, sdem_sex_binary,
        oat_current, incarc_6m_bin,
        sdem_slep6m_binary),
      ~ as.numeric(as.character(.))
    )
  )

# ============================================================
# SANITY CHECK
# ============================================================
stopifnot(
  all(sapply(imputation_data[lca_vars_bin], function(x)
    all(x %in% c(0, 1, NA)))
  )
)

# ============================================================
# DEFINE IMPUTATION METHODS
# ============================================================
meth <- make.method(imputation_data)
meth[] <- ""

meth[lca_vars_bin] <- "logreg"
meth[lca_vars_cat] <- "polyreg"

# ============================================================
# DEFINE PREDICTOR MATRIX
# ============================================================
pred <- make.predictorMatrix(imputation_data)
pred[,] <- 0
pred[lca_vars, lca_vars] <- 1
pred[lca_vars, auxiliary_vars] <- 1
diag(pred) <- 0

# ============================================================
# RUN MULTIPLE IMPUTATION
# ============================================================
set.seed(123)

imp <- mice(
  imputation_data,
  method = meth,
  predictorMatrix = pred,
  m = 100,
  maxit = 10,
  printFlag = TRUE
)

# ============================================================
# VERIFY IMPUTATION
# ============================================================
completed_data <- complete(imp, 1)

cat("Remaining NAs in LCA variables (should be 0):\n")
print(colSums(is.na(completed_data[lca_vars])))

# ============================================================
# STORE COMPLETED DATASETS FROM mice (NO post-hoc filling)
# ============================================================
imputed_datasets_mice <- lapply(seq_len(imp$m), function(i) {
  complete(imp, i)
})

# ============================================================
# PREPARE DATASETS FOR poLCA (factor → integer)
# ============================================================
prepare_for_poLCA <- function(df, vars_bin, vars_cat) {
  df <- df %>%
    mutate(
      # binary vars: ensure factor then integer {1,2}
      across(all_of(vars_bin), ~ as.integer(factor(.x, levels = c(0, 1)))),
      # categorical vars: factor then integer {1,...,K}
      across(all_of(vars_cat), ~ as.integer(factor(.x)))
    )
  df
}

imputed_datasets_lca <- lapply(imputed_datasets_mice, function(df) {
  prepare_for_poLCA(df, lca_vars_bin, lca_vars_cat)
})

# ============================================================
# SANITY CHECKS
# ============================================================

# Check first dataset
cat("\nLevels (binary vars):\n")
print(sapply(imputed_datasets_mice[[1]][lca_vars_bin], levels))

cat("\nLevels (categorical vars):\n")
print(sapply(imputed_datasets_mice[[1]][lca_vars_cat], levels))

# Confirm poLCA-ready encoding
cat("\nUnique values after conversion (should be integers >=1):\n")
print(lapply(imputed_datasets_lca[[1]][lca_vars], unique))

# Confirm no missing data
stopifnot(
  all(sapply(imputed_datasets_lca[[1]][lca_vars], function(x) sum(is.na(x)) == 0))
)

# ==========================================================================
# MULTIPLE IMPUTATION LATENT CLASS ANALYSIS (LCA)
# ==========================================================================

# ======================================================
# NUMBER OF IMPUTATIONS
# ======================================================
n_imp <- length(imputed_datasets_lca)

# ======================================================
# DEFINE LCA FORMULA
# ======================================================
lca_formula <- as.formula(
  paste0("cbind(", paste(lca_vars, collapse = ","), ") ~ 1")
)

# ======================================================
# FUNCTION TO CALCULATE FIT STATISTICS
# ======================================================
calculate_fit_stats <- function(lca_model, k, n) {
  sabic <- lca_model$bic - log(n) * (lca_model$npar - 1) / 2
  
  entropy <- NA
  if (!is.null(lca_model$posterior)) {
    p <- lca_model$posterior
    entropy <- 1 - sum(p * log(p + 1e-10)) / (n * log(k))
  }
  
  class_sizes <- table(lca_model$predclass)
  class_counts <- rep(NA, 5)
  class_counts[seq_along(class_sizes)] <- as.numeric(class_sizes)

  data.frame(
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

# ======================================================
# RUN LCA ON ALL IMPUTED DATASETS
# ======================================================
set.seed(123)

fit_stats_all <- vector("list", n_imp)

for (imp_idx in seq_len(n_imp)) {
  
  cat("Running LCA on imputed dataset", imp_idx, "...\n")
  
  fit_stats_imp <- vector("list", 5)
  
  for (k in 1:5) {
    lca_model <- poLCA(
      lca_formula,
      data    = imputed_datasets_lca[[imp_idx]],
      nclass  = k,
      maxiter = 1000,
      nrep    = 20,
      verbose = FALSE
    )
    
    fit_stats_imp[[k]] <-
      calculate_fit_stats(lca_model, k, nrow(imputed_datasets_lca[[imp_idx]]))
  }
  
  fit_stats_all[[imp_idx]] <- do.call(rbind, fit_stats_imp)
}

# ======================================================
# COMBINE FIT STATISTICS ACROSS IMPUTATIONS
# ======================================================
all_fit_stats <- do.call(rbind, lapply(seq_len(n_imp), function(i) {
  cbind(Imputation = i, fit_stats_all[[i]])
}))

# Save results
write.csv(
  all_fit_stats,
  "data/lca_fit_stats_imputed_all.csv",
  row.names = FALSE
)

# ======================================================
# SUMMARIZE FIT STATISTICS (MEDIAN ACROSS IMPUTATIONS)
# ======================================================
fit_summary <- all_fit_stats %>%
  group_by(NClasses) %>%
  summarise(
    AIC_med   = median(AIC, na.rm = TRUE),
    BIC_med   = median(BIC, na.rm = TRUE),
    SABIC_med = median(SABIC, na.rm = TRUE)
  )

# ======================================================
# ELBOW PLOT FOR MODEL SELECTION
# ======================================================
elbow_plot <- ggplot(fit_summary, aes(x = NClasses)) +
  geom_line(aes(y = AIC_med, color = "AIC"), linewidth = 1.2) +
  geom_point(aes(y = AIC_med, color = "AIC"), size = 3) +
  geom_line(aes(y = BIC_med, color = "BIC"), linewidth = 1.2) +
  geom_point(aes(y = BIC_med, color = "BIC"), size = 3) +
  geom_line(aes(y = SABIC_med, color = "SABIC"), linewidth = 1.2) +
  geom_point(aes(y = SABIC_med, color = "SABIC"), size = 3) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Number of latent classes",
    y = "Median information criterion across imputations",
    title = "LCA Model Selection with Multiple Imputation",
    color = "Criterion"
  ) +
  theme_minimal()

print(elbow_plot)

# ==============================================================================
# MODEL ENUMERATION ACROSS IMPUTATIONS
# ==============================================================================

fit_medians <- all_fit_stats %>%
  group_by(NClasses) %>%
  summarise(
    AIC_med   = median(AIC, na.rm = TRUE),
    BIC_med   = median(BIC, na.rm = TRUE),
    SABIC_med = median(SABIC, na.rm = TRUE),
    .groups = "drop"
  )

print(fit_medians)

# Select final number of classes (based on BIC/SABIC)
k_final <- 4

cat("\nSelected number of classes:", k_final, "\n")

# ==============================================================================
# FIT FINAL LCA MODEL (k = 4) ACROSS IMPUTATIONS
# ==============================================================================

n_imp <- length(imputed_datasets_lca)
n_participants <- nrow(imputed_datasets_lca[[1]])

lca_imputed_results <- vector("list", n_imp)

set.seed(123)

for (i in seq_len(n_imp)) {
  cat("Fitting final LCA (k =", k_final, ") on imputed dataset", i, "...\n")
  
  lca_imputed_results[[i]] <- poLCA(
    lca_formula,
    data    = imputed_datasets_lca[[i]],
    nclass  = k_final,
    maxiter = 1000,
    nrep    = 20,
    verbose = FALSE
  )
}

# ==============================================================================
# ALIGN CLASS LABELS ACROSS IMPUTATIONS
# ==============================================================================

library(clue)

class_assignments <- matrix(NA, nrow = n_participants, ncol = n_imp)

# Reference: first imputation
ref_classes <- lca_imputed_results[[1]]$predclass
class_assignments[, 1] <- ref_classes

align_classes <- function(pred, ref) {
  tab <- table(ref, pred)
  assignment <- solve_LSAP(tab, maximum = TRUE)
  sapply(pred, function(x) which(assignment == x))
}

for (i in 2:n_imp) {
  pred <- lca_imputed_results[[i]]$predclass
  class_assignments[, i] <- align_classes(pred, ref_classes)
}

# ==============================================================================
# POOL CLASS MEMBERSHIP (MAJORITY VOTE)
# ==============================================================================

final_class_assignment <- apply(class_assignments, 1, function(x) {
  as.numeric(names(which.max(table(x))))
})

# Agreement diagnostics
agreement_rate <- mean(apply(class_assignments, 1, function(x)
  length(unique(x)) == 1
))

cat("\nClass assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n\n")

# Inspect class distribution
print(table(final_class_assignment))

# ==============================================================================
# CREATE REFERENCE DATASET WITH FINAL CLASS
# ==============================================================================

df_ref <- imputed_datasets_lca[[1]]
df_ref$Class <- final_class_assignment

# Define the list of LCA variables
lca_vars_selected <- c(
  "syringe_share_6m_bin",
  "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin",
  "syringe_reuse_6m_bin",
  "days_used_1m_3cat",
  "inject_meth_6m",
  "inject_cocaine_6m",
  "sexwork_3m",
  "sexual_abuse_6m",
  "num_sex_partners_3m",
  "condom_1m",
  "condom_intent_6m"
)

# Make sure all LCA vars are factors (numeric LCA levels +1 are okay)
df_ref <- df_ref %>%
  mutate(across(all_of(lca_vars_selected), as.factor))

# Map variable labels using recode with default
df_ref <- df_ref %>%
  mutate(
    syringe_share_6m_bin  = recode(syringe_share_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_cooker_6m_bin = recode(syringe_cooker_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_loan_6m_bin   = recode(syringe_loan_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    syringe_reuse_6m_bin  = recode(syringe_reuse_6m_bin, "1" = "No", "2" = "Yes", .default = NA_character_),
    days_used_1m_3cat     = recode(days_used_1m_3cat, "1" = "0–14 days", "2" = "15–24 days", "3" = "25+ days", .default = NA_character_),
    inject_meth_6m        = recode(inject_meth_6m, "1" = "No", "2" = "Yes", .default = NA_character_),
    inject_cocaine_6m     = recode(inject_cocaine_6m, "1" = "No", "2" = "Yes", .default = NA_character_),
    sexwork_3m            = recode(sexwork_3m, "1" = "No", "2" = "Yes", .default = NA_character_),
    sexual_abuse_6m       = recode(sexual_abuse_6m, "1" = "No", "2" = "Yes", .default = NA_character_),
    num_sex_partners_3m   = recode(num_sex_partners_3m, "1" = "None", "2" = "One", "3" = "Two or more", .default = NA_character_),
    condom_1m             = recode(condom_1m,
                                   "1" = "No sex past month",
                                   "2" = "No sex past 3 months",
                                   "3" = "Very often / Always",
                                   "4" = "Never / Rarely / Some of the time",
                                   .default = NA_character_),
    condom_intent_6m      = recode(condom_intent_6m,
                                   "1" = "Disagree / Strongly Disagree",
                                   "2" = "Neither agree nor disagree",
                                   "3" = "Agree / Strongly Agree",
                                   .default = NA_character_)
  )

# Compute counts and percentages per class
freq_by_class <- lca_vars_selected %>%
  lapply(function(var) {
    df_ref %>%
      group_by(Class, !!sym(var)) %>%
      summarise(Count = n(), .groups = "drop") %>%
      rename(Level_Label = !!sym(var)) %>%
      mutate(Variable = var) %>%
      select(Variable, Level_Label, Class, Count)
  }) %>%
  bind_rows() %>%
  group_by(Variable, Class) %>%
  mutate(
    Total_Class = sum(Count),
    Percent = round(Count / Total_Class * 100, 1)
  ) %>%
  ungroup()

# Pivot so each row is a variable + level, with columns for each class
wide_table <- freq_by_class %>%
  pivot_wider(
    id_cols = c(Variable, Level_Label),         # these define the rows
    names_from = Class,                         # Class values become columns
    values_from = c(Count, Percent),            # multiple values per column
    names_sep = "_Class"
  ) %>%
  arrange(Variable, Level_Label)

# Save to Excel
write.xlsx(wide_table, "data/class_patterns_categorical_wide.xlsx", rowNames = FALSE)

# Assign classes
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined
m2hepprep_prep_combined_lca$class_imputed <- final_class_assignment

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  final_class_assignment,
  levels = 1:4,
  labels = c(
    "High Injecting / High Sexual Risk",      # Class 1
    "High Injecting / High Sexual Risk", # Class 2
    "Low Overall Risk",                      # Class 3
    "Low Injecting / High Sexual Risk"      # Class 4
  )
)

# ---------------------------
# Inspect class distribution
# ---------------------------
cat("\nClass distribution (should match pooled assignments):\n")
print(table(m2hepprep_prep_combined_lca$class_factor_imputed))

# ==============================================================================
# POISSON REGRESSION WITH IMPUTED CLASS ASSIGNMENTS
# ==============================================================================

# Set Class 2 as the reference category
m2hepprep_prep_combined_lca$class_factor_imputed <-
  relevel(m2hepprep_prep_combined_lca$class_factor_imputed,
          ref = "High Injecting / Low Sexual Risk")

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

# Ensure both "No" and "Yes" columns exist
if(!all(c("No", "Yes") %in% colnames(prep_by_class))) {
  missing_cols <- setdiff(c("No", "Yes"), colnames(prep_by_class))
  for(col in missing_cols) prep_by_class <- cbind(prep_by_class, 0)
  colnames(prep_by_class) <- c("No", "Yes")
}

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
  "Pooled_fit_statistics" = fit_medians,       # updated from pooled_fit_stats
  "PrEP_by_class" = prep_summary
), "data/poisson_class_results_imputed.xlsx")

cat("\nAnalysis completed using multiple imputation!\n")
