# ============================================================
# LOAD LIBRARIES
# ============================================================
library(dplyr)
library(mice)
library(writexl)
library(dplyr)
library(poLCA)
library(ggplot2)
library(clue)

# Run the 00. data processing script
source("code/00. data processing.R", local = TRUE)

# ============================================================
# DEFINE LCA VARIABLES
# ============================================================
lca_vars <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "days_used_1m_3cat",
  "num_sex_partners_3m", "condom_1m", "condom_intent_6m",
  "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m"
)

lca_vars_bin <- c(
  "inject_meth_6m", "inject_cocaine_6m",
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "sexwork_3m", "bought_sex_3m", "sexual_abuse_6m"
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

cat("Remaining NAs in LCA variables:\n")
print(colSums(is.na(completed_data[lca_vars])))

imputed_datasets <- lapply(1:imp$m, function(i) {
  df <- complete(imp, i)
  
  df[lca_vars_cat] <- lapply(df[lca_vars_cat], function(x) {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  })
  
  df[lca_vars_bin] <- lapply(df[lca_vars_bin], function(x) {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  })
  
  df  # return the filled dataset
})

# ============================================================
# CONVERT BACK TO FACTORS FOR LCA
# ============================================================
imputed_datasets <- lapply(imputed_datasets, function(df) {
  df <- df %>%
    mutate(
      across(all_of(lca_vars_bin), ~ factor(.x, levels = c(0,1))),
      across(all_of(lca_vars_cat), as.factor)
    )
  df
})

# Check first dataset
sapply(imputed_datasets[[1]][lca_vars_bin], levels)  # binary vars
sapply(imputed_datasets[[1]][lca_vars_cat], levels)  # categorical vars

# Optionally check all datasets in a loop
for(i in 1:length(imputed_datasets)){
  cat("Dataset", i, "\n")
  print(sapply(imputed_datasets[[i]][lca_vars_bin], levels))
  print(sapply(imputed_datasets[[i]][lca_vars_cat], levels))
}

sapply(imputed_datasets[[1]][lca_vars], function(x) sum(is.na(x)))  # should all be 0

# ==========================================================================
# MULTIPLE IMPUTATION LATENT CLASS ANALYSIS (LCA)
# ==========================================================================

# ======================================================
# NUMBER OF IMPUTATIONS
# ======================================================
n_imp <- imp$m

# ======================================================
# PREPARE COMPLETED DATASETS FOR LCA
# ======================================================
prepare_for_poLCA <- function(df, vars) {
  df[vars] <- lapply(df[vars], function(x) {
    x <- factor(x)  # convert to factor
    
    # Replace NAs with mode, if mode exists
    tbl <- table(x)
    if(length(tbl) == 0) {
      mode_val <- NA
    } else {
      mode_val <- names(sort(tbl, decreasing = TRUE))[1]
    }
    
    if(!is.na(mode_val)) x[is.na(x)] <- mode_val
    
    as.integer(x)  # convert factor to integer starting at 1
  })
  df
}

imputed_datasets <- lapply(1:n_imp, function(i) {
  completed_data <- complete(imp, i)
  prepare_for_poLCA(completed_data, lca_vars)
})

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
  class_counts[1:length(class_sizes)] <- as.numeric(class_sizes)

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

for (imp_idx in 1:n_imp) {
  cat("Running LCA on imputed dataset", imp_idx, "...\n")
  fit_stats_imp <- list()
  for (k in 1:5) {
    lca_model <- poLCA(
      lca_formula,
      data = imputed_datasets[[imp_idx]],
      nclass = k,
      maxiter = 1000,
      nrep = 20,
      verbose = FALSE
    )
    fit_stats_imp[[k]] <- calculate_fit_stats(lca_model, k, nrow(imputed_datasets[[imp_idx]]))
  }
  fit_stats_all[[imp_idx]] <- do.call(rbind, fit_stats_imp)
}

# ======================================================
# COMBINE FIT STATISTICS ACROSS IMPUTATIONS
# ======================================================
all_fit_stats <- do.call(rbind, lapply(1:n_imp, function(i) {
  cbind(Imputation = i, fit_stats_all[[i]])
}))

# Save results
write.csv(all_fit_stats, "data/lca_fit_stats_imputed_all.csv", row.names = FALSE)

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
# FINAL MODEL SELECTION AND CLASS ASSIGNMENT
# ==============================================================================

fit_medians <- all_fit_stats %>%
  group_by(NClasses) %>%
  summarise(
    AIC_med   = median(AIC, na.rm = TRUE),
    BIC_med   = median(BIC, na.rm = TRUE),
    SABIC_med = median(SABIC, na.rm = TRUE)
  )

# Display median fit statistics
print(fit_medians)

# ---------------------------
# Pool class assignments across imputations
# ---------------------------
lca_imputed_results <- vector("list", n_imp)

for (i in 1:n_imp) {
  cat("Running LCA on imputed dataset", i, "...\n")
  lca_models <- list()
  for (k in 1:5) {
    lca_models[[k]] <- poLCA(
      lca_formula,
      data = imputed_datasets[[i]],
      nclass = k,
      maxiter = 1000,
      nrep = 20,
      verbose = FALSE
    )
  }
  lca_imputed_results[[i]] <- lca_models
}

n_participants <- nrow(imputed_datasets[[1]])
n_imputations <- length(lca_imputed_results)

class_assignments <- matrix(NA, nrow = n_participants, ncol = n_imputations)

for(i in 1:n_imputations){
  pred <- lca_imputed_results[[i]][[4]]$predclass
  class_assignments[, i] <- pred
}

# Majority vote
final_class_assignment <- apply(class_assignments, 1, function(x){
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
})

# Agreement rate
agreement_rate <- mean(apply(class_assignments, 1, function(x) length(unique(x)) == 1))
cat("Class assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n")

# ==============================================================================
# FINAL MODEL SELECTION AND CLASS ASSIGNMENT
# ==============================================================================

# Select best model (lowest BIC)
best_model <- pooled_fit_stats[which.min(pooled_fit_stats$BIC), ]
cat("Best model based on pooled BIC:\n")
print(best_model)

# ---------------------------
# Pool class assignments across imputations (with label alignment)
# ---------------------------

n_participants <- nrow(imputed_datasets[[1]])
n_imputations <- length(lca_imputed_results)

# Initialize matrix for class assignments
class_assignments <- matrix(NA, nrow = n_participants, ncol = n_imputations)

# Reference classes: first imputation, chosen number of classes (e.g., 4)
ref_classes <- lca_imputed_results[[1]][[4]]$predclass
class_assignments[,1] <- ref_classes

# Function to align predicted classes to reference
align_classes <- function(pred, ref) {
  tbl <- table(ref, pred)
  # solve linear sum assignment to maximize agreement
  assignment <- solve_LSAP(tbl, maximum = TRUE)
  # remap pred to aligned class numbers
  aligned <- sapply(pred, function(x) which(assignment == x))
  return(aligned)
}

# Align other imputations to reference
for(i in 2:n_imputations){
  pred <- lca_imputed_results[[i]][[4]]$predclass
  class_assignments[,i] <- align_classes(pred, ref_classes)
}

# Majority vote across imputations
final_class_assignment <- apply(class_assignments, 1, function(x){
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
})

# Agreement rate
agreement_rate <- mean(apply(class_assignments, 1, function(x) length(unique(x)) == 1))

cat("Class assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n")

# Optional: inspect class distribution
table(final_class_assignment)

# ---------------------------
# Create class descriptions
# ---------------------------

# Use the first imputed dataset as reference
df_ref <- imputed_datasets[[1]]  
df_ref$Class <- final_class_assignment  # Add final class assignment

# Step 1: Compute counts and percentages
freq_by_class <- lca_vars %>%
  lapply(function(var) {
    df_ref %>%
      group_by(Class, !!sym(var)) %>%
      summarise(Count = n(), .groups = "drop") %>%
      rename(Level = !!sym(var)) %>%
      mutate(Variable = var) %>%
      select(Variable, Level, Class, Count)
  }) %>%
  bind_rows() %>%
  group_by(Variable, Level) %>%
  mutate(Total = sum(Count),
         Percent = round(Count / Total * 100, 1)) %>%
  ungroup()

# Step 2: Pivot to wide format (classes as columns)
wide_table <- freq_by_class %>%
  select(Variable, Level, Class, Count, Percent) %>%
  pivot_wider(
    names_from = Class,
    values_from = c(Count, Percent),
    names_sep = "_Class"
  ) %>%
  arrange(Variable, Level)

# Step 3: Save to Excel
write.xlsx(wide_table, "data/class_patterns_categorical_wide.xlsx", rowNames = FALSE)

# Preview
wide_table



library(dplyr)
library(tidyr)
library(openxlsx)

# Step 1: Attach final class assignments to all imputed datasets
imputed_with_class <- lapply(imputed_datasets, function(df) {
  df$Class <- final_class_assignment
  df
})

# Step 2: Compute counts for each variable level by class per dataset
freq_list <- lapply(imputed_with_class, function(df) {
  lapply(lca_vars, function(var) {
    table(
      Class = factor(df$Class, levels = sort(unique(final_class_assignment))),
      Value = factor(df[[var]], levels = sort(unique(unlist(lapply(imputed_datasets, `[[`, var)))))
    )
  })
})

# Step 3: Sum counts across all imputed datasets
pooled_freq <- setNames(lapply(lca_vars, function(var) {
  Reduce(`+`, lapply(freq_list, function(x) x[[var]]))
}), lca_vars)

# Step 4: Convert to long format and compute percentages within each class
pooled_df <- lapply(names(pooled_freq), function(var) {
  tbl <- pooled_freq[[var]]
  df <- as.data.frame(as.table(tbl))
  colnames(df) <- c("Class", "Level", "Count")
  df$Variable <- var
  df <- df %>%
    group_by(Variable, Class) %>%
    mutate(Percent = round(Count / sum(Count) * 100, 1)) %>%
    ungroup() %>%
    select(Variable, Level, Class, Count, Percent)
  df
}) %>% bind_rows()

# Step 5: Pivot to wide format for readability
wide_table <- pooled_df %>%
  pivot_wider(
    names_from = Class,
    values_from = c(Count, Percent),
    names_sep = "_Class"
  ) %>%
  arrange(Variable, Level)

# Step 6: Save to Excel
write.xlsx(wide_table, "class_patterns_categorical_pooled.xlsx", rowNames = FALSE)

# Preview
wide_table








# Assign classes
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined
m2hepprep_prep_combined_lca$class_imputed <- final_class_assignment

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  final_class_assignment,
  levels = 1:4,
  labels = c(
    "High Injecting / High Sexual Risk",      # Class 1
    "High Injecting / Low Sexual Risk", # Class 2
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

library(sandwich)
library(lmtest)

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
cat("Results have been saved successfully to: data/poisson_class_results_imputed.xlsx\n")

