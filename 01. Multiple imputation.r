# ============================================================
# Load libraries and data
# ============================================================

# stop if script has an error
options(error = stop)

# libraries
pacman::p_load(dplyr, mice, writexl, readxl, poLCA, ggplot2, clue, sandwich, lmtest, MASS)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_combined.csv")

# check sex variables
with(m2hepprep_prep_combined, table(condom_1m, num_sex_partners_3m))

# ============================================================
# Define LCA variables
# ============================================================
lca_vars <- c(
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "num_sex_partners_3m", "condom_1m", 
  "sexwork_3m"
)

lca_vars_bin <- c(
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "sexwork_3m"
)

lca_vars_cat <- c(
  "num_sex_partners_3m",
  "condom_1m"
)

# ============================================================
# Auxiliary variables
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
# Create imputation dataset
# ============================================================
imputation_data <- m2hepprep_prep_combined[
  c(lca_vars, auxiliary_vars)
]

# ============================================================
# Make variables correct type and format for mice()
# ============================================================

# binary LCA vars
imputation_data <- imputation_data %>%
  mutate(across(
    all_of(lca_vars_bin),
    ~ factor(.)
  ))

# categorical LCA vars
imputation_data <- imputation_data %>%
  mutate(condom_1m = factor(
    condom_1m,
    levels = c(
      "No sex past 3 months",
      "No sex past month",
      "Never / Rarely / Some of the time",
      "Very often / Always"
    ),
    ordered = TRUE
  )) %>%
  mutate(num_sex_partners_3m = factor(
    num_sex_partners_3m,
    levels = c(0, 1, 2),
    labels = c(
      "No sex past 3 months",
      "One sex partner",
      "Two or more sex partners"
    ),
    ordered = TRUE
  ))

# auxiliary binary vars
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

# check variables are correct
stopifnot(
  all(sapply(imputation_data[lca_vars_bin], function(x)
    all(x %in% c(0, 1, NA)))
  )
)

# ============================================================
# Set imputation methods for binary and cat vars
# ============================================================
meth <- make.method(imputation_data)
meth[] <- ""

meth[lca_vars_bin] <- "logreg"  # binary vars
meth[lca_vars_cat] <- "polr"    # ordered factors

# ============================================================
# Set auxiliary and LCA vars as predictors
# ============================================================
pred <- make.predictorMatrix(imputation_data)
pred[,] <- 0
pred[lca_vars, lca_vars] <- 1
pred[lca_vars, auxiliary_vars] <- 1
diag(pred) <- 0

# ============================================================
# Run multiple imputation
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
# Define factor levels
# ============================================================

levels_condom <- c(
  "No sex past 3 months",
  "No sex past month",
  "Never / Rarely / Some of the time",
  "Very often / Always"
)

levels_nsp <- c(
  "No sex past 3 months",
  "One sex partner",
  "Two or more sex partners"
)

# ============================================================
# Manually code survey skip logic
# ============================================================

imputed_datasets_mice <- lapply(seq_len(imp$m), function(i) {
  df <- complete(imp, i)

  na_rows <- which(is.na(df$condom_1m) | is.na(df$num_sex_partners_3m))

  if (length(na_rows) > 0) {
    df$condom_1m[na_rows] <- "No sex past 3 months"
    df$num_sex_partners_3m[na_rows] <- "No sex past 3 months"
  }

  # recode based on skip logic
  df$condom_1m[df$num_sex_partners_3m == "No sex past 3 months"] <- "No sex past 3 months"
  df$num_sex_partners_3m[df$condom_1m == "No sex past 3 months"] <- "No sex past 3 months"

  # convert to ordered factors
  df$condom_1m <- factor(df$condom_1m, levels = levels_condom, ordered = TRUE)
  df$num_sex_partners_3m <- factor(df$num_sex_partners_3m, levels = levels_nsp, ordered = TRUE)

  df
})

# check that condom_1m logic matches num_sex_partners_3m
completed_data <- imputed_datasets_mice[[1]]
with(completed_data, table(condom_1m, num_sex_partners_3m))

# ============================================================
# Verify imputation
# ============================================================
completed_data <- imputed_datasets_mice[[1]]

# remaining NAs
print(colSums(is.na(completed_data[lca_vars])))

