# ============================================================
# Load libraries and data
# ============================================================

# stop if script has an error
options(error = stop)

# libraries
pacman::p_load(dplyr, mice, writexl, readxl, poLCA, ggplot2, clue, sandwich, lmtest, MASS)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# ==============================================================================
# Poisson regression with imputed class assignments
# ==============================================================================

# load data
m2hepprep_prep_combined_lca <- read.csv("data/m2hepprep_combined_lca.csv")

# high injecting low sexual risk as the reference category
m2hepprep_prep_combined_lca$class_factor_imputed <-
  factor(
    m2hepprep_prep_combined_lca$class_factor_imputed,
    ordered = FALSE
  )

m2hepprep_prep_combined_lca$class_factor_imputed <-
  relevel(
    m2hepprep_prep_combined_lca$class_factor_imputed,
    ref = "High Injecting / Low Sexual Risk"
  )

# unadjusted model
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

# adjusted model
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
# Create PrEP initiation summary by class
# ==============================================================================

prep_by_class <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$prep_init
)

# ensure both "No" and "Yes" columns exist
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
# Save results to Excel
# ==============================================================================

write_xlsx(list(
  "Poisson_class_unadjusted_imputed" = poisson_class_results_imp,
  "Poisson_class_adjusted_imputed" = poisson_class_adjusted_results_imp,
  "Pooled_fit_statistics" = fit_medians,       # updated from pooled_fit_stats
  "PrEP_by_class" = prep_summary
), "data/poisson_class_results_imputed.xlsx")

# ==============================================================================
# Risk perception and PrEP initiation
# ==============================================================================

# reference level
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <-
  factor(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat)

m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <-
  relevel(
    m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
    ref = "Unlikely/Very Unlikely"
  )

# unadjusted Poisson model
mod_prep_3cat <- glm(
  prep_init_num ~ hiv_risk_perception_3cat + sdem_reside + rand_arm,
  data   = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

vc_3cat <- sandwich::vcovHC(mod_prep_3cat, type = "HC0")
ct_3cat <- lmtest::coeftest(mod_prep_3cat, vcov. = vc_3cat)
se_3cat <- sqrt(diag(vc_3cat))

res_prep_3cat <- data.frame(
  term      = rownames(ct_3cat),
  estimate  = exp(ct_3cat[, "Estimate"]),
  conf.low  = exp(ct_3cat[, "Estimate"] - 1.96 * se_3cat),
  conf.high = exp(ct_3cat[, "Estimate"] + 1.96 * se_3cat),
  p.value   = ct_3cat[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# adjusted Poisson model
mod_prep_3cat_adj <- glm(
  prep_init_num ~ hiv_risk_perception_3cat + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current +
    incarc_6m_bin + sdem_slep6m_binary,
  data   = m2hepprep_prep_combined_lca,
  family = poisson(link = "log")
)

vc_3cat_adj <- sandwich::vcovHC(mod_prep_3cat_adj, type = "HC0")
ct_3cat_adj <- lmtest::coeftest(mod_prep_3cat_adj, vcov. = vc_3cat_adj)
se_3cat_adj <- sqrt(diag(vc_3cat_adj))

res_prep_3cat_adj <- data.frame(
  term      = rownames(ct_3cat_adj),
  estimate  = exp(ct_3cat_adj[, "Estimate"]),
  conf.low  = exp(ct_3cat_adj[, "Estimate"] - 1.96 * se_3cat_adj),
  conf.high = exp(ct_3cat_adj[, "Estimate"] + 1.96 * se_3cat_adj),
  p.value   = ct_3cat_adj[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# class membership models

# reference class
m2hepprep_prep_combined_lca$class_factor_imputed <-
  droplevels(
    relevel(
      factor(
        m2hepprep_prep_combined_lca$class_factor_imputed,
        ordered = FALSE
      ),
      ref = "Low Overall Risk"
    )
  )

# unadjusted hiv risk perception model

mod_cls_3cat <- nnet::multinom(
  class_factor_imputed ~ hiv_risk_perception_3cat + sdem_reside + rand_arm,
  data  = m2hepprep_prep_combined_lca,
  trace = FALSE
)

sm3 <- summary(mod_cls_3cat)

res_cls_3cat <- do.call(
  rbind,
  lapply(rownames(sm3$coefficients), function(cls) {
    est <- sm3$coefficients[cls, ]
    se  <- sm3$standard.errors[cls, ]
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

# adjusted hiv risk perception model

mod_cls_3cat_adj <- nnet::multinom(
  class_factor_imputed ~ hiv_risk_perception_3cat + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current +
    incarc_6m_bin + sdem_slep6m_binary,
  data  = m2hepprep_prep_combined_lca,
  trace = FALSE
)

sm3_adj <- summary(mod_cls_3cat_adj)

res_cls_3cat_adj <- do.call(
  rbind,
  lapply(rownames(sm3_adj$coefficients), function(cls) {
    est <- sm3_adj$coefficients[cls, ]
    se  <- sm3_adj$standard.errors[cls, ]
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

# PrEP initiation summary by 3-category risk perception

prep_by_rp_3cat <- table(
  m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
  m2hepprep_prep_combined_lca$prep_init
)

if (!all(c("No", "Yes") %in% colnames(prep_by_rp_3cat))) {
  missing_cols <- setdiff(c("No", "Yes"), colnames(prep_by_rp_3cat))
  for (col in missing_cols) prep_by_rp_3cat <- cbind(prep_by_rp_3cat, 0)
  colnames(prep_by_rp_3cat) <- c("No", "Yes")
}

prep_by_rp_3cat_prop <- prop.table(prep_by_rp_3cat, 1)

prep_summary_rp_3cat <- data.frame(
  risk_perception = rownames(prep_by_rp_3cat),
  n_no    = prep_by_rp_3cat[, "No"],
  n_yes   = prep_by_rp_3cat[, "Yes"],
  prop_no = round(prep_by_rp_3cat_prop[, "No"], 3),
  prop_yes= round(prep_by_rp_3cat_prop[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# risk perception distribution within class

rp_by_class_3cat <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$hiv_risk_perception_3cat
)

rp_by_class_3cat_prop <- prop.table(rp_by_class_3cat, 1)

rp_class_summary_3cat <- do.call(
  rbind,
  lapply(rownames(rp_by_class_3cat), function(cls) {
    data.frame(
      class = cls,
      risk_perception = colnames(rp_by_class_3cat),
      n = as.numeric(rp_by_class_3cat[cls, ]),
      prop = round(as.numeric(rp_by_class_3cat_prop[cls, ]), 3),
      stringsAsFactors = FALSE
    )
  })
)

# save results
writexl::write_xlsx(
  list(
    PrepInit_RP_3cat_Unadj      = res_prep_3cat,
    PrepInit_RP_3cat_Adj        = res_prep_3cat_adj,
    ClassMem_RP_3cat_Unadj     = res_cls_3cat,
    ClassMem_RP_3cat_Adj       = res_cls_3cat_adj,
    PrepInit_Summary_RP_3cat   = prep_summary_rp_3cat,
    RP_Distribution_By_Class  = rp_class_summary_3cat
  ),
  "data/risk_perception_models_3cat.xlsx"
)

