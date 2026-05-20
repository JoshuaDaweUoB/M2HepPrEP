# load libraries
pacman::p_load(dplyr, tidyr, writexl, poLCA, ggplot2, clue, MASS)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_combined.csv")

# stop when error
options(error = stop)

# LCA variables
lca_vars <- c(
  "syringe_share_6m_bin", "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin", "syringe_reuse_6m_bin",
  "num_sex_partners_3m", "condom_1m",
  "sexwork_3m"
)

# complete case coding

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

# Run LCA
lca_formula <- as.formula(
  paste("cbind(", paste(lca_vars, collapse = ", "), ") ~ 1")
)

fit_stats <- list()

n <- nrow(m2hepprep_prep_combined_cc)

set.seed(123)

for (k in 1:5) {

  cat("Running LCA with", k, "classes...\n")

  lca_model <- poLCA(
    lca_formula,
    data    = m2hepprep_prep_combined_cc,
    nclass  = k,
    maxiter = 1000,
    nrep    = 20,
    verbose = FALSE
  )

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

# elbow plot
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
    title = "LCA Model Selection with Complete Case",
    color = "Criterion"
  ) +
  theme_minimal()

print(elbow_plot)

# save plot
ggsave(
  filename = "data/elbow_plot_lca_cc.png",
  plot = elbow_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# final LCA with four classes

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

posterior <- lca_cc_final$posterior
predclass <- lca_cc_final$predclass

# posterior probability of assigned class
posterior_assigned <- sapply(seq_along(predclass), function(j) {
  posterior[j, predclass[j]]
})

# class proportions
class_props <- prop.table(table(predclass))

# BCH weights
bch_weights_cc <- posterior_assigned / as.numeric(class_props[as.character(predclass)])

m2hepprep_prep_combined_cc$bch_weight <- bch_weights_cc

lca_cc_final

# dataset with final class assignments from lca_cc_final

# complete-case dataset
m2hepprep_prep_combined_cc_lca <- m2hepprep_prep_combined_cc

# add final class assignments from lca_cc_final
m2hepprep_prep_combined_cc_lca$Class <- factor(
  lca_cc_final$predclass,
  labels = c(
    "Class 1", "Class 2", "Class 3", "Class 4", "Class 5"
  )[1:length(unique(lca_cc_final$predclass))]
)

# LCA variables
lca_vars_selected <- c(
  "syringe_share_6m_bin",
  "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin",
  "syringe_reuse_6m_bin",
  "sexwork_3m",
  "num_sex_partners_3m",
  "condom_1m"
)

# LCA variables to factors
m2hepprep_prep_combined_cc_lca <- m2hepprep_prep_combined_cc_lca %>%
  mutate(across(all_of(lca_vars_selected), as.factor))

# variable labels
m2hepprep_prep_combined_cc_lca <- m2hepprep_prep_combined_cc_lca %>%
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

# counts and percentages
freq_by_class <- lca_vars_selected %>%
  lapply(function(var) {
    m2hepprep_prep_combined_cc_lca %>%
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

# variables are rows
wide_table <- freq_by_class %>%
  tidyr::pivot_wider(
    id_cols = c(Variable, Level_Label),
    names_from = Class,
    values_from = c(Count, Percent),
    names_sep = "_"
  ) %>%
  dplyr::arrange(Variable, Level_Label)

# save
write_xlsx(wide_table, "data/class_patterns_categorical_wide_finalCC.xlsx")

# numeric class assignment
m2hepprep_prep_combined_cc_lca$class_imputed <- lca_cc_final$predclass

# factor with meaningful labels
m2hepprep_prep_combined_cc_lca$class_factor_imputed <- factor(
  lca_cc_final$predclass,
  levels = 1:k_final,  # ensure matches the number of classes in final model
  labels = c(
    "Low Injecting / High Sexual Risk (n = 53)",        # Class 1
    "Low Injecting / Low Sexual Risk (n = 167)",        # Class 2
    "High Injecting / High Sexual Risk (n = 53)",       # Class 3
    "High Injecting / Low Sexual Risk (n = 118)"        # Class 4
  )
)

# Inspect class distribution
cat("\nClass distribution (final complete-case LCA):\n")
print(table(m2hepprep_prep_combined_cc_lca$class_factor_imputed))

# save data
write.csv(
  m2hepprep_prep_combined_cc_lca,
  "data/m2hepprep_combined_lca_finalCC.csv",
  row.names = FALSE
)

# Poisson regression with complete-case LCA

# numeric and factor class variables from final LCA
m2hepprep_prep_combined_cc_lca$class_imputed <- lca_cc_final$predclass

# reference class
m2hepprep_prep_combined_cc_lca$class_factor_imputed <- relevel(
  m2hepprep_prep_combined_cc_lca$class_factor_imputed,
  ref = "Low Injecting / Low Sexual Risk (n = 167)"
)

# partially adjusted Poisson regression
mod_class_finalCC <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm,
  data = m2hepprep_prep_combined_cc_lca,
  family = poisson(link = "log"),
  weights = bch_weight
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

# fully adjusted Poisson regression
mod_class_adj_finalCC <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age + oat_current +
    incarc_6m_bin + sdem_slep6m_binary,
  data = m2hepprep_prep_combined_cc_lca,
  family = poisson(link = "log"),
  weights = bch_weight
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

# PrEP initiation summary by class
prep_by_class_finalCC <- table(
  m2hepprep_prep_combined_cc_lca$class_factor_imputed,
  m2hepprep_prep_combined_cc_lca$prep_init
)

prep_by_class_prop_finalCC <- prop.table(prep_by_class_finalCC, 1)

prep_summary_finalCC <- data.frame(
  class = rownames(prep_by_class_finalCC),
  n_no = prep_by_class_finalCC[, "No"],
  n_yes = prep_by_class_finalCC[, "Yes"],
  prop_no = round(prep_by_class_prop_finalCC[, "No"], 3),
  prop_yes = round(prep_by_class_prop_finalCC[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# Save results to Excel
write_xlsx(
  list(
    "Poisson_class_unadjusted_finalCC" = poisson_class_results_finalCC,
    "Poisson_class_adjusted_finalCC"   = poisson_class_adjusted_results_finalCC,
    "PrEP_by_class"                    = prep_summary_finalCC
  ),
  "data/poisson_class_results_finalCC.xlsx"
)

# risk perception and PrEP initiation 

# complete-case dataset with final LCA assignments
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined_cc

# numeric and factor class variables from lca_cc_final
m2hepprep_prep_combined_lca$class_imputed <- lca_cc_final$predclass
m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  lca_cc_final$predclass,
  levels = 1:k_final,
  labels = c(
    "Low Injecting / High Sexual Risk (n = 53)",        # Class 1
    "Low Injecting / Low Sexual Risk (n = 167)",        # Class 2
    "High Injecting / High Sexual Risk (n = 53)",       # Class 3
    "High Injecting / Low Sexual Risk (n = 118)"        # Class 4
  )
)

# Poisson regression

# reference level
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <- 
  factor(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat)
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <- 
  relevel(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
          ref = "Unlikely/Very Unlikely")

# partially adjusted model
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

# fully adjusted model
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

# PrEP initiation summary by HIV risk perception
prep_by_rp_3cat_finalCC <- table(
  m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
  m2hepprep_prep_combined_lca$prep_init
)

prep_by_rp_3cat_prop_finalCC <- prop.table(prep_by_rp_3cat_finalCC, 1)

prep_summary_rp_3cat_finalCC <- data.frame(
  risk_perception = rownames(prep_by_rp_3cat_finalCC),
  n_no    = prep_by_rp_3cat_finalCC[, "No"],
  n_yes   = prep_by_rp_3cat_finalCC[, "Yes"],
  prop_no = round(prep_by_rp_3cat_prop_finalCC[, "No"], 3),
  prop_yes= round(prep_by_rp_3cat_prop_finalCC[, "Yes"], 3),
  stringsAsFactors = FALSE
)

# save results
write_xlsx(
  list(
    PrepInit_RP_3cat_Unadj      = res_prep_3cat_finalCC,
    PrepInit_RP_3cat_Adj        = res_prep_3cat_adj_finalCC,
    PrepInit_Summary_RP_3cat    = prep_summary_rp_3cat_finalCC
  ),
  "data/risk_perception_models_3cat_finalCC.xlsx"
)
