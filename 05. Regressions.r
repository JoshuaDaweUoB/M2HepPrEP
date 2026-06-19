options(error = stop)

pacman::p_load(dplyr, tidyr, mice, writexl, readxl, 
               ggplot2, clue, sandwich, lmtest, MASS, nnet)

setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

m2hepprep_prep_combined_lca <- read.csv("data/m2hepprep_combined_lca.csv")
View(m2hepprep_prep_combined_lca)
table(m2hepprep_prep_combined_lca$class_factor_imputed)

# load BCH weights
bch_weights <- readRDS("data/bch_weights.rds")
m2hepprep_prep_combined_lca$bch_weight <- bch_weights

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  ordered = FALSE
)

m2hepprep_prep_combined_lca$class_factor_imputed <- relevel(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  ref = "Low Injecting / Low Sexual Risk (n=157)"
)

# classification certainty (diagnostic info only)
post <- lca_imputed_results[[1]]$posterior

# classification certainty per person (max posterior)
max_prob <- apply(post, 1, max)

# original data format with BCH weights
df <- m2hepprep_prep_combined_lca

# partially adjusted 

bch_model_simple <- glm(
  prep_init_num ~ class_factor_imputed + sdem_reside + rand_arm,
  data = df,
  family = poisson(link = "log"),
  weights = bch_weight
)

robust_se_simple <- sqrt(diag(vcovHC(bch_model_simple, type = "HC0")))
coefs_simple <- coeftest(bch_model_simple, vcov. = vcovHC(bch_model_simple, type = "HC0"))

bch_results_simple <- data.frame(
  term = rownames(coefs_simple),
  rr = exp(coefs_simple[, "Estimate"]),
  conf.low = exp(coefs_simple[, "Estimate"] - 1.96 * robust_se_simple),
  conf.high = exp(coefs_simple[, "Estimate"] + 1.96 * robust_se_simple),
  p.value = coefs_simple[, "Pr(>|z|)"],
  row.names = NULL
)

# fully adjusted

# descriptives

vars_to_summarise <- c(
  "sdem_reside",
  "rand_arm",
  "sdem_sex_binary",
  "sdem_age_binary",
  "oat_current",
  "incarc_6m_bin",
  "sdem_slep6m_binary"
)

prep_summaries_other <- do.call(
  rbind,
  lapply(vars_to_summarise, function(var) {

    tab <- table(
      m2hepprep_prep_combined_lca[[var]],
      m2hepprep_prep_combined_lca$prep_init
    )

    prop_tab <- prop.table(tab, 1)

    data.frame(
      variable = var,
      level = rownames(tab),
      n_no = tab[, "No"],
      n_yes = tab[, "Yes"],
      prop_no = round(prop_tab[, "No"], 3),
      prop_yes = round(prop_tab[, "Yes"], 3),
      row.names = NULL
    )
  })
)

# model
bch_model_adj <- glm(
  prep_init_num ~ class_factor_imputed +
    sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age_binary +
    oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data = df,
  family = poisson(link = "log"),
  weights = bch_weight
)

robust_se_adj <- sqrt(diag(vcovHC(bch_model_adj, type = "HC0")))
coefs_adj <- coeftest(bch_model_adj, vcov. = vcovHC(bch_model_adj, type = "HC0"))

bch_results_adj <- data.frame(
  term = rownames(coefs_adj),
  rr = exp(coefs_adj[, "Estimate"]),
  conf.low = exp(coefs_adj[, "Estimate"] - 1.96 * robust_se_adj),
  conf.high = exp(coefs_adj[, "Estimate"] + 1.96 * robust_se_adj),
  p.value = coefs_adj[, "Pr(>|z|)"],
  row.names = NULL
)

# descriptive table 

prep_by_class <- table(
  m2hepprep_prep_combined_lca$class_factor_imputed,
  m2hepprep_prep_combined_lca$prep_init
)

if (!all(c("No", "Yes") %in% colnames(prep_by_class))) {
  missing_cols <- setdiff(c("No", "Yes"), colnames(prep_by_class))
  for (col in missing_cols) prep_by_class <- cbind(prep_by_class, 0)
  colnames(prep_by_class) <- c("No", "Yes")
}

prep_by_class_prop <- prop.table(prep_by_class, 1)

prep_summary <- data.frame(
  class = rownames(prep_by_class),
  n_no = prep_by_class[, "No"],
  n_yes = prep_by_class[, "Yes"],
  prop_no = round(prep_by_class_prop[, "No"], 3),
  prop_yes = round(prep_by_class_prop[, "Yes"], 3)
)

write_xlsx(list(
  bch_simple = bch_results_simple,
  bch_adjusted = bch_results_adj,
  prep_by_class = prep_summary,
  prep_by_other_covariates = prep_summaries_other
), "data/poisson_class_results_bch.xlsx")

# interaction model: LCA class + risk perception

# set up risk perception factor
m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <-
  factor(m2hepprep_prep_combined_lca$hiv_risk_perception_3cat)

m2hepprep_prep_combined_lca$hiv_risk_perception_3cat <-
  relevel(
    m2hepprep_prep_combined_lca$hiv_risk_perception_3cat,
    ref = "Unlikely/Very Unlikely"
  )

# unadjusted interaction model
mod_interact_unadj <- glm(
  prep_init_num ~ class_factor_imputed * hiv_risk_perception_3cat + 
    sdem_reside + rand_arm,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log"),
  weights = bch_weight
)

vc_interact_unadj <- sandwich::vcovHC(mod_interact_unadj, type = "HC0")
ct_interact_unadj <- lmtest::coeftest(mod_interact_unadj, vcov. = vc_interact_unadj)
se_interact_unadj <- sqrt(diag(vc_interact_unadj))

res_interact_unadj <- data.frame(
  term = rownames(ct_interact_unadj),
  estimate = exp(ct_interact_unadj[, "Estimate"]),
  conf.low = exp(ct_interact_unadj[, "Estimate"] - 1.96 * se_interact_unadj),
  conf.high = exp(ct_interact_unadj[, "Estimate"] + 1.96 * se_interact_unadj),
  p.value = ct_interact_unadj[, "Pr(>|z|)"],
  stringsAsFactors = FALSE,
  row.names = NULL
)

# adjusted interaction model
mod_interact_adj <- glm(
  prep_init_num ~ class_factor_imputed * hiv_risk_perception_3cat + 
    sdem_reside + rand_arm +
    sdem_sex_binary + sdem_age_binary +
    oat_current + incarc_6m_bin + sdem_slep6m_binary,
  data = m2hepprep_prep_combined_lca,
  family = poisson(link = "log"),
  weights = bch_weight
)

vc_interact_adj <- sandwich::vcovHC(mod_interact_adj, type = "HC0")
ct_interact_adj <- lmtest::coeftest(mod_interact_adj, vcov. = vc_interact_adj)
se_interact_adj <- sqrt(diag(vc_interact_adj))

res_interact_adj <- data.frame(
  term = rownames(ct_interact_adj),
  estimate = exp(ct_interact_adj[, "Estimate"]),
  conf.low = exp(ct_interact_adj[, "Estimate"] - 1.96 * se_interact_adj),
  conf.high = exp(ct_interact_adj[, "Estimate"] + 1.96 * se_interact_adj),
  p.value = ct_interact_adj[, "Pr(>|z|)"],
  stringsAsFactors = FALSE,
  row.names = NULL
)

# save interaction results
write_xlsx(list(
  Class_RP_Interaction_Unadj = res_interact_unadj,
  Class_RP_Interaction_Adj = res_interact_adj
), "data/class_rp_interaction_results.xlsx")

# stratified table
stratified <- m2hepprep_prep_combined_lca %>%
  group_by(class_factor_imputed, hiv_risk_perception_3cat) %>%
  summarise(
    n = n(),
    n_prep_init = sum(prep_init == "Yes"),
    prep_rate = round(sum(prep_init == "Yes") / n(), 3),
    .groups = "drop"
  )
stratified

# risk perception and PrEP initiation

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
    sdem_sex_binary + sdem_age_binary + oat_current +
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
    sdem_sex_binary + sdem_age_binary + oat_current +
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

