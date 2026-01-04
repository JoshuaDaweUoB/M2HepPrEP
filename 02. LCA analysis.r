# ============================================================
# Load libraries and data
# ============================================================

# stop if script has an error
options(error = stop)

# libraries
pacman::p_load(dplyr, mice, writexl, readxl, poLCA, ggplot2, clue, sandwich, lmtest, MASS)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

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
# Prepare datasets for poLCA
# ============================================================
prepare_for_poLCA <- function(df, vars) {
  df[vars] <- lapply(df[vars], function(x) {
    x <- factor(x)
    as.integer(x)
  })
  
  df
}

imputed_datasets_lca <- lapply(seq_len(imp$m), function(i) {
  df <- imputed_datasets_mice[[i]]
  prepare_for_poLCA(df, lca_vars)
})

print(lapply(imputed_datasets_lca[[1]][lca_vars], unique))

# ============================================================
# QA checks
# ============================================================

# first dataset
cat("\nLevels (binary vars):\n")
print(sapply(imputed_datasets_mice[[1]][lca_vars_bin], levels))

cat("\nLevels (categorical vars):\n")
print(sapply(imputed_datasets_mice[[1]][lca_vars_cat], levels))

# poLCA-ready encoding
cat("\nUnique values after conversion (should be integers >=1):\n")
print(lapply(imputed_datasets_lca[[1]][lca_vars], unique))

# ============================================================
# Save descriptive stats of LCA variables
# ============================================================

desc_mi_all <- bind_rows(lapply(seq_len(imp$m), function(i) {
  df <- imputed_datasets_mice[[i]]
  bind_rows(lapply(lca_vars, function(v) {
    tb <- as.data.frame(table(df[[v]], useNA = "ifany"), stringsAsFactors = FALSE)
    names(tb) <- c("Level", "n")
    tb$Variable <- v
    tb <- tb %>%
      dplyr::mutate(
        pct = round(100 * n / sum(n), 1),
        Imputation = i
      ) %>%
      dplyr::select(Variable, Level, n, pct, Imputation)
    tb
  }))
}))

# pooled mean % with SD/min/max
desc_mi_summary <- desc_mi_all %>%
  group_by(Variable, Level) %>%
  summarise(
    pct_mean = round(mean(pct, na.rm = TRUE), 1),
    pct_sd   = round(sd(pct, na.rm = TRUE), 1),
    pct_min  = min(pct, na.rm = TRUE),
    pct_max  = max(pct, na.rm = TRUE),
    .groups = "drop"
  )

# save
writexl::write_xlsx(
  list(
    Descriptives_MI_by_Imputation = desc_mi_all,
    Descriptives_MI  = desc_mi_summary
  ),
  "data/lca_descriptives.xlsx"
)

# ==========================================================================
# Multiple imputation latent class analysis
# ==========================================================================

# ======================================================
# Number of imputations
# ======================================================
n_imp <- length(imputed_datasets_lca)

# ======================================================
# LCA formula
# ======================================================
lca_formula <- as.formula(
  paste0("cbind(", paste(lca_vars, collapse = ","), ") ~ 1")
)

# ======================================================
# Calculate fit statistics
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
# Run LCA on all imputed datasets
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
# Combine fit statistics across imputations
# ======================================================

# combine fit statistics across imputations
all_fit_stats <- do.call(rbind, lapply(seq_len(n_imp), function(i) {
  cbind(Imputation = i, fit_stats_all[[i]])
}))

average_fit_stats <- all_fit_stats %>%
  group_by(NClasses) %>%
  summarise(
    across(
      c(AIC, BIC, SABIC, Entropy, LL, ChiSquare, df,
        NClass1, NClass2, NClass3, NClass4, NClass5),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Write to Excel
write_xlsx(
  list(
    LCA_Fit_Imputations = all_fit_stats,
    LCA_Fit_Averages        = average_fit_stats
  ),
  "data/lca_fit_stats_imputed.xlsx"
)

# ======================================================
# Summarise fit statistics
# ======================================================
fit_summary <- all_fit_stats %>%
  group_by(NClasses) %>%
  summarise(
    AIC_med   = median(AIC, na.rm = TRUE),
    BIC_med   = median(BIC, na.rm = TRUE),
    SABIC_med = median(SABIC, na.rm = TRUE)
  )

# ======================================================
# Elbow plot
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
# Model fit statistics across imputations
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

# final number of classes
k_final <- 4

# ==============================================================================
# Fit final LCA model across imputations
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
# Class labels across imputations
# ==============================================================================

# rows with any NA in LCA variables, across all imputations
lapply(seq_along(imputed_datasets_mice), function(i) {
  df <- imputed_datasets_mice[[i]]
  na_rows <- which(rowSums(is.na(df[lca_vars])) > 0)
  if(length(na_rows) > 0) {
    na_vars <- apply(df[na_rows, lca_vars], 1, function(x) names(which(is.na(x))))
    list(imputation = i, rows = na_rows, vars = na_vars)
  } else {
    NULL
  }
})

class_assignments <- matrix(NA, nrow = n_participants, ncol = n_imp)

# first imputation
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
# Pool class membership 
# ==============================================================================

final_class_assignment <- apply(class_assignments, 1, function(x) {
  as.numeric(names(which.max(table(x))))
})

# agreement rate
agreement_rate <- mean(apply(class_assignments, 1, function(x)
  length(unique(x)) == 1
))

cat("\nClass assignment stability across imputations:\n")
cat("Perfect agreement rate:", round(agreement_rate * 100, 1), "%\n\n")

# class distribution
print(table(final_class_assignment))

# ==============================================================================
# First dataset with final class
# ==============================================================================

df_ref <- imputed_datasets_lca[[1]]
df_ref$Class <- final_class_assignment

# list of LCA variables
lca_vars_selected <- c(
  "syringe_share_6m_bin",
  "syringe_cooker_6m_bin",
  "syringe_loan_6m_bin",
  "syringe_reuse_6m_bin",
  "sexwork_3m",
  "num_sex_partners_3m",
  "condom_1m"
)

# LCA vars as factors
df_ref <- df_ref %>%
  mutate(across(all_of(lca_vars_selected), as.factor))

# variable labels using recode with default
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

# counts and percentages per class
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

# reshape so variables are rows
wide_table <- freq_by_class %>%
  tidyr::pivot_wider(
    id_cols = c(Variable, Level_Label),
    names_from = Class,
    values_from = c(Count, Percent),
    names_sep = "_Class"
  ) %>%
  dplyr::arrange(Variable, Level_Label)

# save
writexl::write_xlsx(wide_table, "data/class_patterns_categorical_wide.xlsx")

# ============================================================
# Auxiliary variables: Overall + By City (Montreal / Miami)
# ============================================================
table(df_ref$sdem_reside, useNA = "ifany")

# reference dataset
df_ref <- imputed_datasets_mice[[1]]
df_ref$Class <- final_class_assignment

df_ref <- df_ref %>%
  mutate(
    City = dplyr::case_when(
      sdem_reside == "Greater Montreal area" ~ "Montreal",
      sdem_reside == "Greater Miami area"    ~ "Miami",
      TRUE ~ NA_character_
    )
  )

# auxiliary variables
aux_vars_selected <- setdiff(auxiliary_vars, "sdem_reside")

# make sure auxiliary vars are factors
df_ref <- df_ref %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(aux_vars_selected), as.factor))

# ============================================================
# Overall counts / percents (drop NA levels)
# ============================================================

overall_aux <- dplyr::bind_rows(lapply(aux_vars_selected, function(var) {
  df_ref %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::group_by(
      Variable    = var,
      Level_Label = .data[[var]]
    ) %>%
    dplyr::summarise(
      Count_Overall = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(
      Percent_Overall = round(100 * Count_Overall / sum(Count_Overall), 1)
    ) %>%
    dplyr::ungroup()
}))

# ============================================================
# By-city counts / percents (Montreal & Miami)
# ============================================================

by_city_aux_long <- dplyr::bind_rows(lapply(aux_vars_selected, function(var) {
  df_ref %>%
    dplyr::filter(
      !is.na(.data[[var]]),
      !is.na(City),
      City %in% c("Montreal", "Miami")
    ) %>%
    dplyr::group_by(
      Variable    = var,
      Level_Label = .data[[var]],
      City
    ) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Variable, City) %>%
    dplyr::mutate(
      Percent = round(100 * Count / sum(Count), 1)
    ) %>%
    dplyr::ungroup()
}))

# ============================================================
# Pivot cities wide
# ============================================================

wide_aux_city <- tidyr::pivot_wider(
  by_city_aux_long,
  id_cols     = c(Variable, Level_Label),
  names_from  = City,
  values_from = c(Count, Percent),
  names_sep   = "_"
)

# ============================================================
# Combine Overall + City tables
# ============================================================

levels_overall_by_city_aux <- overall_aux %>%
  dplyr::left_join(
    wide_aux_city,
    by = c("Variable", "Level_Label")
  ) %>%
  dplyr::arrange(Variable, Level_Label)

# make sure city columns exist
for (nm in c(
  "Count_Montreal", "Percent_Montreal",
  "Count_Miami",    "Percent_Miami"
)) {
  if (!nm %in% names(levels_overall_by_city_aux)) {
    levels_overall_by_city_aux[[nm]] <-
      if (grepl("^Count", nm)) NA_integer_ else NA_real_
  }
}

# ============================================================
# Add formatted columns: "n (x.x)"
# ============================================================

levels_overall_by_city_aux <- levels_overall_by_city_aux %>%
  dplyr::mutate(
    Overall_fmt = sprintf(
      "%d (%.1f)",
      Count_Overall,
      Percent_Overall
    ),
    Montreal_fmt = ifelse(
      is.na(Count_Montreal),
      NA,
      sprintf("%d (%.1f)", Count_Montreal, Percent_Montreal)
    ),
    Miami_fmt = ifelse(
      is.na(Count_Miami),
      NA,
      sprintf("%d (%.1f)", Count_Miami, Percent_Miami)
    )
  ) %>%
  dplyr::select(
    Variable,
    Level_Label,
    Overall_fmt,
    Montreal_fmt,
    Miami_fmt,
    dplyr::everything()
  )

# save
writexl::write_xlsx(
  list(
    Auxiliary_Overall_ByCity = levels_overall_by_city_aux
  ),
  "data/auxiliary_overall_by_city.xlsx"
)

# assign classes
m2hepprep_prep_combined_lca <- m2hepprep_prep_combined
m2hepprep_prep_combined_lca$class_imputed <- final_class_assignment

m2hepprep_prep_combined_lca$class_factor_imputed <- factor(
  final_class_assignment,
  levels = 1:k_final,
  labels = c(
    "High Injecting / Low Sexual Risk",           # Class 1
    "High Injecting / High Sexual Risk",          # Class 2
    "Low Overall Risk",                           # Class 3
    "Low Injecting / High Sexual Risk"            # Class 4
  )
)

# levels for plotting
target_levels <- tibble::tribble(
  ~Variable,                 ~Level_Label,                                          ~Domain,           ~Indicator,
  "syringe_share_6m_bin",    "Yes",                                                "Injecting",       "Shared syringe (6m)",
  "syringe_cooker_6m_bin",   "Yes",                                                "Injecting",       "Shared cooker (6m)",
  "syringe_loan_6m_bin",     "Yes",                                                "Injecting",       "Loaned syringe (6m)",
  "syringe_reuse_6m_bin",    "Yes",                                                "Injecting",       "Reused syringe (6m)",
  "sexwork_3m",              "Yes",                                                "Sexual Risk",     "Sex work (3m)",
  "num_sex_partners_3m",     "Two or more",                                        "Sexual Risk",     "≥2 partners (3m)",
  "condom_1m",               "Never / Rarely / Some of the time",                  "Sexual Risk",     "Inconsistent condom use (1m)",
) %>%
  mutate(Order = row_number())

# per-class probability for chosen level of rows
class_labels <- levels(m2hepprep_prep_combined_lca$class_factor_imputed)
classes_idx  <- seq_along(class_labels)

# full grid for all classes × selected indicators
grid <- target_levels %>%
  dplyr::select(Variable, Level_Label, Domain, Indicator, Order) %>%
  tidyr::crossing(Class = classes_idx)

# frequencies and fill missing with zeros
plot_df <- grid %>%
  dplyr::left_join(
    freq_by_class %>%
      dplyr::select(Variable, Level_Label, Class, Percent),
    by = c("Variable", "Level_Label", "Class")
  ) %>%
  dplyr::mutate(
    Percent    = tidyr::replace_na(Percent, 0),
    prob       = Percent / 100,
    ClassLabel = factor(Class, levels = classes_idx, labels = class_labels),
    x          = Order
  ) %>%
  dplyr::arrange(x, ClassLabel)

# separators and headers
domain_spans <- target_levels %>%
  group_by(Domain) %>%
  summarise(start = min(Order), end = max(Order), mid = (start + end) / 2, .groups = "drop")

domain_boundaries <- domain_spans$end[-nrow(domain_spans)] + 0.5

# auto-trim to your k_final
n_classes <- length(class_labels)
color_vals    <- c("#000000", "#2E8B57", "#7F7F7F", "#B0B0B0", "#1F77B4")[seq_len(n_classes)]
linetype_vals <- c("solid",  "solid",   "dashed",  "dotted",  "dotdash")[seq_len(n_classes)]

# ggplot baby
p <- ggplot(plot_df, aes(x = x, y = prob, group = ClassLabel, color = ClassLabel, linetype = ClassLabel)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(
    breaks = target_levels$Order,
    labels = target_levels$Indicator,
    expand = c(0.02, 0.02)
  ) +
  scale_color_manual(values = color_vals) +
  scale_linetype_manual(values = linetype_vals) +
  labs(
    title = "Probability of Indicators for Each Class",
    x = NULL, y = NULL, color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 1.05))

p <- p + theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))
p <- p + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
p <- p + theme(
  text         = element_text(colour = "black"),
  plot.title   = element_text(face = "bold", colour = "black"),
  axis.text.x  = element_text(size = 10, angle = 65, hjust = 1, vjust = 1, colour = "black"),
  axis.text.y  = element_text(colour = "black"),
  legend.title = element_text(colour = "black"),
  legend.text  = element_text(colour = "black")
)
p <- p + ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.6)

# save
ggplot2::ggsave(
  filename = "figures/lca_indicator_probabilities_wide.png",
  plot = p,
  width = 14, height = 6, units = "in", dpi = 300,
  bg = "white"
)

# ---------------------------
# Inspect class distribution
# ---------------------------
cat("\nClass distribution (should match pooled assignments):\n")
print(table(m2hepprep_prep_combined_lca$class_factor_imputed))

# save data
write.csv(m2hepprep_prep_combined_lca, "data/m2hepprep_combined_lca.csv", row.names = FALSE)
