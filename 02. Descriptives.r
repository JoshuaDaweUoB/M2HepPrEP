# stop if script has an error
options(error = stop)

# libraries
pacman::p_load(dplyr, writexl, readxl, tidyr)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load clean data
m2hepprep_prep_combined <- read.csv("data/m2hepprep_combined.csv")

# simple tabs
table(m2hepprep_prep_combined$syringe_share_6m_bin, m2hepprep_prep_combined$sdem_reside)

# Create City variable
table(m2hepprep_prep_combined$sdem_reside)
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    City = case_when(
      sdem_reside == "Greater Montreal area" ~ "Montreal",
      sdem_reside == "Greater Miami area" ~ "Miami",
      TRUE ~ NA_character_
    )
  )

# Recode variables
m2hepprep_prep_combined <- m2hepprep_prep_combined %>%
  mutate(
    age_cat = recode(sdem_age_binary, "0" = "<40", "1" = "40+"),
    sex_cat = recode(sdem_sex_binary, "0" = "Male", "1" = "Female"),
    housing_cat = recode(sdem_slep6m_binary, "0" = "No", "1" = "Yes"),
    incarc_cat = recode(incarc_6m_bin, "0" = "No", "1" = "Yes"),
    oat_cat = recode(oat_current, "0" = "No", "1" = "Yes"),
    heroin = recode(inject_heroin_6m, "0" = "No", "1" = "Yes"),
    fentanyl = recode(inject_fent_6m, "0" = "No", "1" = "Yes"),
    cocaine = recode(inject_cocaine_6m, "0" = "No", "1" = "Yes"),
    amphetamines = recode(inject_meth_6m, "0" = "No", "1" = "Yes"),
    share_material = recode(syringe_share_6m_bin, "0" = "No", "1" = "Yes"),
    reuse_material = recode(syringe_reuse_6m_bin, "0" = "No", "1" = "Yes"),
    share_equipment = recode(syringe_cooker_6m_bin, "0" = "No", "1" = "Yes"),
    inject_days = recode(days_used_1m_3cat, "0" = "0–14", "1" = "15–29", "2" = "Daily"),
    partners = recode(num_sex_partners_3m, "0" = "None", "1" = "One", "2" = "Two or more"),
    condom_cat = case_when(
      condom_1m == "Very often / Always" ~ "Consistent",
      condom_1m == "Never / Rarely / Some of the time" ~ "Inconsistent",
      condom_1m == "No sex past month" ~ "No sex past month",
      condom_1m == "No sex past 3 months" ~ "No sex past three months"
    ),
    condom_intent = recode(condom_intent_6m, "0" = "Disagree/strongly disagree", "1" = "Neither agree nor disagree", "2" = "Agree/strongly agree"),
    sexwork = recode(sexwork_3m, "0" = "No", "1" = "Yes"),
    sexual_abuse = recode(sexual_abuse_ever, "0" = "No", "1" = "Yes")
  )

# Function to build n (%)
tab_var <- function(data, var, label) {
  overall <- data %>%
    filter(!is.na(.data[[var]])) %>%
    count(Level = .data[[var]]) %>%
    mutate(pct = 100 * n / sum(n), Group = "Overall")

  by_city <- data %>%
    filter(!is.na(.data[[var]]), !is.na(City)) %>%
    count(City, Level = .data[[var]]) %>%
    group_by(City) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup() %>%
    rename(Group = City)

  bind_rows(overall, by_city) %>%
    mutate(value = sprintf("%d (%.1f)", n, pct), Variable = label) %>%
    select(Variable, Level, Group, value) %>%
    pivot_wider(names_from = Group, values_from = value)
}

# Variables in exact display order
vars <- list(
  "Sociodemographic characteristics and structural factors" = NULL,
  age_cat = "Age (years)",
  sex_cat = "Sex",
  housing_cat = "Unstable housing",
  incarc_cat = "Incarceration",
  oat_cat = "Current OAT",
  "Injecting-related HIV risks" = NULL,
  heroin = "Injected heroin",
  fentanyl = "Injected fentanyl",
  cocaine = "Injected cocainea",
  amphetamines = "Injected amphetaminesa",
  share_material = "Used others injection material/worksa",
  reuse_material = "Reused own injection material/worksa",
  share_equipment = "Shared drug preparation equipmenta",
  inject_days = "Days injected past month",
  "Sexual-related HIV risks" = NULL,
  partners = "Number sex partners",
  condom_cat = "Condom use past month",
  condom_intent = "Intends to use condoms",
  sexwork = "Sex work (buying and selling)",
  sexual_abuse = "Victim of sexual abuse"
)

# Build table
table_list <- list()
for (v in names(vars)) {
  if (is.null(vars[[v]])) {
    # Section header row
    table_list[[length(table_list)+1]] <- tibble(Variable = v, Level = "", Overall = "", Miami = "", Montreal = "")
  } else {
    table_list[[length(table_list)+1]] <- tab_var(m2hepprep_prep_combined, v, vars[[v]])
    # Add empty row after each variable
    table_list[[length(table_list)+1]] <- tibble(Variable = "", Level = "", Overall = "", Miami = "", Montreal = "")
  }
}
table1 <- bind_rows(table_list)

# Add total N row at top
n_row <- tibble(
  Variable = "Overall",
  Level = "",
  Overall = as.character(nrow(m2hepprep_prep_combined)),
  Miami = as.character(sum(m2hepprep_prep_combined$City == "Miami", na.rm = TRUE)),
  Montreal = as.character(sum(m2hepprep_prep_combined$City == "Montreal", na.rm = TRUE))
)
table1 <- bind_rows(n_row, table1)

# Save
write_xlsx(table1, "data/table1_full.xlsx")