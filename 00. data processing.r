# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load raw data
m2hepprep_raw <- read.csv("data/cleaned_m2_data_05012025 (1).csv")
View(m2hepprep_raw)

