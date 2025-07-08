# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Montreal paper/")

# load raw data
baseline <- read.csv("data/cleaned_m2_data_05012025 (1).csv")
View(baseline)
