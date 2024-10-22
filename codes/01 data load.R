rm(list=ls())

#-----------------1. Install, load libraries and dataset-------------------
#Step-1: Install and load library

Packages <- c("sf","here","gtsummary","foreign","survey",'labelled',"readxl", "tidyverse", "haven","rockchalk", "forcats")


new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
lapply(Packages, require, character.only=T)


# #load personal data set PR file
dhs22_personal <- read_dta(here("raw_dataset","household_member","NPPR82FL.DTA"))

#calculating sample weight
dhs22_personal <- dhs22_personal %>%
  mutate(sample_wt = hv005/1000000)



#writing sample label

dhs22_personal <- forcats::as_factor(dhs22_personal, only_labelled = TRUE)
write.csv(t(data.frame(var_label(dhs22_personal))), "personal_data_labels.csv", na="")

