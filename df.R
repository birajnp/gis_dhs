

## load household data file HR
dhs22_household <- read_dta(here("raw_dataset","NPHR82DT","NPHR82FL.DTA"))

dhs22_household <- forcats::as_factor(dhs22_household, only_labelled = TRUE)

write.csv(t(data.frame(var_label(dhs22_household))), "household_data_labels.csv", na="")


# Check the length of hhid in both datasets
summary(nchar(dhs22_personal$hhid))
summary(nchar(dhs22_household$hhid))

# Example: Right-align hhid if necessary to ensure length is consistent
dhs22_personal$hhid <- str_pad(dhs22_personal$hhid, width = 12, side = "left", pad = " ")
dhs22_household$hhid <- str_pad(dhs22_household$hhid, width = 12, side = "left", pad = " ")



# merging dataset

# Perform the merge (m:1 relationship)
merged_data <- dhs22_personal %>%
  left_join(dhs22_household, by = c("hhid","hv001", "hv002", "hv003"))
# Drop the 'hhid' column from merged_data
merged_data <- merged_data %>%
  select(-hhid)
merged_data <- forcats::as_factor(merged_data, only_labelled = TRUE)

write.csv(t(data.frame(var_label(merged_data))), "merged_data_labels.csv", na="")

