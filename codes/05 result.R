library(here)
source(here("codes", "04 data_harmonization_merging.R"))

# # selected data
# selected_data <- combined_data_cleaned[, c("sample_wt", "bmi_category","hv025",
#                                            
#                                            "high_blood_pressure","shecoreg", "hv040",
#                                            "DISTRICT","DHSID","geometry")]


# converting the data type as numberic

df <- combined_data %>% 
  mutate (
    weight = as.numeric(sample_wt),
    psu = as.numeric(hv021)
  )


#Create the survey design object 
svy_dataset <- svydesign(
  data = df,                      # Use the entire dataset
  ids = ~ psu,                       # Primary Sampling Units variable                # Stratum variable
  weights = ~ sample_wt, nest = TRUE                # Weights variable
)


# Summary statistics
summary_stats <- svy_dataset %>%
  tbl_svysummary(
    include = c( 
                 hv104,  hv105, hv115,  #sex of respondent # age # current marital status
                 
                 sh17b1, # education
                 #hv106,
                 #hv107,  #Education level
                 #hv040, # cluster elevation
                 #hv024:hv025, # type of residency
                 shecoreg, # ecological belt
                 blood_pressure_cat,  bmi_category,
                 new_systolic, new_diastolic, hv222, hv223,
                 diagnosed_HTN, hv270, PR_NAME,
               
    ),
    statistic = list(all_categorical() ~ "{n} ({p})", all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ c(0,1), all_continuous() ~ c(1,1,1))
  ) %>%
  add_ci(
    method = all_continuous() ~ "svymean",
    style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 1),
                     all_continuous() ~ purrr::partial(style_number, digits = 1))
  )





print(summary_stats)


