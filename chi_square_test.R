# Load required libraries
library(survey)
library(stats)

# Function for formatted Chi-square test results
format_chi_square <- function(test_result) {
  return(paste0("χ2(", test_result$parameter, ") = ", 
                round(test_result$statistic, 2), 
                ", p ", ifelse(test_result$p.value < 0.001, "< 0.001", 
                               paste0("= ", round(test_result$p.value, 3)))))
}

# Function for formatted t-test results
format_t_test <- function(test_result) {
  return(paste0("t(", round(test_result$parameter, 1), ") = ", 
                round(test_result$statistic, 2), 
                ", p ", ifelse(test_result$p.value < 0.001, "< 0.001", 
                               paste0("= ", round(test_result$p.value, 3)))))
}

# Categorical Variables Tests (Chi-square)
# Perform Chi-square tests for Hypertension
htn_sex_test <- svychisq(~high_blood_pressure + hv104, design = svy_dataset)
htn_age_test <- svychisq(~high_blood_pressure + age_cat, design = svy_dataset)
htn_marital_test <- svychisq(~high_blood_pressure + marital_status, design = svy_dataset)
htn_education_test <- svychisq(~high_blood_pressure + edu_cat, design = svy_dataset)
htn_wealth_test <- svychisq(~high_blood_pressure + wealth_cat, design = svy_dataset)
htn_urban_test <- svychisq(~high_blood_pressure + hv025, design = svy_dataset)
htn_region_test <- svychisq(~high_blood_pressure + shecoreg, design = svy_dataset)
htn_bmi_test <- svychisq(~high_blood_pressure + bmi_category_nr_ob, design = svy_dataset)

# Perform Chi-square tests for BMI
bmi_sex_test <- svychisq(~bmi_category_nr_ob + hv104, design = svy_dataset)
bmi_age_test <- svychisq(~bmi_category_nr_ob + age_cat, design = svy_dataset)
bmi_marital_test <- svychisq(~bmi_category_nr_ob + marital_status, design = svy_dataset)
bmi_education_test <- svychisq(~bmi_category_nr_ob + edu_cat, design = svy_dataset)
bmi_wealth_test <- svychisq(~bmi_category_nr_ob + wealth_cat, design = svy_dataset)
bmi_urban_test <- svychisq(~bmi_category_nr_ob + hv025, design = svy_dataset)
bmi_region_test <- svychisq(~bmi_category_nr_ob + shecoreg, design = svy_dataset)
bmi_htn_test <- svychisq(~bmi_category_nr_ob + high_blood_pressure, design = svy_dataset)

# Create clean combined results table
clean_combined_results <- data.frame(
  Outcome = c(rep("Hypertension", 8), rep("BMI", 8)),
  Variable = c("Sex", "Age Category", "Marital Status", "Education", 
               "Wealth", "Urban/Rural", "Region", "BMI Category",
               "Sex", "Age Category", "Marital Status", "Education", 
               "Wealth", "Urban/Rural", "Region", "Hypertension"),
  Test_Statistic = round(c(
    # Hypertension results
    htn_sex_test$statistic,
    htn_age_test$statistic,
    htn_marital_test$statistic,
    htn_education_test$statistic,
    htn_wealth_test$statistic,
    htn_urban_test$statistic,
    htn_region_test$statistic,
    htn_bmi_test$statistic,
    # BMI results
    bmi_sex_test$statistic,
    bmi_age_test$statistic,
    bmi_marital_test$statistic,
    bmi_education_test$statistic,
    bmi_wealth_test$statistic,
    bmi_urban_test$statistic,
    bmi_region_test$statistic,
    bmi_htn_test$statistic
  ), 2),
  DF = round(c(
    # Hypertension results
    htn_sex_test$parameter,
    htn_age_test$parameter,
    htn_marital_test$parameter,
    htn_education_test$parameter,
    htn_wealth_test$parameter,
    htn_urban_test$parameter,
    htn_region_test$parameter,
    htn_bmi_test$parameter,
    # BMI results
    bmi_sex_test$parameter,
    bmi_age_test$parameter,
    bmi_marital_test$parameter,
    bmi_education_test$parameter,
    bmi_wealth_test$parameter,
    bmi_urban_test$parameter,
    bmi_region_test$parameter,
    bmi_htn_test$parameter
  ), 2),
  P_Value = c(
    # Hypertension results
    htn_sex_test$p.value,
    htn_age_test$p.value,
    htn_marital_test$p.value,
    htn_education_test$p.value,
    htn_wealth_test$p.value,
    htn_urban_test$p.value,
    htn_region_test$p.value,
    htn_bmi_test$p.value,
    # BMI results
    bmi_sex_test$p.value,
    bmi_age_test$p.value,
    bmi_marital_test$p.value,
    bmi_education_test$p.value,
    bmi_wealth_test$p.value,
    bmi_urban_test$p.value,
    bmi_region_test$p.value,
    bmi_htn_test$p.value
  )
)

# Format p-values
clean_combined_results$P_Value <- ifelse(clean_combined_results$P_Value < 0.001, 
                                         "< 0.001", 
                                         round(clean_combined_results$P_Value, 3))

# Print clean results
print(clean_combined_results)

# Optionally, write to CSV
write.csv(clean_combined_results, "clean_combined_results.csv", row.names = FALSE)
