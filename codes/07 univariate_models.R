# Univariate ordinal logistic regression models for BMI
# 1. Sex (hv104)
bmi_sex_model <- svyolr(bmi_category ~ hv104, design = svy_dataset)
summary(bmi_sex_model)

# 2. Age categories
bmi_age_model <- svyolr(bmi_category ~ age_cat, design = svy_dataset)
summary(bmi_age_model)

# 3. Marital status
bmi_marital_model <- svyolr(bmi_category ~ marital_status, design = svy_dataset)
summary(bmi_marital_model)

# 4. Education categories
bmi_edu_model <- svyolr(bmi_category ~ edu_cat, design = svy_dataset)
summary(bmi_edu_model)

# 5. Wealth categories
bmi_wealth_model <- svyolr(bmi_category ~ wealth_cat, design = svy_dataset)
summary(bmi_wealth_model)

# 6. Urban/Rural (hv025)
bmi_urban_model <- svyolr(bmi_category ~ hv025, design = svy_dataset)
summary(bmi_urban_model)

# 7. Ecological region
bmi_region_model <- svyolr(bmi_category ~ shecoreg, design = svy_dataset)
summary(bmi_region_model)

## Function to extract odds ratios, confidence intervals using vcov matrix
get_or_ci <- function(model) {
  # Get coefficients and variance-covariance matrix
  coef_matrix1 <- coef(summary(model))
  vcov_matrix <- vcov(model)
  
  # Get standard errors from diagonal of vcov matrix
  se <- sqrt(diag(vcov_matrix))
  
  # Get coefficients
  or1 <- coef_matrix1[, "Value"]
  
  # Calculate t values using proper standard errors
  t_values <- or1/se
  
  # Calculate p values
  p_bmi <- 2 * pnorm(abs(t_values), lower.tail = FALSE)
  
  # Calculate confidence intervals using proper standard errors
  ci_lower <- or1 - (1.96 * se)
  ci_upper <- or1 + (1.96 * se)
  
  # Create results matrix
  result_matrix <- round(exp(cbind(
    OR = or1,
    "2.5 %" = ci_lower,
    "97.5 %" = ci_upper
  )), 2)
  
  # Add p-values
  result_matrix <- cbind(result_matrix, "p-value" = round(p_bmi, 3))
  
  # Add type 
  result_matrix <- cbind(
    "Type" = ifelse(grepl("\\|", rownames(coef_matrix1)), "Intercept", "Predictor"),
    result_matrix
  )
  
  # Convert to data frame to maintain structure
  result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
  
  # Convert numeric columns back to numeric
  result_df[,2:5] <- apply(result_df[,2:5], 2, as.numeric)
  
  return(result_df)
}


sex_or <- get_or_ci(bmi_sex_model)
age_or <- get_or_ci(bmi_age_model)
marital_or <- get_or_ci(bmi_marital_model)
edu_or <- get_or_ci(bmi_edu_model)
wealth_or <- get_or_ci(bmi_wealth_model)
urban_or <- get_or_ci(bmi_urban_model)
region_or <- get_or_ci(bmi_region_model)



print(sex_or)
summary(bmi_sex_model)


vcov(bmi_sex_model)






#===================================================================================
#===================================================================================
#htn model
# Sex and blood pressure
sex_model <- svyolr(blood_pressure_cat_numeric ~ hv104, 
                    design = svy_dataset)
summary(sex_model)

# Age and blood pressure
age_model <- svyolr(blood_pressure_cat_numeric ~ age_cat, 
                    design = svy_dataset)
summary(age_model)

# Marital status and blood pressure
marital_model <- svyolr(blood_pressure_cat_numeric ~ marital_status, 
                        design = svy_dataset)
summary(marital_model)

# BMI and blood pressure
bmi_model <- svyolr(blood_pressure_cat_numeric ~ bmi_category, 
                    design = svy_dataset)
summary(bmi_model)

# Education and blood pressure
education_model <- svyolr(blood_pressure_cat_numeric ~ edu_cat, 
                          design = svy_dataset)
summary(education_model)

# Wealth and blood pressure
wealth_model <- svyolr(blood_pressure_cat_numeric ~ wealth_cat, 
                       design = svy_dataset)
summary(wealth_model)

# Urban/Rural residence and blood pressure
residence_model <- svyolr(blood_pressure_cat_numeric ~ hv025, 
                          design = svy_dataset)
summary(residence_model)

# Region and blood pressure
region_model <- svyolr(blood_pressure_cat_numeric ~ shecoreg, 
                       design = svy_dataset)
summary(region_model)

exp(coef(region_model))

exp(confint(region_model))
summary(region_model)$coefficients[1:2]  # Assuming 3 categories -> 2 intercepts
