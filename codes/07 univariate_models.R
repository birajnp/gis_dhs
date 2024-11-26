# Load required libraries
library(survey)
library(sjPlot)

# Sex Model
sex_model <- svyglm(
  bmi_category_nr_ob ~ hv104,
  design = svy_dataset,
  family = binomial()
)
summary(sex_model)
tab_model(sex_model)

# Age Model
age_model <- svyglm(
  bmi_category_nr_ob ~ age_cat,
  design = svy_dataset,
  family = binomial()
)
summary(age_model)
tab_model(age_model)

# Marital Status Model
marital_model <- svyglm(
  bmi_category_nr_ob ~ marital_status,
  design = svy_dataset,
  family = binomial()
)
summary(marital_model)
tab_model(marital_model)

# Education Model
education_model <- svyglm(
  bmi_category_nr_ob ~ edu_cat,
  design = svy_dataset,
  family = binomial()
)
summary(education_model)
tab_model(education_model)

# Wealth Model
wealth_model <- svyglm(
  bmi_category_nr_ob ~ wealth_cat,
  design = svy_dataset,
  family = binomial()
)
summary(wealth_model)
tab_model(wealth_model)

# Urban/Rural Model
urban_model <- svyglm(
  bmi_category_nr_ob ~ hv025,
  design = svy_dataset,
  family = binomial()
)
summary(urban_model)
tab_model(urban_model)

# High Blood Pressure Model
bp_model <- svyglm(
  bmi_category_nr_ob ~ high_blood_pressure,
  design = svy_dataset,
  family = binomial()
)
summary(bp_model)
tab_model(bp_model)

# Ecological Region Model
region_model <- svyglm(
  bmi_category_nr_ob ~ shecoreg,
  design = svy_dataset,
  family = binomial()
)
summary(region_model)
tab_model(region_model)






#===================================================
# univariate model for HTN
# Sex Model for HTN
sex_model_htn <- svyglm(
  high_blood_pressure ~ hv104,
  design = svy_dataset,
  family = binomial()
)
summary(sex_model_htn)
tab_model(sex_model_htn)

# Age Model for HTN
age_model_htn <- svyglm(
  high_blood_pressure ~ age_cat,
  design = svy_dataset,
  family = binomial()
)
summary(age_model_htn)
tab_model(age_model_htn)

# Marital Status Model for HTN
marital_model_htn <- svyglm(
  high_blood_pressure ~ marital_status,
  design = svy_dataset,
  family = binomial()
)
summary(marital_model_htn)
tab_model(marital_model_htn)

# BMI Model for HTN
bmi_model_htn <- svyglm(
  high_blood_pressure ~ bmi_category_nr_ob,
  design = svy_dataset,
  family = binomial()
)
summary(bmi_model_htn)
tab_model(bmi_model_htn)

# Education Model for HTN
education_model_htn <- svyglm(
  high_blood_pressure ~ edu_cat,
  design = svy_dataset,
  family = binomial()
)
summary(education_model_htn)
tab_model(education_model_htn)

# Wealth Model for HTN
wealth_model_htn <- svyglm(
  high_blood_pressure ~ wealth_cat,
  design = svy_dataset,
  family = binomial()
)
summary(wealth_model_htn)
tab_model(wealth_model_htn)

# Urban/Rural Model for HTN
urban_model_htn <- svyglm(
  high_blood_pressure ~ hv025,
  design = svy_dataset,
  family = binomial()
)
summary(urban_model_htn)
tab_model(urban_model_htn)

# Region Model for HTN
region_model_htn <- svyglm(
  high_blood_pressure ~ shecoreg,
  design = svy_dataset,
  family = binomial()
)
summary(region_model_htn)
tab_model(region_model_htn)

