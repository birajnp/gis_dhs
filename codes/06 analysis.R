library(here)
source(here("codes", "05 result.R"))


Packages <- c("sf","here","gtsummary","foreign","survey",'labelled',"readxl", "tidyverse", "haven","rockchalk", "forcats")


new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
lapply(Packages, require, character.only=T)

df <- read_rds(here("data_selected_gis_new.rds"))


df <- df %>%
  mutate(forcats::as_factor(df)) %>%
  mutate(weight = as.numeric(sample_wt),
         psu = as.numeric(hv021))

#Creating the survey design object 
svy_dataset <- svydesign(
  ids = ~psu,
  weights = ~weight,
  data = df,
  nest = TRUE
)


# Specify categorical and continuous variables
cat_vars <- c("hv104", "age_cat", "marital_status", "edu_cat", 
              "wealth_cat", "blood_pressure_cat_new", "bmi_category", "hv025", "shecoreg")

cont_vars <- c("hv105", "systolic_bp", "diastolic_bp", "new_bmi", "new_height", "new_weight")


# Function to summarize categorical variables
get_cat_summary <- function(var) {
  formula <- as.formula(paste0("~", var))
  
  result <- svymean(formula, design = svy_dataset, na.rm = TRUE)
  counts <- svytotal(formula, design = svy_dataset, na.rm = TRUE)
  
  # Create the summary table without the Formatted column and with Total count
  data_frame(
    Variable = var,
    Category = gsub(paste0("^", var), "", names(result)),  # Clean up the category names
    Percent = round(coef(result) * 100, 1),
    SE = round(SE(result) * 100, 2),
    Total_Count = round(coef(counts), 0),  # Add the total count for each category
    Mean = NA,
    SD = NA
  )
}




# Generate summaries for categorical variables
cat_summaries <- lapply(cat_vars, get_cat_summary)
cat_summaries <- do.call(rbind, cat_summaries)


print(cat_summaries, n = 40)

# Function to summarize continuous variables
get_cont_summary <- function(var) {
  formula <- as.formula(paste0("~", var))
  
  # Get the mean and standard deviation for the continuous variable
  mean_result <- svymean(formula, design = svy_dataset, na.rm = TRUE)
  sd_result <- sqrt(svyvar(formula, design = svy_dataset, na.rm = TRUE))
  
  # Get the total count for the variable
  total_count <- sum(!is.na(df[[var]])) 
  
  # Create summary data frame
  data_frame(
    Variable = var,
    Category = "Mean (SD)",  # Placeholder as it's for continuous variables
    Percent = NA,            # Set Percent to NA for continuous variables
    SE = round(SE(mean_result), 2),   # Standard error of the mean
    Total_Count = total_count ,   # Total count for the continuous variable
    Mean = round(coef(mean_result), 2),  # Mean value of the variable
    SD = round(coef(sd_result), 2)  # Standard deviation of the variable
  )
}

# Generate summaries for continuous variables
cont_summaries <- lapply(cont_vars, get_cont_summary)
cont_summaries <- do.call(rbind, cont_summaries)
print((cont_summaries))



# Combine summaries into a single Table 1 data frame
table_1 <- rbind(cat_summaries, cont_summaries)

table_1 <- table_1 %>% 
  mutate(Mean_SD = paste(Mean, "(", SD, ")", sep = ""))

print(table_1, n = 50)

write.csv(table_1, "tab1.csv")


# Convert character variables to factors
svy_dataset$variables$age_cat <- factor(svy_dataset$variables$age_cat)
svy_dataset$variables$marital_status <- factor(svy_dataset$variables$marital_status)
svy_dataset$variables$edu_cat <- factor(svy_dataset$variables$edu_cat)
svy_dataset$variables$wealth_cat <- factor(svy_dataset$variables$wealth_cat)


str(svy_dataset)


# set seed
set.seed(123458)

table(svy_dataset$variables$blood_pressure_cat_numeric)


# 
table(svy_dataset$variables$bmi_category)

svy_dataset$variables$age_cat <- relevel(svy_dataset$variables$age_cat, ref = "16-39")
svy_dataset$variables$edu_cat <- relevel(svy_dataset$variables$edu_cat, ref = "No education")
svy_dataset$variables$marital_status <- relevel(svy_dataset$variables$marital_status, ref = "Unmarried")
svy_dataset$variables$wealth_cat <- relevel(svy_dataset$variables$wealth_cat, ref = "Poor")
svy_dataset$variables$shecoreg <- relevel(svy_dataset$variables$shecoreg, ref = "terai")
svy_dataset$variables$bmi_category <- factor(svy_dataset$variables$bmi_category, ordered = TRUE)   




# 1. BMI Ordinal Logistic Regression
bmi_model <- svyolr(bmi_category ~ 
                      hv104 +      # sex
                      age_cat +    # age categories
                      marital_status + 
                      edu_cat +    # education
                      wealth_cat + # wealth
                      hv025 +      # urban/rural
                      shecoreg,    # ecological region
                    design = svy_dataset)


summary(bmi_model)


#coef matrix
coef_matrix1<- round(coef(summary(bmi_model)),3)

# Calculate p value
p_bmi <- pnorm(abs(coef_matrix1[, "t value"]), lower.tail = F) * 2

# odds calculation
or1 <- round(coef(bmi_model),2)

#calculating ci

ci <- round(confint(bmi_model), 2)

result_bmi <- cbind(round(exp(cbind(OR = or1, ci)), 2), "p-value"=round(p_bmi,3))


#Print formatted results
print(result_bmi)



write.csv(result_bmi, "model_bmi.csv")




svy_dataset$variables$blood_pressure_cat_numeric <- factor(svy_dataset$variables$blood_pressure_cat_numeric, ordered = TRUE)
table(svy_dataset$variables$blood_pressure_cat_numeric)
svy_dataset$variables$bmi_category <- factor(svy_dataset$variables$bmi_category, ordered = FALSE)  # convert to unordered
svy_dataset$variables$bmi_category <- relevel(svy_dataset$variables$bmi_category, ref = 1)
svy_dataset$variables$hv104 <- relevel(svy_dataset$variables$hv104, ref = "female")

# HTN ordinal logistic regression model with survey weights
htn_model <- svyolr(blood_pressure_cat_numeric ~ 
                      hv104 +      # sex
                      age_cat +    # age categories
                      marital_status +
                      bmi_category +
                      edu_cat +    # education
                      wealth_cat + # wealth
                      hv025 +      # urban/rural
                      shecoreg,    # ecological region
                    design = svy_dataset)

summary(htn_model)



#coef matrix
coef_matrix2<- round(coef(summary(htn_model)),3)
# Calculate p value
p_htn <- pnorm(abs(coef_matrix2[, "t value"]), lower.tail = F) * 2

# odds calculation
or2 <- round(coef(htn_model),2)

#calculating ci

ci_htn <- round(confint(htn_model), 2)

result_htn <- cbind(round(exp(cbind(OR = or2, ci_htn)), 2), "p-value"=round(p_htn,3))


#Print formatted results
print(result_htn)

write.csv(result_htn, "htn_model.csv")





