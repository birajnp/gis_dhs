#library(here)
#source(here("codes", "05 result.R"))


Packages <- c("sf", "here","gtsummary","foreign","survey",'labelled',"readxl", 
              "tidyverse", "haven","rockchalk", "forcats", "nnet",
              "psycho", "sjPlot")


new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
lapply(Packages, require, character.only=T)

df <- read_rds(here("data_selected_gis_new.rds"))

table(df$high_blood_pressure)



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

svy_dataset$variables$high_blood_pressure <- as.factor(svy_dataset$variables$high_blood_pressure)
svy_dataset$variables$blood_pressure_cat_numeric <- as.factor(svy_dataset$variables$blood_pressure_cat_numeric)

# Specify categorical and continuous variables
cat_vars <- c("hv104", "age_cat", "marital_status", "edu_cat", 
              "wealth_cat", "high_blood_pressure", "bmi_category_nr_ob", "hv025", "shecoreg")

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


#str(svy_dataset)


# set seed
set.seed(123458)

table(svy_dataset$variables$blood_pressure_cat_numeric)


# 
svy_dataset$variables$age_cat <- relevel(svy_dataset$variables$age_cat, ref = "16-39")
svy_dataset$variables$edu_cat <- relevel(svy_dataset$variables$edu_cat, ref = "No education")
svy_dataset$variables$marital_status <- relevel(svy_dataset$variables$marital_status, ref = "Unmarried")
svy_dataset$variables$wealth_cat <- relevel(svy_dataset$variables$wealth_cat, ref = "Poor")
svy_dataset$variables$shecoreg <- relevel(svy_dataset$variables$shecoreg, ref = "terai")
#svy_dataset$variables$bmi_category_nr_ob <- as.numeric(svy_dataset$variables$bmi_category_nr_ob)

# Run logistic regression with survey design using binomial
bmi_model <- svyglm(
  bmi_category_nr_ob ~ 
    hv104 +           # sex
    age_cat +         # age categories
    marital_status + 
    edu_cat +         # education
    wealth_cat +      # wealth
    hv025 +           # urban/rural
    high_blood_pressure +
    shecoreg,         # ecological region
    #PR_NAME,
  design = svy_dataset,
  family = binomial())  # changed to binomial

summary(bmi_model)
tab_model(bmi_model)

model_data <- sjPlot::get_model_data(bmi_model, type = "est")

print(model_data)
summary(bmi_model)
#check multicollinearity
vif(bmi_model)

#Assess model fit
# Hosmer-Lemeshow test for survey data
residuals <- residuals(bmi_model, type = "deviance")
fitted_values <- fitted(bmi_model)
pseudo_r2 <- 1 - (bmi_model$deviance/bmi_model$null.deviance)
print(round(pseudo_r2, 3))
summary(residuals)

#histogram of residuals
hist(residuals, 
     breaks = 30,  # number of bars
     main = "Histogram of Deviance Residuals",
     xlab = "Deviance Residuals",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
# Add a vertical line at zero
abline(v = 0, col = "red", lty = 2)
# Add density curve (optional)
lines(density(residuals), col = "darkblue", lwd = 2)

# Save to CSV
write.csv(model_data, "bmi_model_results.csv")
tab_model(bmi_model, file = "bmi_model_results.html")

# 6. Get BMI model results
# Get BMI model results
bmi_results <- rbind(
  coef = coef(bmi_model),
  or = exp(coef(bmi_model)))

ci = exp(confint(bmi_model))

print(ci)
print(bmi_results)



write.csv(result_bmi, "model_bmi.csv")



# Converting character variables to factors
df$age_cat <- as.factor(df$age_cat)
df$marital_status <- as.factor(df$marital_status)
df$edu_cat <- as.factor(df$edu_cat)
df$wealth_cat <- as.factor(df$wealth_cat)
df$bmi_category <- factor(df$bmi_category, ordered = FALSE)
df$bmi_category <- relevel(df$bmi_category, ref = 2)
svy_dataset$variables$age_cat <- relevel(svy_dataset$variables$age_cat, ref = "16-39")
svy_dataset$variables$edu_cat <- relevel(svy_dataset$variables$edu_cat , ref = "No education")

#
# HTN ordinal logistic regression model with survey weights
htn_model <-  svyglm(high_blood_pressure ~ 
                        hv104 +      
                        age_cat +    
                        marital_status +
                        bmi_category_nr_ob +
                        edu_cat +    
                        wealth_cat + 
                        hv025 +
                       #hv024+
                        shecoreg,
                      design = svy_dataset,
                      family = binomial())


vif(htn_model)

summary(htn_model)

tab_model(htn_model)








# 8. Get HTN model results
htn_results <- list(
  coef = coef(htn_model),
  or = exp(coef(htn_model)),
  ci = exp(confint(htn_model)))


print(htn_results)

tab_model(htn_model)

tab_model(htn_model, file = "htn_model_results.html")


#Print formatted results
print(result_htn)

write.csv(a, "htn_model.csv")




