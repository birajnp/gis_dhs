
library(here)

source(here("codes", "03 variable selection.R"))


#filtering male data
male_data <- b %>% 
  filter(hv104 == "male")

#filtering female data
female_data <- b %>% 
  filter(hv104 == "female")

sum(!is.na(female_data$ha40))

sum(!is.na(male_data$hb40))

# removing female data in male dataset
male_data <- select(
  male_data,  
  -c(ha40, #WOMENS BMI
     ha2,  # women's weight in kg
     ha3,  # women's height in centimeters
     wbp24,  # women's blood pressure final systolic reading
     wbp25,  # women's blood pressure final diastolic reading
     wbp26,  # women's blood pressure category
     wbp19,  
     wbp16 
     
  )
)

# removing male variables in female data
female_data <- select(
  female_data,  
  -c(hb40, #MENS BMI
     hb2,  # men's weight in kg
     hb3,  # men's height in centimeters
     mbp24,  # women's blood pressure final systolic reading
     mbp25,  # women's blood pressure final diastolic reading
     mbp26,  # women's blood pressure category
     mbp19,  
     mbp16
  )
)

# renaming variables
male_data <- male_data %>% rename("systolic_bp" = "mbp24",
                                  "diastolic_bp" = "mbp25",
                                  "blood_pressure_cat" = "mbp26",
                                  "BMI" = "hb40",
                                  "weight" = "hb2",
                                  "height" = "hb3",
                                  "BP_medication" = "mbp19",
                                  "diagnosed_HTN" = "mbp16"
                                  )


# renaming variables
female_data <- female_data %>% rename("systolic_bp" = "wbp24",
                                      "diastolic_bp" = "wbp25",
                                      "blood_pressure_cat" = "wbp26",
                                      "BMI" = "ha40",
                                      "weight" = "ha2",
                                      "height" = "ha3",
                                      "BP_medication" = "wbp19",
                                      "diagnosed_HTN" = "wbp16")




combined_data <- rbind(female_data, male_data)



sum(!is.na(combined_data$BMI))


# Define labels for all variables, including NULLs
labels <- list(
  sample_wt         = "household sample weight (6 decimals)",
  shecoreg          = "ecological region",
  shdist            = "district",
  hv104             = "sex of household member",
  hv105             = "age",
  hv115             = "current marital status",
  sh17b1            = "highest educational level attained - cs",
  hv107             = "highest year of education completed",
  hv040             = "cluster altitude in meters",
  hv024             = "province",
  hv025             = "type of place of residence",
  hv222             = "type of cook stove",
  hv223             = "type of cooking fuel",
  hv225             = "share toilet with other households",
  hv226             = "type of cooking fuel",
  hv241             = "food cooked in the house/ separate building/ outdoors",
  hfs1              = "household worried about food for lack of money or resources",
  hfs2              = "household members unable to eat healthy food for lack of money or resources",
  hfs3              = "household members ate only a few foods because of lack of money or resources",
  hfs4              = "household members skipped a meal because of lack of money or resources",
  hfs5              = "household members ate less than expected because of lack of money or resources",
  hfs6              = "household ran out of food because of lack of money or resources",
  hfs7              = "household members were hungry and did not eat because of lack of money or resources",
  hfs8              = "household members did not eat for a whole day because of lack of money or resources",
  hfs_mod           = "moderate or severe food insecurity probability",
  hfs_sev           = "severe food insecurity probability",
  systolic_bp       = "final blood pressure: systolic",
  diastolic_bp      = "final blood pressure: diastolic",
  blood_pressure_cat = "blood pressure category",
  BP_medication     = "is respondent taken medication to control blood pressure",
  diagnosed_HTN     = "ever been diagnosed by doctor/health worker with hypertension",
  BMI               = "body mass index",
  weight            = "weight in kilograms (1 decimal)",
  height            = "height in centimeters (1 decimal)",
  ha35              = "smoking (cigarettes in last 24 hours)",
  hb35              = "na - smoking (cigarettes in last 24 hours)",
  hv270             = "wealth index combined",
  OBJECTID          = "OBJECTID ",
  PROVINCE          = "PROVINCE",
  PR_NAME           = "PR_NAME ",
  DISTRICT          = "DISTRICT ",
  DHSID             = "DHSID",
  DHSCC             = "DHSCC",
  DHSYEAR           = "DHSYEAR",
  DHSCLUST          = "DHSCLUST",
  geometry          = "geometry",
  District          = "District")

# Loop through the labels and assign them to the combined dataset
for (var in names(labels)) {
  if (var %in% names(combined_data)) {
    var_label(combined_data[[var]]) <- labels[[var]]
  }
}


# converting a specific factor column to numeric and createing a new varaibles
combined_data$new_bmi <- as.numeric(as.character(combined_data$BMI))
combined_data$systolic_bp <- as.numeric(as.character(combined_data$systolic_bp))
combined_data$diastolic_bp <- as.numeric(as.character(combined_data$diastolic_bp))
combined_data$new_height <- as.numeric(as.character(combined_data$height))
combined_data$new_weight <- as.numeric(as.character(combined_data$weight))


combined_data$new_weight <- combined_data$new_weight / 10  # Divide by 10 to place the decimal point
combined_data$new_height <- combined_data$new_height / 1000
combined_data$new_bmi <- combined_data$new_bmi/100
summary(combined_data$new_bmi)

# creating a new categorical variable for obesity
combined_data$bmi_category <- cut(
  combined_data$new_bmi,
  breaks = c(-Inf, 18.5, 23, 25, Inf),
  labels = c("underweight", "Normal", "overweight", "obese"))


combined_data$bmi_category_nr_ob <- cut(
  combined_data$new_bmi,
  breaks = c(-Inf, 23, Inf),
  labels = c(0, 1)) # o normal #1 overweright


table(combined_data$bmi_category)
table(combined_data$bmi_category_nr_ob)


# Create a new variable for high blood pressure
# combined_data$high_blood_pressure <- ifelse(
#   combined_data$systolic_bp >= 140 | combined_data$diastolic_bp >= 90,
#   1,  # Indicates high blood pressure
#   0   # Indicates normal blood pressure
# )



combined_data <- combined_data %>%
  mutate(
    high_blood_pressure = ifelse(
      is.na(systolic_bp) | is.na(diastolic_bp), NA_real_,
      ifelse(systolic_bp >= 130 | diastolic_bp >= 80,
             1,  # Indicates high blood pressure
             0)))  # Indicates normal blood pressure
  

sum(!is.na(combined_data$diastolic_bp))

table(combined_data$high_blood_pressure)

length(combined_data$diastolic_bp)




# Create categorized blood pressure
combined_data <- combined_data %>%
  mutate(blood_pressure_cat_new = case_when(
    systolic_bp < 120 & diastolic_bp < 80 ~ "Normal",
    systolic_bp >= 120 & systolic_bp < 130 & diastolic_bp < 80 ~ "Elevated",
    (systolic_bp >= 130) | (diastolic_bp >= 80) ~ "Hypertensive",
    TRUE ~ NA_character_  # Handle any values that don't match
  ))

# Create numeric category based on the character category
combined_data <- combined_data %>%
  mutate(blood_pressure_cat_numeric = case_when(
    blood_pressure_cat_new == "Normal" ~ 1,
    blood_pressure_cat_new == "Elevated" ~ 2,
    blood_pressure_cat_new == "Hypertensive" ~ 3,
    TRUE ~ NA_real_  # Handle any values that don't match
  ))


table(combined_data$systolic_bp)
                                            
                                            
table(combined_data$blood_pressure_cat_numeric)

table(combined_data$blood_pressure_cat_new, combined_data$blood_pressure_cat)
table(combined_data$high_blood_pressure)



# clean dataset where either new_bmi or blood_pressure data is present
combined_data <- combined_data %>%
  filter(!is.na(bmi_category) | !is.na(blood_pressure_cat))



# Extracting longitude and latitude
combined_data <- combined_data %>%
  mutate(
    geometry = as.character(geometry),
    Long_x = format(as.numeric(str_extract(geometry, "(?<=c\\()[-\\d.]+")), nsmall = 6),
    Lati_y = format(as.numeric(str_extract(geometry, "(?<=, )[-\\d.]+(?=\\))")), nsmall = 6))


# Combine 'overweight' and 'obese' into 'overweight/obese'
combined_data <- combined_data %>%
  mutate(bmi_category = case_when(
    bmi_category %in% c("overweight", "obese") ~ "overweight/obese",
    TRUE ~ as.character(bmi_category)
  ))


print(table(combined_data$bmi_category))

#  factor levels BMI Category
combined_data$bmi_category <- factor(
  combined_data$bmi_category,
  levels = c("underweight", "Normal", "overweight/obese"),
  labels = c(1, 2, 3),
  ordered = TRUE)




# age categories
combined_data <- combined_data |> mutate (age_cat = case_when(
  hv105 <= 39 ~ "16-39",
  hv105 > 39 & hv105 <= 59 ~ "40-59",
  hv105 >= 60 ~ "60+"))



# education category
combined_data <- combined_data %>% mutate(edu_cat = case_when(
  sh17b1 == "no education" ~ "No education",
  sh17b1 == "don't know" ~ "No education",
  sh17b1 == "lower basic education (1-5)" ~ "Basic Education",
  sh17b1 == "upper basic education (6-8)" ~ "Basic Education",
  sh17b1 == "lower secondary (9-10)"  ~ "Secondary or Higher",
  sh17b1 == "higher secondary (11-12)"  ~ "Secondary or Higher",
  sh17b1 == "more than secondary (13 and above)" ~ "Secondary or Higher"))

# marital status
combined_data <- combined_data %>%
  mutate(marital_status = case_when(
      hv115 == "widowed" ~ "divorced/separated/widowed",
      hv115 == "divorced/sepearated" ~ "divorced/separated/widowed",
      hv115 == "married" ~ "Married",
      hv115 == "never married" ~ "Unmarried"
    ))
    



table(combined_data$marital_status)

combined_data <- combined_data %>% 
  mutate(wealth_cat = case_when(hv270 == "poorest" ~ "Poor",
                                hv270 == "poorer" ~ "Poor",
                                hv270 == "middle" ~ "Middle",
                                hv270 == "richer" ~ "Rich",
                                hv270 == "richest" ~ "Rich"))
                             


table(combined_data$bmi_category)

table(combined_data$blood_pressure_cat)
table(combined_data$blood_pressure_cat_new)

sum(!is.na(dhs22_personal$mbp24))
sum(!is.na(dhs22_personal$wbp24))
sum(!is.na(combined_data$blood_pressure_cat_new))
sum(!is.na(combined_data$blood_pressure_cat))

sum(!is.na(combined_data$BMI))
sum(!is.na(combined_data$bmi_category))
sum(is.na(dhs22_personal$hb40))

table(combined_data$bmi_category)


sum(!is.na(combined_data$high_blood_pressure))
sum(!is.na(combined_data$systolic_bp))

