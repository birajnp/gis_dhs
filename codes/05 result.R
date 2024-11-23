library(here)
source(here("codes", "04 data_harmonization_merging.R"))


table(combined_data$blood_pressure_cat_new)
#Selecting specific columns from combined_data
selected <- combined_data %>%
  select(DHSID, hv021, PR_NAME, DISTRICT, hv024, hv025, sample_wt, shecoreg,
         hv104, hv105, age_cat, marital_status, edu_cat,high_blood_pressure,
         blood_pressure_cat_numeric, blood_pressure_cat_new, BP_medication,
         diagnosed_HTN, systolic_bp, diastolic_bp, bmi_category, bmi_category_nr_ob, new_bmi, new_height,
         new_weight, wealth_cat, hv040, Long_x, Lati_y)

selected$dist_new <-  paste(selected$DISTRICT,selected$hv021, sep = "")


write_rds(selected, "data_selected_gis_new.rds")
write_csv(selected, "data_selected_gis_new.csv")
st_write(selected, 
         "selected_data.shp",  
         driver = "ESRI Shapefile",
         append = FALSE)

rm(list=ls())
