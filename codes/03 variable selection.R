

library(here)
source(here("codes","02 gps_data integration.R"))
## variable selection

b <- dhs22_personal %>% 
  select(
    
    #caseid
   # sample_wt,
    #hhid, hv005, hv028,
    shdist,
    sample_wt, hv021,
    #ID,
    #hv003,
    shecoreg, shdist,
    
    hv104,  hv105, hv115,  #sex of respondent # age # current marital status
    
   sh17b1, # education
    #hv106,
    hv107,  #Education level
    hv040, # cluster elevation
    hv024:hv025, # type of residency
    shecoreg, # ecological belt
    hv222:hv226, # other information
    
    hv241,  # ecological belt
    
    hfs1:hfs_sev, #wbp15,
    #body mass index (BMI) women
    
    ha40,
    # womens weight in kg
    ha2,
    # womens height in centimeter
    ha3,
    # Dependent variables (BP Women)
    # women blood pressure first systolic reading
    #wbp9,
    #women blood pressure first diastolic reading
    #wbp10,
    #women blood pressure second systolic reading
    #wbp13,
    #women blood pressure second diastolic reading
    #wbp14,
    #women blood pressure third systolic reading
    #wbp22,
    #women blood pressure third diastolic reading
    #wbp23,
    #women blood pressure Final systolic reading
    wbp24,
    #women blood pressure Final diastolic reading
    wbp25,
    # women blood pressure category
    wbp26,
    
    # men blood pressure first systolic reading
    #mbp9,
    #men blood pressure first diastolic reading
    #mbp10,
    #men blood pressure second systolic reading
    #mbp13,
    #men blood pressure second diastolic reading
    #mbp14,
    #men blood pressure third systolic reading
   # mbp22,
    #men blood pressure third diastolic reading
    #mbp23,
    #men blood pressure Final systolic reading
    mbp24,
    #men blood pressure Final diastolic reading
    mbp25,
    # men blood pressure category
    mbp26,# mbp15,
    
    # medication for BP
    mbp19, wbp19,
    mbp16, wbp16,
   
    
    #mens body mass index (BMI)
    
    hb40,
    
    # mens weight in kg
    
    hb2,
    
    # mens height in centimeter
    
    hb3,
    
    # Provence, type of residence
    hv024,hv025,
    
    # Independent variables
    
    # smoking in last 24 hour
    
    ha35, hb35,
    
    #place of residence
    hv025,  
    
    # wealth: hv270
    hv270,
    
    #samplewt to gps
    OBJECTID:District)



b$hv105 <- as.numeric(b$hv105)



# filtering the population with age >=15
b <- b %>% filter(hv105 >= 15)




summary(dhs22_personal$ha13)
summary(dhs22_personal$hb40)
