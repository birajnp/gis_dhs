
library(here)
source(here("codes", "01 data load.R"))

# shape file of Nepal
nepal <- sf::st_read(here("raw_dataset", "hermes_NPL_new_wgs", "hermes_NPL_new_wgs_2.shp"))

# cluster GPS points
cluster <- sf::st_read(here("raw_dataset", "GPS_NDHS2022", "NPGE81FL", "NPGE81FL.shp"))

# read cluster shape file
cluster <- sf::st_read(here("raw_dataset", "GPS_NDHS2022", "districts.shp"))


#  dataset merge with cluster code
dhs22_personal <- dhs22_personal %>% 
  left_join(cluster, by =c("hv021" = "DHSCLUST"))


