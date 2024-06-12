# Add aridity data (CRU_TS_4.05) to flux sites

library(tidyverse)
library(sf) # for distance calculation

# Get list of flux sites

setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")
flx_site_info <- read_csv("combined_source_site_list_info_gpp_ignore_years.csv")
colnames(flx_site_info)[1] <- "SITE_ID" # need to make sure first column is labeled correctly

# Read in aridity data
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/aridity")
cru_aridity <- read_csv("cruts405_1991-2020_aridity.csv")


# Convert to sf files for distance
flx_site_info_sf <- st_as_sf(flx_site_info, coords = c("LOCATION_LONG", "LOCATION_LAT"),
                             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

cru_aridity_sf <- st_as_sf(cru_aridity, coords = c("Lon", "Lat"),
                           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Get minimum distance aridity grid cell for each flx site
flx_site_info$min_dist_m <- NA
flx_site_info$CRU_Aridity_P_PET <- NA
flx_site_info$CRU_P_Annual_Mean_Total_mm <- NA
flx_site_info$CRU_PET_Annual_Mean_Total_mm <- NA

# slow because it calculates all distances, could be made faster but works ok
for (i in 1:nrow(flx_site_info)){
  print(i)
  dist <- st_distance(flx_site_info_sf[i,], cru_aridity_sf)
  flx_site_info$min_dist_m[i] <- min(dist)
  aridity_output <- cru_aridity_sf[which.min(dist),]
  flx_site_info$CRU_Aridity_P_PET[i] <- aridity_output$Aridity_P_PET[1]
  flx_site_info$CRU_P_Annual_Mean_Total_mm[i] <- aridity_output$P_Annual_Mean_Total_mm[1]
  flx_site_info$CRU_PET_Annual_Mean_Total_mm[i] <- aridity_output$PET_Annual_Mean_Total_mm[1]
}

# delete extra junk columns afterwards

# Write out
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")
write_csv(flx_site_info, "combined_source_site_list_info_gpp_ignore_years_cru_aridity.csv")