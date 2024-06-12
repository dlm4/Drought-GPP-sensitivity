
# Load all WorldClim2.1 data for all of the combined source (112 sites) site locations, output as a single file

library(tidyverse)
library(sf)
library(raster)
library(randomForest)

# Load FLUXNET2015 and/or oneflux location information, either as lat/lon or shapefile
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/site_locations")
# fluxsites <- read_sf("fluxnet2015_site_locations_plaintext_allsites.shp") # just lat lon points

# load full set of sites
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/")
flx_sites_all <- read_csv("combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

# load 10 year set of sites (for subsetting)
flx_sites_10yr <- read_csv("combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_10springyrs.csv")

# Change to sf shape (original fluxsites dataframe, gets appended)
fluxsites <- st_as_sf(flx_sites_all, coords = c("LOCATION_LONG", "LOCATION_LAT"),
         crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# WorldClim2.1 Bioclimate variables
wc2_bio <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 19))
colnames(wc2_bio) <- paste("wc2_bio", 1:19, sep = "_")
# Loop through bioclimate tif file vars
for (i in 1:19){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/original_layers") # location of bioclimate vars
  
  # change which one of these for each loop
  wc2_var_name <- paste("wc2.1_30s_bio_", i, ".tif", sep = "")
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get bioclimate information for that lat lon location or for point
  
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_bio[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 Elevation
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_elev")
wc2 <- brick("wc2.1_30s_elev.tif")
wc2_elev <- raster::extract(wc2, fluxsites) %>% as.vector()
colnames(wc2_elev) <- "wc2_elev"

# WorldClim2.1 Precipitation
wc2_prec <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_prec) <- paste("wc2_prec", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_prec")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_prec_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_prec_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get prec information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_prec[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 Solar Radiation
wc2_srad <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_srad) <- paste("wc2_srad", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_srad")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_srad_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_srad_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get srad information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_srad[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 Tavg
wc2_tavg <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_tavg) <- paste("wc2_tavg", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_tavg")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_tavg_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_tavg_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get tavg information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_tavg[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 Tmax
wc2_tmax <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_tmax) <- paste("wc2_tmax", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_tmax")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_tmax_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_tmax_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get tmax information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_tmax[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 Tmin
wc2_tmin <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_tmin) <- paste("wc2_tmin", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_tmin")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_tmin_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_tmin_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get tmin information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_tmin[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 water vapor pressure (NOT VPD)
wc2_vapr <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_vapr) <- paste("wc2_vapr", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_vapr")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_vapr_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_vapr_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get vapr information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_vapr[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# WorldClim2.1 wind speed
wc2_wind <- data.frame(matrix(NA, nrow = nrow(fluxsites), ncol = 12)) # 12 columns for 12 months
colnames(wc2_wind) <- paste("wc2_wind", 1:12, sep = "_")
# Loop through tif file vars
for (i in 1:12){
  print(i)
  # Loop through locations from fluxnet2015
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/wc2.1_30s/wc2.1_30s_wind")
  
  # change which one of these for each loop
  if (i < 10) {
    wc2_var_name <- paste("wc2.1_30s_wind_0", i, ".tif", sep = "") # less than 10 has a 0 in front
  } else {
    wc2_var_name <- paste("wc2.1_30s_wind_", i, ".tif", sep = "")
  }
  wc2 <- brick(wc2_var_name) # load in raster
  
  # get wind information for that lat lon location or for point
  # set coordinate ref system for points (projection)
  st_crs(fluxsites) <- crs(wc2)
  
  #site_info <- raster::extract(wc2, fluxsites)
  wc2_wind[, i] <- raster::extract(wc2, fluxsites) %>% as.vector()
}

# cbind everything to single data frame
fluxsites_wc2_bio_all <- cbind.data.frame(fluxsites$SITE_ID, wc2_bio, wc2_elev, wc2_prec, wc2_srad, wc2_tavg, wc2_tmax, wc2_tmin, wc2_vapr, wc2_wind)
colnames(fluxsites_wc2_bio_all)[1] <- "SITE_ID"

# write out here
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/")
write_csv(fluxsites_wc2_bio_all, file = "fluxsites_combinedsource_wc2_bio_all_vars.csv")

# subset for 10 year and also write out
fluxsites_wc2_bio_all_10yr <- subset(fluxsites_wc2_bio_all, subset = SITE_ID %in% flx_sites_10yr$SITE_ID)
write_csv(fluxsites_wc2_bio_all_10yr, file = "fluxsites_combinedsource_min10yr_wc2_bio_all_vars.csv")

# 
# #####
# # Subset and test rf regression
# # subset fluxsites_wc2_bio
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/processed_data")
# df_predict <- read.csv("fluxnet2015_seasonal_sensitivity_topredict.csv")
# fluxsites_wc2_bio_all <- subset(fluxsites_wc2_bio_all, fluxsites_wc2_bio_all$SITE_ID %in% unique(df_predict$SITE_ID))
# wc2_bio_all_sub <- fluxsites_wc2_bio_all[, 2:ncol(fluxsites_wc2_bio_all)]
# 
# set.seed(14)
# y <- df_predict$P_F_slope_spring
# rfreg_wc2_bio_all_spring <- randomForest(wc2_bio_all_sub, y, importance = TRUE)
# varImpPlot(rfreg_wc2_bio_all_spring)
# plot(rfreg_wc2_bio_all_spring$y, rfreg_wc2_bio_all_spring$predicted)
# mean(rfreg_wc2_bio_all_spring$rsq) # doesn't really seem to help the model too much overall, very similar psuedo Rsq
