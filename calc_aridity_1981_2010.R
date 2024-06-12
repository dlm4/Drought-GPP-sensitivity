# calc_aridity.R

# ORIGINALLY FROM: calc_trendy_gpp_precip_slopes.R

# create GPP ~ precip slopes per season from:
# TRENDY v6 GPP
# CRU TS 4.05 Precip (and PET, for aridity, P/PET)
# Also include in output:
#   lat and lon
#   land cover classes from UMD LC, 1993 reference
# calculate everything for >15 N latitude, can refine later
# Time range: 1981-2010, can change this too

# Analysis based on gpp_precip_slope.R

#####

# Set input and output folders
# ANOTHER TIME

# Load relevant libraries
library(rhdf5)
library(ncdf4)
library(sp)
library(trend)
library(zyp)
library(tidyverse)

#####
# set ranges, 1991-2020 for EC era now that we have ICOS 2020 data as well
#yr_range <- 1991:2020

# want to use 1981-2010 so we have a proper comparison with TerraClimate
yr_range <- 1981:2010

yrs <- rep(1901:2020, each = 12) # get list of all years, 12 elements each
mos <- rep(1:12, 120) # repeat 1:12 120 times, for 1901 through 2016
band_inds <- which(yrs %in% yr_range) # will be same for gpp and precip
latlim <- -90 # minimum latitude for analysis, do whole globe in this case

#####
# Get precip and PET
#library(ncdf4)
#setwd("D:/berkeley/TRENDY/CRU/cru_ts4.05.1901.2020")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/")
ncin <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc") # netcdf file

# get lat and lon
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")

# LOAD precip
dname <- "pre" # variable name, precipitation
pre_array <- ncvar_get(ncin, dname) # this takes a decent amount of time off of disk... much faster w ssd

# close connection to netcdf file
nc_close(ncin)
rm(ncin)

# LOAD PET
# Need to load PET, convert units, and calculate aridity (P/PET)
# date range for aridity calculation??
ncin <- nc_open("cru_ts4.05.1901.2020.pet.dat.nc") # netcdf file for pet
pet_array <- ncvar_get(ncin, "pet")
nc_close(ncin)
rm(ncin)
gc()

# latitude limit
latlim_inds <- which(lat > latlim) # latitude limit

# calculate aridity over range of years, 1991-2016
# northern hemisphere limit
ndays_permonth <- rep(c(rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 3), c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 120/4)
ndays_permonth_sub <- ndays_permonth[band_inds]
# need to find a better way to do this but here we are
pet_sum_sub <- matrix(NA, 720, length(latlim_inds))
pre_sum_sub <- matrix(NA, 720, length(latlim_inds))
base_j <- 360 - length(latlim_inds) # basis for latitude indexing in N hem
#aridity_sub <- matrix(NA, 720, length(latlim_inds))
for (i in 1:nrow(pet_sum_sub)){
  print(i)
  for (j in 1:ncol(pet_sum_sub)){
    # something wrong with this summing step, need to think about units...
    # NOTE the base_j + j indexing for columns
    pet_sum_sub[i, j] <- sum(pet_array[i, base_j + j, band_inds] * ndays_permonth_sub, na.rm = T) # think about how to handle NAs??
    pre_sum_sub[i, j] <- sum(pre_array[i, base_j + j, band_inds], na.rm = T)
  }
}

aridity_sub <- pre_sum_sub / pet_sum_sub
# set infinity values to NA, and NaN to NA, array misalignment
aridity_sub[which(aridity_sub == Inf | is.nan(aridity_sub))] <- NA
# will need to write out this aridity df eventually...

# IMAGES
library(lattice)
library(RColorBrewer)
lat_sub <- lat[latlim_inds]

# making sure the color range beyond limit
col <- brewer.pal(8,"YlGnBu")
breaks <- seq(0,4,0.5)
image(lon, lat_sub, aridity_sub,  breaks = breaks, col=col)

library(tidyverse)
lonlat_sub <- as.matrix(expand.grid(lon,lat_sub))
aridity_df <- cbind.data.frame(lonlat_sub, c(aridity_sub), c(pre_sum_sub/length(yr_range)), c(pet_sum_sub/length(yr_range)))
names(aridity_df) <- c("Lon", "Lat", "Aridity_P_PET", "P_Annual_Mean_Total_mm", "PET_Annual_Mean_Total_mm")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/aridity/")
#write_csv(aridity_df, "cruts405_2000-2016_aridity.csv")
#write_csv(aridity_df, "cruts405_1991-2016_aridity.csv")
#write_csv(aridity_df, "cruts405_1991-2020_aridity.csv")
write_csv(aridity_df, "cruts405_1981-2010_aridity.csv")

#####
# library(raster)
# library(tidyverse)
# aridity_df_test <- read_csv("cruts405_2000-2016_aridity.csv")
# aridity_r <- rasterFromXYZ(aridity_df_test, res = c(0.5, 0.5), crs = CRS("+init=epsg:4326"))
# plot(aridity_r)