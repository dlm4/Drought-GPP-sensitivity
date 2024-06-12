# calc_trendy_gpp_precip_slopes.R
# outputs to v4_pval

# try reprocessing at home to make sure it outputs correctly on the Mac

#####

# Julia suggests fields package

# create GPP ~ precip slopes per season from:
# TRENDY v6 GPP
# CRU TS 4.05 Precip (and PET, for aridity, P/PET)
# Also include in output:
#   lat and lon
#   land cover classes from UMD LC, 1993 reference & MODIS land cover
# calculate everything for >15 N latitude, can refine later
# Time range: 1991-2016, can change this too

# Analysis based on gpp_precip_slope.R

#####

# Set input and output folders
# ANOTHER TIME

# Load libraries
library(rhdf5)
library(ncdf4)
library(sp)
library(trend)
library(zyp)
library(tidyverse)

library(raster)
library(tidyverse)

library(sp)

library(R.matlab)

library(foreach)
library(doParallel)

library(abind)

#####
# set ranges
rs_types <- c("MODIS", "GOSIF_GPP", "FLUXCOM_RS", "FLUXCOM_RS_METEO")

for (rs_ind in 3:4){ # loop over all 4
#rs_ind <- 2
#for (rs_ind in 1:length(rs_types))
rs_source <- rs_types[rs_ind]

yrs <- rep(1901:2016, each = 12) # get list of all years, 12 elements each
mos <- rep(1:12, 116) # repeat 1:12 116 times, for 1901 through 2016
# band_inds <- which(yrs %in% 1992:2016) # will be same for gpp and precip

latlim <- 15 # minimum latitude for analysis, 15 is beyond minimum # new minimum is 30, but filtering for locations for individual flux sites

if (rs_source == "MODIS"){
  band_inds <- which(yrs %in% 2001:2016)
  yr_range <- "2001_2016"
} else if (rs_source == "GOSIF_GPP"){
  band_inds <- which(yrs %in% 2000:2016) 
  band_inds <- band_inds[3:length(band_inds)] # start in March for this data
  yr_range <- "2000_2016"
} else if (rs_source == "FLUXCOM_RS"){
  band_inds <- which(yrs %in% 2001:2016)
  yr_range <- "2001_2016"
} else if (rs_source == "FLUXCOM_RS_METEO"){
  # Started this in 1992
  band_inds <- which(yrs %in% 1992:2016)
  yr_range <- "1992_2016"
}

#####
# Get lat, lon, and aridity
#library(ncdf4)
#setwd("D:/berkeley/TRENDY/CRU/cru_ts4.05.1901.2020")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/") # ssd internal path
ncin <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc") # netcdf file

# get lat and lon - need these for reference coordinates
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
# 

# close connection to netcdf file
nc_close(ncin)
rm(ncin)

# latitude limit
latlim_inds <- which(lat > latlim) # latitude limit

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/aridity/")
aridity_df <- read_csv("cruts405_1991-2016_aridity.csv") # needed to fix this! was starting in 2000 previously
aridity_r <- rasterFromXYZ(aridity_df, res = c(0.5, 0.5), crs = CRS("+init=epsg:4326"))
#plot(aridity_r)

aridity_df_sub <- aridity_df[aridity_df$Lat > latlim,]
rm(aridity_r)
gc()

#####
# Veg cover for reference
# veg cover, UMD LC from 1993, could update later...
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/umd_global_lc/ISLSCP_II_UMD_LC_969/ISLSCP_II_UMD_LC_969/data/umd_landcover_hdeg/umd_landcover_hdeg")
umdlc <- read.asciigrid("umd_landcover_class_hd.asc")
umdlc_mat <- as.matrix(umdlc) # make matrix
umdlc_mat <- umdlc_mat[, rev(seq_len(ncol(umdlc_mat)))] # flip columns
umdlc_mat_sub <- umdlc_mat[,latlim_inds]

# Use MODIS classes from Trevor at 0.5 degree as well
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/")
modis_pft <- R.matlab::readMat("MODIS_PFTS_Gridded.mat")
# 360 rows, 720 columns, need to swap?
#modis_pft$landCoverPadded
modis_pft <- aperm(modis_pft$landCoverPadded, c(2,1)) # swap rows and columns
modis_pft <- modis_pft[, rev(seq_len(ncol(modis_pft)))] # flip columns
modis_pft_sub <- modis_pft[,latlim_inds]
#image(lon,lat_sub, modis_pft_sub, col=rev(brewer.pal(10,"RdBu")))

#####
# Get list of input GPP files - will need to redo this!

#####

# LOOP and process GPP files
#library(rhdf5)
#setwd("D:/berkeley/TRENDY/TRENDYv6_processed/GPP")
#setwd("/Volumes/Helios/berkeley/TRENDY/TRENDYv6_processed/GPP")
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/TRENDY_S2/GPP")
# list all *.mat gpp files
#gpp_filelist <- list.files(pattern = glob2rx(paste("*S2*gpp*.mat", sep = "")))

#####

# Just do CABLE_S2

# READ IN PRECIP / TEMP / SW
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/TRENDY_S2/CABLE_met/")

precip <- nc_open("CABLE_S2_pr.nc")

#dname <- "pr" # variable name, precipitation
# 1573 is January 1991, will want to change this to not be hard coded for different models
# 1585 is Jan 1992
pre_array <- ncvar_get(precip, "pr", start = c(1, 1, 1585))
# 720 rows, 360 columns, 312 layers
# need to flip
pre_array <- pre_array[, rev(seq_len(ncol(pre_array))),] # flip columns
# 
# close connection to netcdf file
nc_close(precip)
rm(precip)

# need to figure out which way is up in precip file and subset latitude
pre_sub <- pre_array[, latlim_inds,]
rm(pre_array)
gc()
# kg / m2 / s --> mm, will be same conversion as for GPP

# AIRTEMP, K
ta <- nc_open("CABLE_S2_tas.nc")
ta_array <- ncvar_get(ta, "tas", start = c(1, 1, 1585))
ta_array <- ta_array[, rev(seq_len(ncol(ta_array))),] # flip columns
nc_close(ta)
rm(ta)
ta_sub <- ta_array[, latlim_inds,]
rm(ta_array)
gc()
#image(lon,lat_sub, ta_sub[,,1], col=rev(brewer.pal(10,"RdBu")))


# SW, W m-2
sw <- nc_open("CABLE_S2_rsds.nc")
sw_array <- ncvar_get(sw, "rsds", start = c(1, 1, 1585))
sw_array <- sw_array[, rev(seq_len(ncol(sw_array))),] # flip columns
nc_close(sw)
rm(sw)
sw_sub <- sw_array[, latlim_inds,]
rm(sw_array)
gc()
#image(lon,lat_sub, sw_sub[,,1], col=rev(brewer.pal(10,"RdBu")))

# detrendGPP function
#library(trend)
#library(zyp)

#####

detrendGPP <- function(gpp_vec, yr_vec){
  # Needs library(trend) and library(zyp)
  # Usual inputs example
  # detrendGPP(gpp_sub_spring_sum[i,j,], unique(yr_sub))
  
  # If less than half the values are NaN, then process, otherwise skip and do nothing
  # Change if trend is on less than 5 points, don't detrend
  #if (length(which(is.na(gpp_vec)))/length(gpp_vec) < 0.5){
  if (length(gpp_vec) - length(which(is.na(gpp_vec))) >= 5){ # Needs at least 5 points to detrend!
    
    # list of indexes that are NOT NaN
    good_inds <- which(!is.na(gpp_vec))
    
    mktest_result <- trend::mk.test(gpp_vec[good_inds]) # do mann kendall test for trend over time for gpp, values that are NOT NaN
    
    # needed to fix detrend setting, otherwise NaN occurs for p.value if they all have the same value
    if (!is.na(mktest_result$p.value)){
      
      if (mktest_result$p.value < 0.05){
        x <- yr_vec[good_inds] # x is years
        y <- gpp_vec[good_inds] # y is gpp
        zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
        # subtract zyp.sen trend from original GPP values to get detrended version of GPP
        # add mean to put values in correct detrended order of magnitude
        gpp_vec[good_inds] <- gpp_vec[good_inds] - (zypsen_form$coefficients[2]*x + zypsen_form$coefficients[1]) + mean(gpp_vec[good_inds])
      }
      
    }
  }
  return(gpp_vec)
}

# Now includes slope se!
getSlopeIntInfo <- function(gpp_vec, met_vec){
  # if it's all NA or NaN or less than 2 elements not NA, don't calculate
  if (all(is.na(gpp_vec)) || all(is.na(met_vec)) ||
      length(which(!is.na(gpp_vec))) < 2 ||
      length(which(!is.na(met_vec))) < 2){
    slope_val <- NA
    int_val <- NA
    r2_val <- NA
    p_val <- NA
    slope_se <- NA
  } else {
    lm_summary <- summary(lm(gpp_vec ~ met_vec))
    r2_val <- lm_summary$r.squared
    # if there are not two rows of coeffs, then it failed
    if (nrow(lm_summary$coefficients) == 2) {
      slope_val <- lm_summary$coefficients[2,1]
      int_val <- lm_summary$coefficients[1,1]
      p_val <- lm_summary$coefficients[2,4]
      slope_se <- lm_summary$coefficients[2,2] # new: added slope standard error
    } else {
      slope_val <- NA
      int_val <- NA
      p_val <- NA
      slope_se <- NA
    }
  }
  return(c(slope_val, int_val, r2_val, p_val, slope_se))
}

# sum function, but return NA if all elements are NA
# https://stackoverflow.com/questions/56473098/efficient-way-to-calculate-sum-or-return-na-if-all-values-are-na
# slower than I'd want, but here we are
sumna <- function(x) {
  if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
}


# Loop over all S2 files
#registerDoParallel(detectCores() - 2)
#foreach(file_ind=1:length(gpp_filelist), .packages = ) %dopar% {
#for (file_ind in 1:length(gpp_filelist)){
#file_ind <- 15
  #matfile <- gpp_filelist[1]
  #gppfile <- gpp_filelist[file_ind]
  #print(paste("Working with", gppfile))
  
  # READ IN GPP - using the mat files
  #gpp <- h5read("CABLE_S3_gpp.mat", name = "gpp_05") # test!
  #setwd("D:/berkeley/TRENDY/TRENDYv6_processed/GPP")
  #setwd("/Volumes/Helios/berkeley/TRENDY/TRENDYv6_processed/GPP")
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/TRENDY_S2/GPP")
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/MOD17A2H_monthly_agg_full_list_05")
  #gpp <- h5read(gppfile, name = "gpp_05")
  # double check but I think they all should be named gpp_05
  
  #setwd("D:/berkeley/TRENDY/TRENDYv6_originals_sub/")
  #gpp <- nc_open("gpp/CABLE_S2_gpp.nc")
  
  #dname <- "pr" # variable name, precipitation
  # 1573 is January 1991, will want to change this to not be hard coded for different models
  #gpp_array <- ncvar_get(gpp, "gpp", start = c(1, 1, 1573))
  # 720 rows, 360 columns, 312 layers
  # need to flip
  #gpp_array <- gpp_array[, rev(seq_len(ncol(gpp_array))),] # flip columns
  # 
  # close connection to netcdf file
  #nc_close(gpp)
  #rm(gpp)
  
  # this should behave the same way as the processed version did.
  
  # # rows and cols flipped compared with CRU data but that's ok
  # # don't need to do this for pr, tas, and rsds files?
  # gpp <- aperm(gpp, c(2,1,3)) # swap rows and columns
  # gpp <- gpp[, rev(seq_len(ncol(gpp))),] # flip columns
  # gc() # clean up RAM
  
if (rs_source == "MODIS"){
  # MODIS MOD17: 2001-01 through 2021-01
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/")
  setwd("MOD17A2H_monthly_agg_full_list_05")
  filelist <- list.files()[grep("*05deg.tif", list.files())]
  # load and stack everything, first band only
  allrasters <- stack(filelist, bands = 1) # set bands = 1 to just load first band, matters with the MODIS tif files
  img <- brick(allrasters) # this takes a while to load everything into memory
  gpp <- as.array(img)
  gpp <- aperm(gpp, c(2,1,3)) # switch rows and columns
  gpp <- gpp[, rev(seq_len(ncol(gpp))),] # reverse columns
  
  # subset time index to range, and limit to > 15 N (could pick different threshold)
  modis_band_inds <- 1:(12*16)
  gpp_sub <- gpp[, latlim_inds, modis_band_inds]
  yrs_sub <- yrs[band_inds] # these are the same band_inds
  mos_sub <- mos[band_inds] # these are the same band_inds
  
} else if (rs_source == "GOSIF_GPP"){
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/")
  setwd("gosif_gpp_05deg")
  filelist <- list.files()[grep("*05deg.tif", list.files())]
  # load and stack everything, first band only
  allrasters <- stack(filelist, bands = 1) # set bands = 1 to just load first band, matters with the MODIS tif files
  img <- brick(allrasters) # this takes a while to load everything into memory
  gpp <- as.array(img)
  gpp <- aperm(gpp, c(2,1,3)) # switch rows and columns
  gpp <- gpp[, rev(seq_len(ncol(gpp))),] # reverse columns
  
  # subset time index to range, and limit to > 15 N (could pick different threshold)
  gosif_band_inds <- 1:(12*17)+2
  gosif_band_inds <- gosif_band_inds[1:(length(gosif_band_inds)-2)]
  gpp_sub <- gpp[, latlim_inds, gosif_band_inds]
  yrs_sub <- yrs[band_inds] # these are the same band_inds
  mos_sub <- mos[band_inds] # these are the same band_inds
  
  
} else if (rs_source == "FLUXCOM_RS"){
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/")
  setwd("fluxcom_rs_v006_05deg")
  filelist <- list.files()[grep("*05deg.tif", list.files())]
  # load and stack everything, first band only
  allrasters <- stack(filelist, bands = 1) # set bands = 1 to just load first band, FLUXCOM_RS is single layer anyway
  img <- brick(allrasters) # this takes a while to load everything into memory
  gpp <- as.array(img)
  gpp <- aperm(gpp, c(2,1,3)) # switch rows and columns
  gpp <- gpp[, rev(seq_len(ncol(gpp))),] # reverse columns
  
  # subset time index to range, and limit to > 15 N (could pick different threshold)
  fluxcom_rs_band_inds <- 1:(12*16)
  gpp_sub <- gpp[, latlim_inds, fluxcom_rs_band_inds]
  yrs_sub <- yrs[band_inds] # these are the same band_inds
  mos_sub <- mos[band_inds] # these are the same band_inds
  
  
} else if (rs_source == "FLUXCOM_RS_METEO"){
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_meteo_era5")
  filelist <- list.files()[grep("*.nc", list.files())]
  
  filelist_df <- do.call(rbind, strsplit(filelist, '[.]'))
  filelist_inds <- which(as.numeric(filelist_df[,8]) %in% 1992:2016)
  
  nc <- nc_open(filelist[filelist_inds[1]])
  gpp <- ncvar_get(nc, "GPP", start = c(1, 1, 1))
  nc_close(nc)
  
  for(ind in 2:length(filelist_inds)){
    nc <- nc_open(filelist[filelist_inds[ind]])
    gpp_yr2 <- ncvar_get(nc, "GPP", start = c(1, 1, 1))
    nc_close(nc)
    
    gpp <- abind(gpp, gpp_yr2, along = 3)
  }

  gpp <- gpp[, rev(seq_len(ncol(gpp))),] # reverse columns
  gpp_sub <- gpp[, latlim_inds,]
  yrs_sub <- yrs[band_inds] # these are the same band_inds
  mos_sub <- mos[band_inds] # these are the same band_inds
}
  
  # don't use band_inds with MODIS data because of new indexing, only need to subset last few years to get it to be:
  # 2000-01 through 2016-12
  
  lat_sub <- lat[latlim_inds]
  
  # remove gpp, don't need it anymore now that we're subsetting
  #rm(gpp)
  #rm(gpp_array)
  gc()
  
  # library(lattice)
  # library(RColorBrewer)
  # image(lon,lat,gpp_sub[,,6], col=rev(brewer.pal(10,"RdBu")))
  
  
  #####
  # Do seasonal aggregation / subsetting
  # northern hemisphere seasons
  spring_mos <- 3:5
  summer_mos <- 6:8
  fall_mos <- 9:11
  
  gpp_sub_spring <- gpp_sub[,,which(mos_sub %in% spring_mos)] # get GPP for spring months in year range
  pre_sub_spring <- pre_sub[,,which(mos_sub %in% spring_mos)] # get precip for spring months in year range
  ta_sub_spring <- ta_sub[,,which(mos_sub %in% spring_mos)] # get ta for spring months in year range
  sw_sub_spring <- sw_sub[,,which(mos_sub %in% spring_mos)] # get sw for spring months in year range
  yrs_sub_spring <- yrs_sub[which(mos_sub %in% spring_mos)] # get yrs index, same length
  
  gpp_sub_summer <- gpp_sub[,,which(mos_sub %in% summer_mos)] # get GPP for summer months in year range
  pre_sub_summer <- pre_sub[,,which(mos_sub %in% summer_mos)] # get precip for summer months in year range
  ta_sub_summer <- ta_sub[,,which(mos_sub %in% summer_mos)] # get ta for summer months in year range
  sw_sub_summer <- sw_sub[,,which(mos_sub %in% summer_mos)] # get sw for summer months in year range
  
  gpp_sub_fall <- gpp_sub[,,which(mos_sub %in% fall_mos)] # get GPP for fall months in year range
  pre_sub_fall <- pre_sub[,,which(mos_sub %in% fall_mos)] # get precip for fall months in year range
  ta_sub_fall <- ta_sub[,,which(mos_sub %in% fall_mos)] # get ta for fall months in year range
  sw_sub_fall <- sw_sub[,,which(mos_sub %in% fall_mos)] # get sw for fall months in year range
  

  # loop? this is lazy, need to think of a better way to aggregate through this...
  nyrs <- length(unique(yrs_sub_spring))
  #gpp
  gpp_sub_spring_sum <- array(data = NA, dim = c(nrow(gpp_sub_spring), ncol(gpp_sub_spring), nyrs))
  gpp_sub_summer_sum <- array(data = NA, dim = c(nrow(gpp_sub_summer), ncol(gpp_sub_summer), nyrs))
  gpp_sub_fall_sum <- array(data = NA, dim = c(nrow(gpp_sub_fall), ncol(gpp_sub_fall), nyrs))
  
  #####
  # precip slope setup
  # precip
  pre_sub_spring_sum <- array(data = NA, dim = c(nrow(pre_sub_spring), ncol(pre_sub_spring), nyrs))
  pre_sub_summer_sum <- array(data = NA, dim = c(nrow(pre_sub_summer), ncol(pre_sub_summer), nyrs))
  pre_sub_fall_sum <- array(data = NA, dim = c(nrow(pre_sub_fall), ncol(pre_sub_fall), nyrs))
  
  # slope, gpp ~ precip
  gpp_pre_slope_spring <- matrix(data = NA, nrow = nrow(pre_sub_spring), ncol = ncol(pre_sub_spring))
  gpp_pre_slope_summer <- matrix(data = NA, nrow = nrow(pre_sub_summer), ncol = ncol(pre_sub_summer))
  gpp_pre_slope_fall <- matrix(data = NA, nrow = nrow(pre_sub_fall), ncol = ncol(pre_sub_fall))
  
  # intercept
  gpp_pre_intercept_spring <- matrix(data = NA, nrow = nrow(pre_sub_spring), ncol = ncol(pre_sub_spring))
  gpp_pre_intercept_summer <- matrix(data = NA, nrow = nrow(pre_sub_summer), ncol = ncol(pre_sub_summer))
  gpp_pre_intercept_fall <- matrix(data = NA, nrow = nrow(pre_sub_fall), ncol = ncol(pre_sub_fall))
  
  # R2
  gpp_pre_r2_spring <- matrix(data = NA, nrow = nrow(pre_sub_spring), ncol = ncol(pre_sub_spring))
  gpp_pre_r2_summer <- matrix(data = NA, nrow = nrow(pre_sub_summer), ncol = ncol(pre_sub_summer))
  gpp_pre_r2_fall <- matrix(data = NA, nrow = nrow(pre_sub_fall), ncol = ncol(pre_sub_fall))
  
  # pval
  gpp_pre_pval_spring <- matrix(data = NA, nrow = nrow(pre_sub_spring), ncol = ncol(pre_sub_spring))
  gpp_pre_pval_summer <- matrix(data = NA, nrow = nrow(pre_sub_summer), ncol = ncol(pre_sub_summer))
  gpp_pre_pval_fall <- matrix(data = NA, nrow = nrow(pre_sub_fall), ncol = ncol(pre_sub_fall))
  
  # slope_se
  gpp_pre_se_spring <- matrix(data = NA, nrow = nrow(pre_sub_spring), ncol = ncol(pre_sub_spring))
  gpp_pre_se_summer <- matrix(data = NA, nrow = nrow(pre_sub_summer), ncol = ncol(pre_sub_summer))
  gpp_pre_se_fall <- matrix(data = NA, nrow = nrow(pre_sub_fall), ncol = ncol(pre_sub_fall))
  
  #####
  # ta slope setup
  # ta
  ta_sub_spring_mean <- array(data = NA, dim = c(nrow(ta_sub_spring), ncol(ta_sub_spring), nyrs))
  ta_sub_summer_mean <- array(data = NA, dim = c(nrow(ta_sub_summer), ncol(ta_sub_summer), nyrs))
  ta_sub_fall_mean <- array(data = NA, dim = c(nrow(ta_sub_fall), ncol(ta_sub_fall), nyrs))
  
  # slope, gpp ~ ta
  gpp_ta_slope_spring <- matrix(data = NA, nrow = nrow(ta_sub_spring), ncol = ncol(ta_sub_spring))
  gpp_ta_slope_summer <- matrix(data = NA, nrow = nrow(ta_sub_summer), ncol = ncol(ta_sub_summer))
  gpp_ta_slope_fall <- matrix(data = NA, nrow = nrow(ta_sub_fall), ncol = ncol(ta_sub_fall))
  
  # intercept
  gpp_ta_intercept_spring <- matrix(data = NA, nrow = nrow(ta_sub_spring), ncol = ncol(ta_sub_spring))
  gpp_ta_intercept_summer <- matrix(data = NA, nrow = nrow(ta_sub_summer), ncol = ncol(ta_sub_summer))
  gpp_ta_intercept_fall <- matrix(data = NA, nrow = nrow(ta_sub_fall), ncol = ncol(ta_sub_fall))
  
  # R2
  gpp_ta_r2_spring <- matrix(data = NA, nrow = nrow(ta_sub_spring), ncol = ncol(ta_sub_spring))
  gpp_ta_r2_summer <- matrix(data = NA, nrow = nrow(ta_sub_summer), ncol = ncol(ta_sub_summer))
  gpp_ta_r2_fall <- matrix(data = NA, nrow = nrow(ta_sub_fall), ncol = ncol(ta_sub_fall))
  
  # pval
  gpp_ta_pval_spring <- matrix(data = NA, nrow = nrow(ta_sub_spring), ncol = ncol(ta_sub_spring))
  gpp_ta_pval_summer <- matrix(data = NA, nrow = nrow(ta_sub_summer), ncol = ncol(ta_sub_summer))
  gpp_ta_pval_fall <- matrix(data = NA, nrow = nrow(ta_sub_fall), ncol = ncol(ta_sub_fall))
  
  # slope se
  gpp_ta_se_spring <- matrix(data = NA, nrow = nrow(ta_sub_spring), ncol = ncol(ta_sub_spring))
  gpp_ta_se_summer <- matrix(data = NA, nrow = nrow(ta_sub_summer), ncol = ncol(ta_sub_summer))
  gpp_ta_se_fall <- matrix(data = NA, nrow = nrow(ta_sub_fall), ncol = ncol(ta_sub_fall))
  
  #####
  # sw slope setup, except convert to PAR
  # par
  par_sub_spring_sum <- array(data = NA, dim = c(nrow(sw_sub_spring), ncol(sw_sub_spring), nyrs))
  par_sub_summer_sum <- array(data = NA, dim = c(nrow(sw_sub_summer), ncol(sw_sub_summer), nyrs))
  par_sub_fall_sum <- array(data = NA, dim = c(nrow(sw_sub_fall), ncol(sw_sub_fall), nyrs))
  
  # slope, gpp ~ par
  gpp_par_slope_spring <- matrix(data = NA, nrow = nrow(sw_sub_spring), ncol = ncol(sw_sub_spring))
  gpp_par_slope_summer <- matrix(data = NA, nrow = nrow(sw_sub_summer), ncol = ncol(sw_sub_summer))
  gpp_par_slope_fall <- matrix(data = NA, nrow = nrow(sw_sub_fall), ncol = ncol(sw_sub_fall))
  
  # intercept
  gpp_par_intercept_spring <- matrix(data = NA, nrow = nrow(sw_sub_spring), ncol = ncol(sw_sub_spring))
  gpp_par_intercept_summer <- matrix(data = NA, nrow = nrow(sw_sub_summer), ncol = ncol(sw_sub_summer))
  gpp_par_intercept_fall <- matrix(data = NA, nrow = nrow(sw_sub_fall), ncol = ncol(sw_sub_fall))
  
  # R2
  gpp_par_r2_spring <- matrix(data = NA, nrow = nrow(sw_sub_spring), ncol = ncol(sw_sub_spring))
  gpp_par_r2_summer <- matrix(data = NA, nrow = nrow(sw_sub_summer), ncol = ncol(sw_sub_summer))
  gpp_par_r2_fall <- matrix(data = NA, nrow = nrow(sw_sub_fall), ncol = ncol(sw_sub_fall))
  
  # pval
  gpp_par_pval_spring <- matrix(data = NA, nrow = nrow(sw_sub_spring), ncol = ncol(sw_sub_spring))
  gpp_par_pval_summer <- matrix(data = NA, nrow = nrow(sw_sub_summer), ncol = ncol(sw_sub_summer))
  gpp_par_pval_fall <- matrix(data = NA, nrow = nrow(sw_sub_fall), ncol = ncol(sw_sub_fall))
  
  # slope se
  gpp_par_se_spring <- matrix(data = NA, nrow = nrow(sw_sub_spring), ncol = ncol(sw_sub_spring))
  gpp_par_se_summer <- matrix(data = NA, nrow = nrow(sw_sub_summer), ncol = ncol(sw_sub_summer))
  gpp_par_se_fall <- matrix(data = NA, nrow = nrow(sw_sub_fall), ncol = ncol(sw_sub_fall))
  
  #####
  
  # this is still pretty slow so this would be worthwhile if we need to do these tests a lot
  # Parallelize this with a foreach()
  # not sure how to set this up easily with current indexing...
  # library(foreach)
  # library(doParallel)
  # library(parallel)
  # library(MASS)
  # 
  # registerDoParallel(7)  # use multicore, set to the 1 - number of cores
  # foreach (i = 1:nrow(gpp_sub_spring)) %do% {
  
  # should reshape these and use apply fns
  
  for (i in 1:nrow(gpp_sub_spring)){
    print(i)
    #print(paste(matfile, " row ", i, sep = ""))
    #print(paste(gppfile, " row ", i, sep = ""))
    for (j in 1:ncol(gpp_sub_spring)){
      for (k in 1:nyrs){
        yrs_inds <- which(yrs_sub_spring == unique(yrs_sub_spring)[k]) # which values are associated with a year
        
        # get sums per season per year, gpp
        # reprocessed versions had weird units, kg C m-2 mo-1 but seemed to assume all months had 30 days??
        # kg C m-2 s-1 * 86400 s/d * 1000 g/kg * nday per month = g C m-1 month-1, then summed per season
        # gpp_sub_spring_sum[i,j,k] <- sumna(gpp_sub_spring[i,j, yrs_inds]*86400*1000*c(31, 30, 31)) # sumna fn for all na handling
        # gpp_sub_summer_sum[i,j,k] <- sumna(gpp_sub_summer[i,j, yrs_inds]*86400*1000*c(30, 31, 31))
        # gpp_sub_fall_sum[i,j,k] <- sumna(gpp_sub_fall[i,j, yrs_inds]*86400*1000*c(30, 31, 30))
        
        # different values for reprocessed GPP
        # These original calculations were incorrect due to typos and wrong units
        # should have been multiplication by 1000 and need to rescale months
        # kg C m-2 mo-1 (30 day) * 1000 g/kg * nday per month - MODIFIED nday correction! just multiply by nday for this
        # g C m-2 d-1 for MODIS
        gpp_sub_spring_sum[i,j,k] <- sumna(gpp_sub_spring[i,j, yrs_inds]*c(31, 30, 31)) # sumna fn for all na handling
        gpp_sub_summer_sum[i,j,k] <- sumna(gpp_sub_summer[i,j, yrs_inds]*c(30, 31, 31))
        gpp_sub_fall_sum[i,j,k] <- sumna(gpp_sub_fall[i,j, yrs_inds]*c(30, 31, 30))
        
        # actually want the sum to be NA if all elements are not numbers
        
        # sumna in place for GPP, but NOT for precip:
        # If there's a GPP value missing, assume it would have been zero; if all are missing, gpp is NA
        # IF there's a precip value missing, unclear WHY it's missing and should not include grid point in analysis
        
        # get sums per year, precip in kg m-2 s-1 --> mm/month --> season
        pre_sub_spring_sum[i,j,k] <- sum(pre_sub_spring[i,j, yrs_inds]*86400*c(31, 30, 31))
        pre_sub_summer_sum[i,j,k] <- sum(pre_sub_summer[i,j, yrs_inds]*86400*c(30, 31, 31))
        pre_sub_fall_sum[i,j,k] <- sum(pre_sub_fall[i,j, yrs_inds]*86400*c(30, 31, 30))
        
        # get average for ta, K
        ta_sub_spring_mean[i,j,k] <- sum(ta_sub_spring[i,j, yrs_inds]*c(31, 30, 31))/sum(c(31, 30, 31))
        ta_sub_summer_mean[i,j,k] <- sum(ta_sub_summer[i,j, yrs_inds]*c(30, 31, 31))/sum(c(30, 31, 31))
        ta_sub_fall_mean[i,j,k] <- sum(ta_sub_fall[i,j, yrs_inds]*c(30, 31, 30))/sum(c(30, 31, 30))
        
        # get sum for PAR, MJ m-2
        # this is assuming midday PAR for 24 hours? NO, that's only for *VEGAS*, CABLE is OK like this
        par_sub_spring_sum[i,j,k] <- sum(sw_sub_spring[i,j, yrs_inds]*86400*10^-6*0.5*c(31, 30, 31))
        par_sub_summer_sum[i,j,k] <- sum(sw_sub_summer[i,j, yrs_inds]*86400*10^-6*0.5*c(30, 31, 31))
        par_sub_fall_sum[i,j,k] <- sum(sw_sub_fall[i,j, yrs_inds]*86400*10^-6*0.5*c(30, 31, 30))
        
      }
      
      # detrend GPP (after getting sums by looping over years)
      gpp_sub_spring_sum[i,j,] <- detrendGPP(gpp_sub_spring_sum[i,j,], unique(yrs_sub))
      gpp_sub_summer_sum[i,j,] <- detrendGPP(gpp_sub_summer_sum[i,j,], unique(yrs_sub))
      gpp_sub_fall_sum[i,j,] <- detrendGPP(gpp_sub_fall_sum[i,j,], unique(yrs_sub))
      
      # Compute slope, gpp ~ precip
      # Units:
      # GPP: g C m-2 season-1
      # Precip: mm season-1

      gpp_pre_slope_spring_info <- getSlopeIntInfo(gpp_sub_spring_sum[i,j,], pre_sub_spring_sum[i,j,])
      gpp_pre_slope_summer_info <- getSlopeIntInfo(gpp_sub_summer_sum[i,j,], pre_sub_summer_sum[i,j,])
      gpp_pre_slope_fall_info <- getSlopeIntInfo(gpp_sub_fall_sum[i,j,], pre_sub_fall_sum[i,j,])
      
      #slope
      gpp_pre_slope_spring[i,j] <- gpp_pre_slope_spring_info[1]
      gpp_pre_slope_summer[i,j] <- gpp_pre_slope_summer_info[1]
      gpp_pre_slope_fall[i,j] <- gpp_pre_slope_fall_info[1]
      
      #intercept
      gpp_pre_intercept_spring[i,j] <- gpp_pre_slope_spring_info[2]
      gpp_pre_intercept_summer[i,j] <- gpp_pre_slope_summer_info[2]
      gpp_pre_intercept_fall[i,j] <- gpp_pre_slope_fall_info[2]
      
      # r2
      gpp_pre_r2_spring[i,j] <- gpp_pre_slope_spring_info[3]
      gpp_pre_r2_summer[i,j] <- gpp_pre_slope_summer_info[3]
      gpp_pre_r2_fall[i,j] <- gpp_pre_slope_fall_info[3]
      
      # pval
      gpp_pre_pval_spring[i,j] <- gpp_pre_slope_spring_info[4]
      gpp_pre_pval_summer[i,j] <- gpp_pre_slope_summer_info[4]
      gpp_pre_pval_fall[i,j] <- gpp_pre_slope_fall_info[4]
      
      # slope se
      gpp_pre_se_spring[i,j] <- gpp_pre_slope_spring_info[5]
      gpp_pre_se_summer[i,j] <- gpp_pre_slope_summer_info[5]
      gpp_pre_se_fall[i,j] <- gpp_pre_slope_fall_info[5]
      
      
      # Ta: K (mean value)
      gpp_ta_slope_spring_info <- getSlopeIntInfo(gpp_sub_spring_sum[i,j,], ta_sub_spring_mean[i,j,])
      gpp_ta_slope_summer_info <- getSlopeIntInfo(gpp_sub_summer_sum[i,j,], ta_sub_summer_mean[i,j,])
      gpp_ta_slope_fall_info <- getSlopeIntInfo(gpp_sub_fall_sum[i,j,], ta_sub_fall_mean[i,j,])
      
      #slope
      gpp_ta_slope_spring[i,j] <- gpp_ta_slope_spring_info[1]
      gpp_ta_slope_summer[i,j] <- gpp_ta_slope_summer_info[1]
      gpp_ta_slope_fall[i,j] <- gpp_ta_slope_fall_info[1]
      
      #intercept
      gpp_ta_intercept_spring[i,j] <- gpp_ta_slope_spring_info[2]
      gpp_ta_intercept_summer[i,j] <- gpp_ta_slope_summer_info[2]
      gpp_ta_intercept_fall[i,j] <- gpp_ta_slope_fall_info[2]
      
      # r2
      gpp_ta_r2_spring[i,j] <- gpp_ta_slope_spring_info[3]
      gpp_ta_r2_summer[i,j] <- gpp_ta_slope_summer_info[3]
      gpp_ta_r2_fall[i,j] <- gpp_ta_slope_fall_info[3]
      
      # pval
      gpp_ta_pval_spring[i,j] <- gpp_ta_slope_spring_info[4]
      gpp_ta_pval_summer[i,j] <- gpp_ta_slope_summer_info[4]
      gpp_ta_pval_fall[i,j] <- gpp_ta_slope_fall_info[4]
      
      # slope se
      gpp_ta_se_spring[i,j] <- gpp_ta_slope_spring_info[5]
      gpp_ta_se_summer[i,j] <- gpp_ta_slope_summer_info[5]
      gpp_ta_se_fall[i,j] <- gpp_ta_slope_fall_info[5]
      
      
      # PAR: MJ m-2 season-1
      gpp_par_slope_spring_info <- getSlopeIntInfo(gpp_sub_spring_sum[i,j,], par_sub_spring_sum[i,j,])
      gpp_par_slope_summer_info <- getSlopeIntInfo(gpp_sub_summer_sum[i,j,], par_sub_summer_sum[i,j,])
      gpp_par_slope_fall_info <- getSlopeIntInfo(gpp_sub_fall_sum[i,j,], par_sub_fall_sum[i,j,])
      
      #slope
      gpp_par_slope_spring[i,j] <- gpp_par_slope_spring_info[1]
      gpp_par_slope_summer[i,j] <- gpp_par_slope_summer_info[1]
      gpp_par_slope_fall[i,j] <- gpp_par_slope_fall_info[1]
      
      #intercept
      gpp_par_intercept_spring[i,j] <- gpp_par_slope_spring_info[2]
      gpp_par_intercept_summer[i,j] <- gpp_par_slope_summer_info[2]
      gpp_par_intercept_fall[i,j] <- gpp_par_slope_fall_info[2]
      
      # r2
      gpp_par_r2_spring[i,j] <- gpp_par_slope_spring_info[3]
      gpp_par_r2_summer[i,j] <- gpp_par_slope_summer_info[3]
      gpp_par_r2_fall[i,j] <- gpp_par_slope_fall_info[3]
      
      # pval
      gpp_par_pval_spring[i,j] <- gpp_par_slope_spring_info[4]
      gpp_par_pval_summer[i,j] <- gpp_par_slope_summer_info[4]
      gpp_par_pval_fall[i,j] <- gpp_par_slope_fall_info[4]
      
      # slope se
      gpp_par_se_spring[i,j] <- gpp_par_slope_spring_info[5]
      gpp_par_se_summer[i,j] <- gpp_par_slope_summer_info[5]
      gpp_par_se_fall[i,j] <- gpp_par_slope_fall_info[5]
    }
  }
  
  
  # end multicore processing
  #stopImplicitCluster()
  
  # Arrange into data frames and write out
  # output directory
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_allmodel_slope_error_1992_2016_detrend/")
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/outputs")
  #prefix <- unlist(strsplit(matfile, "[.]"))[1] # output file name prefix
  #prefix <- unlist(strsplit(gppfile, "[.]"))[1] # output file name prefix
  prefix <- paste(rs_source, yr_range, sep = "_") # prefix is the rs_source name + yr_range
  
  #library(tidyverse)
  # write_csv is a readr package function
  
  
  # SPRING
  lonlat_sub <- as.matrix(expand.grid(lon,lat_sub))
  
  # check aridity_df_sub is OK to use (lines up with existing lon lat export)
  #all(aridity_df_sub$Lon == lonlat_sub[,1])
  #all(aridity_df_sub$Lat == lonlat_sub[,2])
  
  df_spring <- cbind.data.frame(lonlat_sub, aridity_df_sub$Aridity_P_PET, c(umdlc_mat_sub), c(modis_pft_sub),
                                c(rowMeans(gpp_sub_spring_sum, dims = 2, na.rm = T)), # get mean value per grid cell
                                c(rowMeans(pre_sub_spring_sum, dims = 2, na.rm = T)), 
                                c(rowMeans(ta_sub_spring_mean, dims = 2, na.rm = T)),
                                c(rowMeans(par_sub_spring_sum, dims = 2, na.rm = T)),
                                c(gpp_pre_slope_spring), c(gpp_pre_intercept_spring), c(gpp_pre_r2_spring), c(gpp_pre_pval_spring), c(gpp_pre_se_spring), 
                                c(gpp_ta_slope_spring), c(gpp_ta_intercept_spring), c(gpp_ta_r2_spring), c(gpp_ta_pval_spring), c(gpp_ta_se_spring),
                                c(gpp_par_slope_spring), c(gpp_par_intercept_spring), c(gpp_par_r2_spring), c(gpp_par_pval_spring), c(gpp_par_se_spring))
  colnames(df_spring) <- c("Lon", "Lat", "Aridity", "UMD_LC", "MODIS_LC", "GPP_gCm2", "Precip_mm", "Ta_K", "PAR_MJm2", 
                           "Precip_Slope", "Precip_Intercept", "Precip_R2", "Precip_pval", "Precip_Slope_SE",
                           "Ta_Slope", "Ta_Intercept", "Ta_R2", "Ta_pval", "Ta_Slope_SE",
                           "PAR_Slope", "PAR_Intercept", "PAR_R2", "PAR_pval", "PAR_Slope_SE")
  df_spring$GPP_gCm2[is.nan(df_spring$GPP_gCm2)] <- NA
  df_spring$Precip_mm[is.nan(df_spring$Precip_mm)] <- NA
  df_spring$Ta_K[is.nan(df_spring$Ta_K)] <- NA
  df_spring$PAR_MJm2[is.nan(df_spring$PAR_MJm2)] <- NA
  #df_spring_sub <- na.omit(df_spring)
  
  # write out both to compare sizes, will need to repeat or also append summer and fall
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_outputs/test2/")
  
  write_csv(df_spring, paste(prefix, "gppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_detrend.csv", sep = ""))
  

  # SUMMER
  #lonlat_sub <- as.matrix(expand.grid(lon,lat_sub))
  
  df_summer <- cbind.data.frame(lonlat_sub, aridity_df_sub$Aridity_P_PET, c(umdlc_mat_sub), c(modis_pft_sub),
                                c(rowMeans(gpp_sub_summer_sum, dims = 2, na.rm = T)), # get mean value per grid cell
                                c(rowMeans(pre_sub_summer_sum, dims = 2, na.rm = T)), 
                                c(rowMeans(ta_sub_summer_mean, dims = 2, na.rm = T)),
                                c(rowMeans(par_sub_summer_sum, dims = 2, na.rm = T)),
                                c(gpp_pre_slope_summer), c(gpp_pre_intercept_summer), c(gpp_pre_r2_summer), c(gpp_pre_pval_summer), c(gpp_pre_se_summer),
                                c(gpp_ta_slope_summer), c(gpp_ta_intercept_summer), c(gpp_ta_r2_summer), c(gpp_ta_pval_summer), c(gpp_ta_se_summer),
                                c(gpp_par_slope_summer), c(gpp_par_intercept_summer), c(gpp_par_r2_summer), c(gpp_par_pval_summer), c(gpp_par_se_summer))
  colnames(df_summer) <- c("Lon", "Lat", "Aridity", "UMD_LC", "MODIS_LC", "GPP_gCm2", "Precip_mm", "Ta_K", "PAR_MJm2", 
                           "Precip_Slope", "Precip_Intercept", "Precip_R2", "Precip_pval", "Precip_Slope_SE",
                           "Ta_Slope", "Ta_Intercept", "Ta_R2", "Ta_pval", "Ta_Slope_SE",
                           "PAR_Slope", "PAR_Intercept", "PAR_R2", "PAR_pval", "PAR_Slope_SE")
  df_summer$GPP_gCm2[is.nan(df_summer$GPP_gCm2)] <- NA
  df_summer$Precip_mm[is.nan(df_summer$Precip_mm)] <- NA
  df_summer$Ta_K[is.nan(df_summer$Ta_K)] <- NA
  df_summer$PAR_MJm2[is.nan(df_summer$PAR_MJm2)] <- NA
  #df_summer_sub <- na.omit(df_summer)
  
  # write out both to compare sizes, will need to repeat or also append summer and fall
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_outputs/test2/")
  
  write_csv(df_summer, paste(prefix, "gppproc_met_CABLE_S2_precip_ta_par_df_summer_slope_se_detrend.csv", sep = ""))


  # FALL
  #lonlat_sub <- as.matrix(expand.grid(lon,lat_sub))
  
  df_fall <- cbind.data.frame(lonlat_sub, aridity_df_sub$Aridity_P_PET, c(umdlc_mat_sub), c(modis_pft_sub),
                                c(rowMeans(gpp_sub_fall_sum, dims = 2, na.rm = T)), # get mean value per grid cell
                                c(rowMeans(pre_sub_fall_sum, dims = 2, na.rm = T)), 
                                c(rowMeans(ta_sub_fall_mean, dims = 2, na.rm = T)),
                                c(rowMeans(par_sub_fall_sum, dims = 2, na.rm = T)),
                                c(gpp_pre_slope_fall), c(gpp_pre_intercept_fall), c(gpp_pre_r2_fall), c(gpp_pre_pval_fall), c(gpp_pre_se_fall),
                                c(gpp_ta_slope_fall), c(gpp_ta_intercept_fall), c(gpp_ta_r2_fall), c(gpp_ta_pval_fall), c(gpp_ta_se_fall),
                                c(gpp_par_slope_fall), c(gpp_par_intercept_fall), c(gpp_par_r2_fall), c(gpp_par_pval_fall), c(gpp_par_se_fall))
  colnames(df_fall) <- c("Lon", "Lat", "Aridity", "UMD_LC", "MODIS_LC", "GPP_gCm2", "Precip_mm", "Ta_K", "PAR_MJm2", 
                           "Precip_Slope", "Precip_Intercept", "Precip_R2", "Precip_pval", "Precip_Slope_SE",
                           "Ta_Slope", "Ta_Intercept", "Ta_R2", "Ta_pval", "Ta_Slope_SE",
                           "PAR_Slope", "PAR_Intercept", "PAR_R2", "PAR_pval", "PAR_Slope_SE")
  df_fall$GPP_gCm2[is.nan(df_fall$GPP_gCm2)] <- NA
  df_fall$Precip_mm[is.nan(df_fall$Precip_mm)] <- NA
  df_fall$Ta_K[is.nan(df_fall$Ta_K)] <- NA
  df_fall$PAR_MJm2[is.nan(df_fall$PAR_MJm2)] <- NA
  #df_fall_sub <- na.omit(df_fall)
  
  # write out both to compare sizes, will need to repeat or also append summer and fall
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_outputs/test2/")
  
  write_csv(df_fall, paste(prefix, "gppproc_met_CABLE_S2_precip_ta_par_df_fall_slope_se_detrend.csv", sep = ""))
}
#registerDoSEQ()