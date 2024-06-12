
# output like these for each model
# c("SITE_ID", "Lat", "Lon", "Year", "Month", "Model_Name", "GPP", "CABLE_Pr", "CABLE_Tas", "CABLE_Rsds")
# or something similar

# modified from this script for lat lon extraction for each site for each model:
# plot_trendy_gpp_allproc_met_CABLE_precip_ta_par_slopes_fluxsite_combined_source_locations_getslopes_error.R

# Load locations for each flux site
# Get lat lon info for each of the flux towers and find the nearest available grid cell from the TRENDY data
library(tidyverse)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
library(sf) # for distance calculation
library(rhdf5)
library(ncdf4)
library(foreach)
library(doParallel)

# read in list of flux sites
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/plots/flux_site_summary_energy_swc1_sensitivity/new_aridity")
#ec_gpp_sens <- read.csv("ec_gpp_sens_P_F_sum.csv") # would need to swap this for temperature and PAR

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
ec_gpp_sens <- read.csv("combined_source_ec_univariate_slopes2.csv") # this includes everything
ec_gpp_sens <- subset(ec_gpp_sens, Met_Var == "P_F_sum")

# New site list from combined source
flx_site_info <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
colnames(flx_site_info)[1] <- "SITE_ID" # need to make sure first column is labeled correctly
flx_site_info <- flx_site_info[order(flx_site_info$SITE_ID),] # alphabetical reorder

# Load in trendy data
# need to load and merge southern and northern hem
# will need to loop through different models
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4") # this is changed
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_allmodel_slope_error_1992_2016_detrend")
file_list <- list.files()

j <- 1 # iterator

# Set up parallel computing
#cl <- makeCluster(detectCores(logical = TRUE))
#registerDoParallel(cl)

registerDoParallel(detectCores() - 2)
foreach(i=1:length(file_list), .packages = ) %dopar% {
#for (i in 1:length(file_list)){
  #getTrendy <- function(file_num, ec_gpp_sens, flx_site_info, file_list, log.formula, my.formula){
  #foreach (file_num = 1:length(file_list)) %do% {
  # test
  #trendyfile <- file_list[file_num]
  
  #trendyfile <- "CABLE_S2_gppgppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_1992_2016.csv"
  #print(trendyfile)
  #trendy_slopes <- read.csv("CLM_S2_gppgppproc_met_CABLE_S2_precip_ta_par_df_spring_slope.csv")
  trendyfile <- file_list[i]
  trendy_slopes <- read.csv(trendyfile)
  print(i)
  
  ###
  # Load in flux site data
  # get sites for particular season
  
  # get season
  splitname <- unlist(str_split(trendyfile, "_"))
  season_name <- splitname[length(splitname)-5] # this needed to be modified to -5 from -1 to extract the season correctly with the 1992_2016_detrend suffix now
  
  # get site list based on season
  site_list <- ec_gpp_sens$SITE_ID[which(tolower(ec_gpp_sens$Season) == season_name)]
  
  # subset list
  flx_site_info_sub <- flx_site_info[which(flx_site_info$SITE_ID %in% site_list),]
  
  # backwards subset ec_gpp_sens to get climate info
  ec_gpp_sens_sub <- ec_gpp_sens[which(ec_gpp_sens$SITE_ID %in% flx_site_info_sub$SITE_ID &
                                         tolower(ec_gpp_sens$Season) == season_name),]
  
  flx_site_info_sub$Distance <- NA # retain distance column
  
  ###
  # set up output file
  flx_site_output <- cbind.data.frame(flx_site_info_sub, data.frame(matrix(NA, nrow = nrow(flx_site_info_sub), ncol = ncol(trendy_slopes))))
  colnames(flx_site_output) <- c(colnames(flx_site_info_sub), colnames(trendy_slopes))
  
  col_start <- ncol(flx_site_info_sub) + 1
  col_end <- ncol(flx_site_output)
  
  # only do complete slopes, no missing values! excludes NA
  trendy_slopes <- trendy_slopes[complete.cases(trendy_slopes),]
  trendy_slopes_sf <- st_as_sf(trendy_slopes, coords = c("Lon", "Lat"),
                               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  #st_is_longlat(trendy_slopes_sf)
  # turn flux site data frame into a sf format points
  flx_site_info_sub_sf <- st_as_sf(flx_site_info_sub, coords = c("LOCATION_LONG", "LOCATION_LAT"),
                                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  col_start <- ncol(flx_site_info_sub) + 1
  col_end <- ncol(flx_site_output)
  
  # calculate distances for each point, grid cell with minimum distance to each flx site
  # slow because it checks all grid cells, but not too bad since we only have to do it once
  for (i in 1:nrow(flx_site_info_sub)){
    #print(i)
    dist <- st_distance(flx_site_info_sub_sf[i,], trendy_slopes_sf)
    flx_site_output[i, col_start - 1] <- min(dist)
    flx_site_output[i, col_start:col_end] <- trendy_slopes[which.min(dist),]
  }
  
  # flx_site_output, season and modelname
  flx_site_output$Season <- season_name
  flx_site_output$ModelName <- splitname[1]
  
  # need to do some string splitting for naming
  
  # write out data frame to csv
  # actually includes gpp sensitivity to precip, ta, and par
  #write.csv(flx_site_output, paste("../results/v4_ec_site_locations_combined_source_error_1992_2016/", splitname[1], "_gpp_allvar_ec_sites_combined_source_data_error_1992_2016_", season_name, ".csv", sep = ""), row.names = F)
  write.csv(flx_site_output, paste("../results/v4_ec_site_locations_combined_source_error_1992_2016_detrend/", splitname[1], "_gpp_allvar_ec_sites_combined_source_data_error_1992_2016_", season_name, ".csv", sep = ""), row.names = F)
  # # output all together
  # if (j == 1){
  #   # if first time, label as output file
  #   flx_site_output_all <- flx_site_output
  # } else {
  #   # if after first time, append by rows
  #   flx_site_output_all <- rbind.data.frame(flx_site_output_all, flx_site_output)
  # }
  # 
  # j <- j + 1 # iterator
}

# This works. Now need to get these to iteratively access the spatial data for the selected lat lons from here and extract monthly data for CABLE met vars and model GPPs 

# Stop the cluster
#stopCluster(cl)
registerDoSEQ()

# write out ALL data frame to csv, only need to do the first time
# this is the key output file we're looking for
setwd("../results/v4_ec_site_locations_combined_source_error_1992_2016_detrend/")
output_file_list <- list.files()
if (length(output_file_list == 45)){ # check that this is 45 things, 15 models * 3 seasons
  for (j in 1: length(output_file_list)){
    #print(j)
    flx_site_output <- read.csv(output_file_list[j])
    if (j == 1){
      # if first time, label as output file
      flx_site_output_all <- flx_site_output
    } else {
      # if after first time, append by rows
      flx_site_output_all <- rbind.data.frame(flx_site_output_all, flx_site_output)
    }
  }
}
#write.csv(flx_site_output_all, paste("../results/v4_ec_site_locations_combined_source_error/allmodels_gpp_allvar_ec_sites_combined_source_data_error_1992_2016.csv", sep = ""), row.names = F)
write.csv(flx_site_output_all, "allmodels_gpp_allvar_ec_sites_combined_source_data_error_1992_2016_detrend.csv", row.names = F)

#####
# Check locations of outputted data
# re-order the output
flx_site_locations <- subset(flx_site_output_all, select = c("SITE_ID", "SOURCE", "LOCATION_LAT", "LOCATION_LONG", "Distance", "Lon", "Lat", "Season", "ModelName")) 
flx_site_locations <- flx_site_locations[order(flx_site_locations$SITE_ID),] # reorder to visualize consistency of locations in table
write.csv(flx_site_locations, "allmodels_gpp_allvar_ec_sites_combined_source_data_error_1992_2016_detrend_locations_ordered.csv", row.names = F)

flx_site_locations2 <- subset(flx_site_locations, select = c("LOCATION_LAT", "LOCATION_LONG", "Distance", "Lon", "Lat"))
flx_site_locations_sd_check <- aggregate(flx_site_locations2, by = list(flx_site_locations$SITE_ID, flx_site_locations$ModelName), FUN = sd)
flx_site_locations_sd_check_gt0 <- subset(flx_site_locations_sd_check, flx_site_locations_sd_check$Distance > 0)
# IL-Yat is the only site with inconsistent locations across seasons...
# All other models are pulling from the same (within model) GPP grid cells

flx_site_locations_IL_Yat <- subset(flx_site_locations, subset = SITE_ID == "IL-Yat")

#####
# For each model / season, pull relevant information from given grid cells for each month/year, 1992-2016
# ModelName, Model_GPP, Model_GPP_CABLE_loc, CABLE_precip, CABLE_tas, CABLE_rsds
# So each season row in flx_site_locations expands to (2016-1992+1)* 3  = 75 rows
# And also pull Model GPP for location that CABLE GPP model is referenced to... there's a reason if it's likely missing
# Set it up as a foreach loading each model/season output information, subset to flx_site_locations limited format, then expanding and outputting each

# "SITE_ID", "SOURCE", "LOCATION_LAT", "LOCATION_LONG", "Distance", "Lon", "Lat", "Season", "ModelName", 
# "Year", "Month", "Model_GPP", "Model_GPP_CABLE_loc", "CABLE_precip", "CABLE_tas", "CABLE_rsds"

flx_site_locations <- read.csv("allmodels_gpp_allvar_ec_sites_combined_source_data_error_1992_2016_detrend_locations_ordered.csv")


# try a setup for a model that's not CABLE and do the extraction routine

model_list <- unique(flx_site_locations$ModelName)
season_list <- unique(flx_site_locations$Season)
# Seasons won't matter for nearly all sites, but IL-Yat is weird and so we need to do it this way...

yrs <- rep(1901:2016, each = 12) # get list of all years, 12 elements each
mos <- rep(1:12, 116) # repeat 1:12 116 times, for 1901 through 2016
band_inds <- which(yrs %in% 1991:2016) # will be same for gpp and precip
mos_sub <- mos[band_inds] # subsetted version for months, 1991-2016
yrs_sub <- yrs[band_inds] # subsetted version for years, 1991-2016

# set up index vectors to reference for the indexing within the arrays
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/") # ssd internal path
ncin <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc") # netcdf file
# get lat and lon - need these for reference coordinates
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
nc_close(ncin) # close connection to netcdf file
rm(ncin)

# Load CABLE meteorological data

# Just do CABLE_S2
#gpp_filelist <- c("CABLE_S2_gpp.mat")
#setwd("D:/berkeley/TRENDY/TRENDYv6_originals_sub/gpp")
#gpp_filelist <- c("CABLE_S2_gpp.nc")

# READ IN CABLE PRECIP / TEMP / SW
# this was in the loop originally, but can be outside and just done once.
# using CABLE_S2 for all of these! It's in the correct gridding already

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/TRENDY_S2/CABLE_met/")
precip <- nc_open("CABLE_S2_pr.nc")
# 1573 is January 1991, will want to change this to not be hard coded for different models?
pre_array <- ncvar_get(precip, "pr", start = c(1, 1, 1573))
# 720 rows, 360 columns, 312 layers
# need to flip
pre_array <- pre_array[, rev(seq_len(ncol(pre_array))),] # flip columns
nc_close(precip) # close connection to netcdf file
rm(precip)

# AIRTEMP, K
ta <- nc_open("CABLE_S2_tas.nc")
ta_array <- ncvar_get(ta, "tas", start = c(1, 1, 1573))
ta_array <- ta_array[, rev(seq_len(ncol(ta_array))),] # flip columns
nc_close(ta)
rm(ta)

# SW, W m-2
sw <- nc_open("CABLE_S2_rsds.nc")
sw_array <- ncvar_get(sw, "rsds", start = c(1, 1, 1573))
sw_array <- sw_array[, rev(seq_len(ncol(sw_array))),] # flip columns
nc_close(sw)
rm(sw)
gc()

# repeat to make sure we have all combinations of models, seasons
model_list_rep <- rep(model_list, 3)
season_list_rep <- rep(season_list, each = length(model_list))

# do foreach for all models, seasons, and combine together with rbind
registerDoParallel(detectCores() - 2)
flx_site_output_monthly_info_all <- foreach(model_ind = 1:length(model_list_rep), .combine = rbind) %dopar% {
#for (model_ind in 1:length(model_list_rep)){ # would be better to run as a foreach if possible...
  print(model_ind)
  #foreach(i=1:length(file_list), .packages = ) %dopar% {
  
  #model_ind <- 2
  model_name <- model_list_rep[model_ind]
  
  #season_ind <- 2
  season_name <- season_list_rep[model_ind]
  
  # GPP loading, for each model
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/TRENDY_S2/GPP")
  # list all *.mat gpp files
  #gpp_filelist <- list.files(pattern = glob2rx(paste("*S2*gpp*.mat", sep = "")))
  #gppfile <- "CLASS-CTEM_S2_gpp.mat"
  gppfile <- list.files(pattern = glob2rx(paste(model_name, "*S2*gpp*.mat", sep = "")))[1] # use 1 to distinguish LPJ from LPJ-GUESS and ORCHIDEE from OCHIDEE-MICT
  gpp <- h5read(gppfile, name = "gpp_05")
  gpp <- aperm(gpp, c(2,1,3)) # swap rows and columns
  gpp <- gpp[, rev(seq_len(ncol(gpp))),] # flip columns
  gpp_array <- gpp[,, band_inds]
  rm(gpp)
  gc() # clean up RAM
  
  # dims for gpp_array, pre_array, sw_array, and ta_array should all be 720, 360, 312
  
  # Subset flx_site_locations for each model and season
  flx_site_locations_model_season <- subset(flx_site_locations, subset = Season == season_name & ModelName == model_name)
  flx_site_locations_model_season_CABLE <- subset(flx_site_locations, subset = Season == season_name & ModelName == "CABLE") # get info for CABLE location
  # Loop over all sites
  for (i in 1:nrow(flx_site_locations_model_season)){
    
    #i <- 1
    # get indices for season, year
    if (season_name == "spring"){
      time_inds <- which(mos_sub %in% 3:5)
    } else if (season_name == "summer"){
      time_inds <- which(mos_sub %in% 6:8)
    } else if (season_name == "fall"){
      time_inds <- which(mos_sub %in% 9:11)
    }
    
    flx_site_locations_model_season_sub <- flx_site_locations_model_season[i,]
    flx_site_locations_model_season_CABLE_sub <- flx_site_locations_model_season_CABLE[i,]
    new_cols <- c("Year", "Month", "Model_GPP", "CABLE_precip", "CABLE_tas", "CABLE_rsds", "Lon_CABLE_loc", "Lat_CABLE_loc", "Model_GPP_CABLE_loc", "CABLE_loc_precip", "CABLE_loc_tas", "CABLE_loc_rsds")
    new_cols_df <- data.frame(matrix(NA, nrow = length(time_inds), ncol = length(new_cols)))
    colnames(new_cols_df) <- new_cols
    flx_site_locations_model_season_sub <- cbind.data.frame(flx_site_locations_model_season_sub, new_cols_df) # gives a missing row names warning but that's no big deal
    flx_site_locations_model_season_sub$Year <- yrs_sub[time_inds]
    flx_site_locations_model_season_sub$Month <- mos_sub[time_inds]
    
    flx_site_locations_model_season_sub$Lon_CABLE_loc <- flx_site_locations_model_season_CABLE_sub$Lon
    flx_site_locations_model_season_sub$Lat_CABLE_loc <- flx_site_locations_model_season_CABLE_sub$Lat
    
    row_lon_ind <- which(lon == flx_site_locations_model_season_sub$Lon[1]) # indexes based on the first row
    col_lat_ind <- which(lat == flx_site_locations_model_season_sub$Lat[1])
    
    row_lon_ind_cable <- which(lon == flx_site_locations_model_season_CABLE_sub$Lon[1]) # indexes based on the first row
    col_lat_ind_cable <- which(lat == flx_site_locations_model_season_CABLE_sub$Lat[1])
    
    for (j in 1:length(time_inds)){ # this is fast since everything is already loaded and it's just index referencing
      # index by: row (longitude of grid cell), col (latitude of grid cell), time (at the indexed reference)
      flx_site_locations_model_season_sub$Model_GPP[j] <- gpp_array[row_lon_ind, col_lat_ind, time_inds[j]] # gpp
      flx_site_locations_model_season_sub$CABLE_precip[j] <- pre_array[row_lon_ind, col_lat_ind, time_inds[j]] # cable precip # why is this coming back all NA...
      flx_site_locations_model_season_sub$CABLE_tas[j] <- ta_array[row_lon_ind, col_lat_ind, time_inds[j]] # cable air temp
      flx_site_locations_model_season_sub$CABLE_rsds[j] <- sw_array[row_lon_ind, col_lat_ind, time_inds[j]] # cable shortwave
      
      # cable location indexing
      flx_site_locations_model_season_sub$Model_GPP_CABLE_loc[j] <- gpp_array[row_lon_ind_cable, col_lat_ind_cable, time_inds[j]] # gpp
      flx_site_locations_model_season_sub$CABLE_loc_precip[j] <- pre_array[row_lon_ind_cable, col_lat_ind_cable, time_inds[j]] # cable precip # why is this coming back all NA...
      flx_site_locations_model_season_sub$CABLE_loc_tas[j] <- ta_array[row_lon_ind_cable, col_lat_ind_cable, time_inds[j]] # cable air temp
      flx_site_locations_model_season_sub$CABLE_loc_rsds[j] <- sw_array[row_lon_ind_cable, col_lat_ind_cable, time_inds[j]] # cable shortwave
    }
    # repeat appending as necessary to append out
    if (i == 1){
      # if first time, label as output file
      flx_site_output_monthly_info <- flx_site_locations_model_season_sub
    } else {
      # if after first time, append by rows
      flx_site_output_monthly_info <- rbind.data.frame(flx_site_output_monthly_info, flx_site_locations_model_season_sub)
    }
  }
  return(flx_site_output_monthly_info)
  
  # use this to append if using regular for loop
  # if (model_ind == 1){
  #   # if first time, label as output file
  #   flx_site_output_monthly_info_all <- flx_site_output_monthly_info
  # } else {
  #   # if after first time, append by rows
  #   flx_site_output_monthly_info_all <- rbind.data.frame(flx_site_output_monthly_info_all, flx_site_output_monthly_info)
  # }
}

registerDoSEQ()

setwd("../../results/v4_ec_site_locations_combined_source_error_1992_2016_detrend/")
write.csv(flx_site_output_monthly_info_all, "all_model_data_for_ec_sites_monthly_1991_2016_cable_loc_info.csv", row.names = F) # need to re-do if I want to add what the GPP would be for each model at the location that CABLE identifies
# did it from 1991 just in case

# Aggregate up to seasons
# sum function, but return NA only if all elements are NA
# https://stackoverflow.com/questions/56473098/efficient-way-to-calculate-sum-or-return-na-if-all-values-are-na
# slower than I'd want, but here we are
sumna <- function(x) {
  if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
}

# GPP from TRENDY mat files is: kg C m-2 mo-1 (30 day), so need to: * 1000 g/kg * nday per month correction, eg for spring: sumna(gpp_sub_spring[i,j, yrs_inds]*1000*c(31/30, 30/30, 31/30))
# Precip from CABLE: precip in kg m-2 s-1 --> mm/month --> season, sum(pre_sub_spring[i,j, yrs_inds]*86400*c(31, 30, 31))
# Ta from CABLE: average for ta, K, sum(ta_sub_spring[i,j, yrs_inds]*c(31, 30, 31))/sum(c(31, 30, 31))
# SW from CABLE: sum for PAR, MJ m-2, sum(sw_sub_spring[i,j, yrs_inds]*86400*10^-6*0.5*c(31, 30, 31))

# unique things: SITE_ID, Season, ModelName, Year
site_id_list<- unique(flx_site_output_monthly_info_all$SITE_ID)
season_list <- unique(flx_site_output_monthly_info_all$Season)
model_list <- unique(flx_site_output_monthly_info_all$ModelName)
year_list <- unique(flx_site_output_monthly_info_all$Year)

season_agg_vars <- expand.grid(site_id_list, season_list, model_list, year_list) # generates data frame with all combinations of these variables
colnames(season_agg_vars) <- c("SITE_ID", "Season", "ModelName", "Year")

flx_sub_season_output_info_all <- data.frame(matrix(NA, nrow = nrow(season_agg_vars), ncol = ncol(flx_site_output_monthly_info_all)))
colnames(flx_sub_season_output_info_all) <- colnames(flx_site_output_monthly_info_all)

registerDoParallel(detectCores() - 2)
#flx_site_output_season_info_all <- foreach(i = 1:nrow(season_agg_vars), .combine = rbind) %dopar% {
# takes about ten minutes, breaks up the loop for each model. Each takes about 4-5 minutes, then use 8 cores running over 15 models
flx_site_output_season_info_all <- foreach(model_ind = 1:length(model_list), .combine = rbind) %dopar% {
  modelname <- model_list[model_ind]
  season_agg_vars_sub <- subset(season_agg_vars, ModelName == modelname)
  flx_sub_season_output_info_all_sub <- data.frame(matrix(NA, nrow = nrow(season_agg_vars_sub), ncol = ncol(flx_site_output_monthly_info_all)))
  colnames(flx_sub_season_output_info_all_sub) <- colnames(flx_site_output_monthly_info_all)
  for(i in 1:nrow(season_agg_vars_sub)){
    print(i)
    flx_sub_monthly <- subset(flx_site_output_monthly_info_all, subset = SITE_ID == season_agg_vars_sub$SITE_ID[i] & Season == season_agg_vars_sub$Season[i] & ModelName == season_agg_vars_sub$ModelName[i] & Year == season_agg_vars_sub$Year[i])
    flx_sub_season <- flx_sub_monthly[1,] # get first row to fill
    
    flx_sub_season[1,which(colnames(flx_sub_season) %in% c("Month", "Model_GPP", "CABLE_precip", "CABLE_tas", "CABLE_rsds", "Model_GPP_CABLE_loc", "CABLE_loc_precip", "CABLE_loc_tas", "CABLE_loc_rsds"))] <- NA
    
    # next need to do the math per season as described above to get all of these columns aggregated properly...
    
    # Set mdays in three months per season, this is season and order agnostic, just looks at months
    for (m in 1:nrow(flx_sub_monthly)){
      if (flx_sub_monthly$Month[m] %in% c(4, 6, 9, 11)){mday <- 30}
      else if (flx_sub_monthly$Month[m] %in% c(3, 5, 7, 8, 10)){mday <- 31}
      
      if (m == 1){mday_list <- mday}
      else {mday_list <- c(mday_list, mday)}
    }
    
    # GPP - Need to use sumna fn here to avoid gaps, note that sumna takes MUCH longer
    #flx_sub_season$Model_GPP <- sum(flx_sub_monthly$Model_GPP*1000*(mday_list/30))
    #flx_sub_season$Model_GPP_CABLE_loc <- sum(flx_sub_monthly$Model_GPP_CABLE_loc*1000*(mday_list/30))
    flx_sub_season$Model_GPP <- sumna(flx_sub_monthly$Model_GPP*1000*(mday_list/30))
    flx_sub_season$Model_GPP_CABLE_loc <- sumna(flx_sub_monthly$Model_GPP_CABLE_loc*1000*(mday_list/30))
    
    # Precip
    flx_sub_season$CABLE_precip <- sum(flx_sub_monthly$CABLE_precip*86400*mday_list)
    flx_sub_season$CABLE_loc_precip <- sum(flx_sub_monthly$CABLE_loc_precip*86400*mday_list)
    
    # Ta
    flx_sub_season$CABLE_tas <- sum(flx_sub_monthly$CABLE_tas*mday_list)/sum(mday_list)
    flx_sub_season$CABLE_loc_tas <- sum(flx_sub_monthly$CABLE_loc_tas*mday_list)/sum(mday_list)
    
    # PAR
    flx_sub_season$CABLE_rsds <- sum(flx_sub_monthly$CABLE_rsds*86400*10^-6*0.5*mday_list)
    flx_sub_season$CABLE_loc_rsds <- sum(flx_sub_monthly$CABLE_loc_rsds*86400*10^-6*0.5*mday_list)
    
    #return(flx_sub_season)
    flx_sub_season_output_info_all_sub[i,] <- flx_sub_season
  }
  return(flx_sub_season_output_info_all_sub)
}
registerDoSEQ()
# will need to rename the rsds columns to PAR at the end
colnames(flx_site_output_season_info_all)[colnames(flx_site_output_season_info_all) == "CABLE_rsds"] <- "CABLE_PAR"
colnames(flx_site_output_season_info_all)[colnames(flx_site_output_season_info_all) == "CABLE_loc_rsds"] <- "CABLE_loc_PAR"
#write.csv(flx_site_output_season_info_all, "all_model_data_for_ec_sites_season_1991_2016_cable_loc_info.csv", row.names = F) 
write.csv(flx_site_output_season_info_all, "all_model_data_for_ec_sites_season_1991_2016_cable_loc_info_sumna.csv", row.names = F) 
