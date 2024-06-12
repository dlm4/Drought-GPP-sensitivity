


# extract data from TerraClimate PDSI for individual sites
# average for seasons, like in previous analyses
# get slopes for GPP ~ PDSI

library(tidyverse)
library(ncdf4)

# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/pdsi")
# 
# nc <- nc_open("TerraClimate_PDSI_1992.nc")
# pdsi <- ncvar_get(nc, 'PDSI')
# lat <- ncvar_get(nc, 'lat')
# lon <- ncvar_get(nc, 'lon')
# nc_close(nc)
# rm(nc)

#####

# load in flux site list with lat lon information
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

# Will need to later identify which years to keep and which years to remove

# go to where the climatology summaries are for terraclimate data
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/pdsi")
tc_files <- list.files(pattern = glob2rx("*.nc")) # list out all the terraclimate variables ending in .nc

# do the matching for lat lon first, and then just retain these values and can apply to extract and cbind

nc <- nc_open(tc_files[1]) # just do this on the first file
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

x <- cbind(flx_site_info$LOCATION_LONG, flx_site_info$LOCATION_LAT) # x is lon, lat location of flux site

getLocIndex <- function(lon, lat, x){
  flat = match(abs(lat - x[2]) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/48, 1)
  lonindex = which(flon %in% 1)
  return(c(lonindex, latindex))
}

#getLocIndex(lon, lat, x[112,])
site_inds <- t(apply(x, MARGIN = 1, FUN = getLocIndex, lon = lon, lat = lat)) # apply to get all site inds and then transpose

# read in the full period of record using aggregated files
#start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)
lon_vals <- lon[site_inds[,1]]
lat_vals <- lat[site_inds[,2]]

site_tc_location_info<- cbind.data.frame(flx_site_info$SITE_ID, x, lon_vals, lat_vals, site_inds)
colnames(site_tc_location_info) <- c("SITE_ID", "SITE_LON", "SITE_LAT", "TC_LON", "TC_LAT", "TC_LON_IND", "TC_LAT_IND")

# now get the information from each of the terraclimate variables

site_tc_data <- site_tc_location_info

#for (i in 1:length(tc_files)){
for (i in 1:length(tc_files)){
  print(tc_files[i])
  # get terraclimate variable name, string parsing
  tc_varname <- unlist(str_split(tc_files[i], "_"))
  tc_varname <- tc_varname[length(tc_varname)]
  tc_varname <- unlist(str_split(tc_varname, "[.]"))[1] # this is year once parsed
  
  nc <- nc_open(tc_files[i]) # open file
  
  # Column number names assume climatology with 12 months, month indexes are hard coded!
  var_vals <- data.frame(matrix(NA, nrow = nrow(site_tc_location_info), ncol = nc$dim$time$len)) # preallocate enough space
  colnames(var_vals) <- paste("PDSI", tc_varname, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "_") # names for columns, would need to change 
  
  for (j in 1:nrow(site_tc_location_info)){
    var_vals[j, ] <- as.numeric(ncvar_get(nc, varid = "PDSI", start = c(site_inds[j,], 1), count)) # variable is just PDSI
  }
  
  # then cbind to the right
  site_tc_data <- cbind.data.frame(site_tc_data, var_vals)
  
  nc_close(nc)
}

# use write.csv, write_csv introduces floating point error for some reason
write.csv(site_tc_data, "../processed_data/flx_site_combinedsource_terraclimate_PDSI.csv", row.names = F)

# next summarize into seasons 
# get inds
mar <- grep("_03", colnames(site_tc_data))
apr <- grep("_04", colnames(site_tc_data))
may <- grep("_05", colnames(site_tc_data))

jun <- grep("_06", colnames(site_tc_data))
jul <- grep("_07", colnames(site_tc_data))
aug <- grep("_08", colnames(site_tc_data))

sep <- grep("_06", colnames(site_tc_data))
oct <- grep("_07", colnames(site_tc_data))
nov <- grep("_08", colnames(site_tc_data))

pdsi_seasons <- paste("PDSI_", seasons, "_", rep(yrs, each=3), "_mean", sep = "")
x <- data.frame(matrix(NA, nrow = nrow(site_tc_data), ncol = length(pdsi_seasons)))
site_tc_data_seasons <- cbind(site_tc_location_info, x)
colnames(site_tc_data_seasons) <- c(colnames(site_tc_location_info), pdsi_seasons)

yrs <- 1991:2021
for (i in 1:length(yrs)){
  yr <- yrs[i]
  
  spring_ind <- grep(paste("Spring", yr, sep = "_"), colnames(site_tc_data_seasons))
  summer_ind <- grep(paste("Summer", yr, sep = "_"), colnames(site_tc_data_seasons))
  fall_ind <- grep(paste("Fall", yr, sep = "_"), colnames(site_tc_data_seasons))
  
  site_tc_data_seasons[,spring_ind] <- apply(site_tc_data[, c(mar[i], apr[i], may[i])], MARGIN = 1, FUN = mean)
  site_tc_data_seasons[,summer_ind] <- apply(site_tc_data[, c(jun[i], jul[i], aug[i])], MARGIN = 1, FUN = mean)
  site_tc_data_seasons[,fall_ind] <- apply(site_tc_data[, c(sep[i], oct[i], nov[i])], MARGIN = 1, FUN = mean)
} 

write.csv(site_tc_data_seasons, "../processed_data/flx_site_combinedsource_terraclimate_PDSI_seasons.csv", row.names = F)  

# Sensitivity of [detrended] GPP to PDSI, then plotted by aridity (like Fig 1)
# Need to match up available years with PDSI time series from original source FLUXNET seasonal data

#####

# Append PDSI data to the combined source file
# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons.csv")

# Load the PDSI data

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/")
site_tc_data_seasons <- read.csv("flx_site_combinedsource_terraclimate_PDSI_seasons.csv")


# Not every site was extracted with the pdsi because some have less than 5 years, so just keep the intersection
df_all_sub <- df_all[which(df_all$SITE_ID %in% unique(site_tc_data_seasons$SITE_ID)),]

# for each flux site, season, and year
# Get the appropriate PDSI value

pdsi_cols <- colnames(site_tc_data_seasons)
df_all_sub$PDSI_mean <- NA
for (i in 1:nrow(df_all_sub)){
  #print(i), this loop is fast
  site_id <- df_all_sub$SITE_ID[i]
  yr <- df_all_sub$year[i]
  season <- df_all_sub$season[i]
  
  site_id_row <- which(site_tc_data_seasons$SITE_ID == site_id)
  season_yr_col <- grep(paste(season, yr, sep="_"), pdsi_cols)
  
  df_all_sub$PDSI_mean[i] <- site_tc_data_seasons[site_id_row, season_yr_col]
}

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
write.csv(df_all_sub, "combined_source_info_swc1_all_seasons_site_id_intersect_pdsi.csv", row.names = FALSE) # output this appended here
