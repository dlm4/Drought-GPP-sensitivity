# get terraclimate information for locations of flux sites

# Version 2/28/2023: Ngoc sites

library(tidyverse)
library(ncdf4)

# load in flux site list with lat lon information
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/dryland_sites_ngoc/")
flx_site_info <- read_csv("site_lists/all_sites.csv")

# go to where the climatology summaries are for terraclimate data
setwd("../../terraclimate/summaries/")
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

for (i in 1:length(tc_files)){
  print(tc_files[i])
  # get terraclimate variable name, string parsing
  tc_varname <- unlist(str_split(tc_files[i], "_"))
  tc_varname <- tc_varname[length(tc_varname)]
  tc_varname <- unlist(str_split(tc_varname, "[.]"))[1]
  
  nc <- nc_open(tc_files[i]) # open file
  
  # Column number names assume climatology with 12 months, month indexes are hard coded!
  var_vals <- data.frame(matrix(NA, nrow = nrow(site_tc_location_info), ncol = nc$dim$time$len)) # preallocate enough space
  colnames(var_vals) <- paste(tc_varname, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "_") # names for columns, would need to change 
  
  for (j in 1:nrow(site_tc_location_info)){
    var_vals[j, ] <- as.numeric(ncvar_get(nc, varid = tc_varname, start = c(site_inds[j,], 1), count))
  }
  
  # then cbind to the right.
  site_tc_data <- cbind.data.frame(site_tc_data, var_vals)
}

# get sums for aridity
PET_sum <- site_tc_data[,grep("pet*", colnames(site_tc_data))] %>% apply(1,sum)
PPT_sum <- site_tc_data[,grep("ppt*", colnames(site_tc_data))] %>% apply(1,sum)
TC_aridity <- PPT_sum / PET_sum

site_tc_data$PET_sum <- PET_sum
site_tc_data$PPT_sum <- PPT_sum
site_tc_data$TC_aridity <- TC_aridity

# use write.csv, write_csv introduces floating point error for some reason
write.csv(site_tc_data, "../dryland_sites_ngoc/all_sites_terraclimate_data.csv", row.names = F)

flx_site_info$PET_sum <- PET_sum
flx_site_info$PPT_sum <- PPT_sum
flx_site_info$TC_aridity <- TC_aridity
write.csv(flx_site_info, "../dryland_sites_ngoc/all_sites_info_terraclimate_data.csv", row.names = F)
