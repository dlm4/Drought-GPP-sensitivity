# get terraclimate information for locations of flux sites, do large 112 site list

library(tidyverse)
library(ncdf4)

# load in flux site list with lat lon information
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

# go to where the climatology summaries are for terraclimate data
#setwd("../../terraclimate/summaries/")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/aridity/")
#tc_files <- list.files() # list out all the terraclimate variables # not doing terraclimate

# do the matching for lat lon first, and then just retain these values and can apply to extract and cbind

cru_aridity <- read.csv("cruts405_1981-2010_aridity.csv")
lon <- unique(cru_aridity$Lon)
lat <- unique(cru_aridity$Lat)

x <- cbind(flx_site_info$LOCATION_LONG, flx_site_info$LOCATION_LAT) # x is lon, lat location of flux site

# do closest to 1/4 because of site indexing
getLocIndex <- function(lon, lat, x){
  flat = match(abs(lat - x[2]) < 1/4, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x[1]) < 1/4, 1)
  lonindex = which(flon %in% 1)
  return(c(lonindex, latindex))
}

#getLocIndex(lon, lat, x[112,])
site_inds <- t(apply(x, MARGIN = 1, FUN = getLocIndex, lon = lon, lat = lat)) # apply to get all site inds and then transpose

#getLocIndex(lon, lat, x[1,])

# read in the full period of record using aggregated files
#start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)
lon_vals <- lon[site_inds[,1]]
lat_vals <- lat[site_inds[,2]]

site_cru_location_info<- cbind.data.frame(flx_site_info$SITE_ID, x, lon_vals, lat_vals, site_inds)
colnames(site_cru_location_info) <- c("SITE_ID", "SITE_LON", "SITE_LAT", "CRU_LON", "CRU_LAT", "CRU_LON_IND", "CRU_LAT_IND") # CRU 4.05 at half degree grid

# now get the information from each of the terraclimate variables

site_cru_location_info$Aridity_P_PET <- NA
site_cru_location_info$P_Annual_Mean_Total_mm <- NA
site_cru_location_info$PET_Annual_Mean_Total_mm <- NA

for (i in 1:nrow(site_cru_location_info)){
  row <- which((cru_aridity$Lon == site_cru_location_info$CRU_LON[i]) & (cru_aridity$Lat == site_cru_location_info$CRU_LAT[i]))
  site_cru_location_info$Aridity_P_PET[i] <- cru_aridity$Aridity_P_PET[row]
  site_cru_location_info$P_Annual_Mean_Total_mm[i] <- cru_aridity$P_Annual_Mean_Total_mm[row]
  site_cru_location_info$PET_Annual_Mean_Total_mm[i] <- cru_aridity$PET_Annual_Mean_Total_mm[row]
}

write.csv(site_cru_location_info, "site_cru_aridity_1981_2010.csv", row.names = F)


#site_tc_data <- site_tc_location_info

# for (i in 1:length(tc_files)){
#   print(tc_files[i])
#   # get terraclimate variable name, string parsing
#   tc_varname <- unlist(str_split(tc_files[i], "_"))
#   tc_varname <- tc_varname[length(tc_varname)]
#   tc_varname <- unlist(str_split(tc_varname, "[.]"))[1]
#   
#   nc <- nc_open(tc_files[i]) # open file
#   
#   # Column number names assume climatology with 12 months, month indexes are hard coded!
#   var_vals <- data.frame(matrix(NA, nrow = nrow(site_tc_location_info), ncol = nc$dim$time$len)) # preallocate enough space
#   colnames(var_vals) <- paste(tc_varname, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "_") # names for columns, would need to change 
#   
#   for (j in 1:nrow(site_tc_location_info)){
#     var_vals[j, ] <- as.numeric(ncvar_get(nc, varid = tc_varname, start = c(site_inds[j,], 1), count))
#   }
#   
#   # then cbind to the right.
#   site_tc_data <- cbind.data.frame(site_tc_data, var_vals)
# }

# use write.csv, write_csv introduces floating point error for some reason
# write.csv(site_tc_data, "../processed_data/flx_site_combinedsource_terraclimate_data.csv", row.names = F)
