# aggregate FLUXCOM for each site

library(tidyverse)
library(ncdf4)
library(sf)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_v006")

fluxcom <- nc_open("GPP.RS_V006.FP-ALL.MLM-ALL.METEO-NONE.4320_2160.monthly.2001.nc")

fluxcom_gpp <- ncvar_get(fluxcom, 'GPP')
fluxcom_lat <- ncvar_get(fluxcom, 'lat_bnds') # these are two row ranges, need to average to get middle point
fluxcom_lon <- ncvar_get(fluxcom, 'lon_bnds')
nc_close(fluxcom)
rm(fluxcom)

lat <- colMeans(fluxcom_lat)
lon <- colMeans(fluxcom_lon)

rm(fluxcom_lat)
rm(fluxcom_lon)

# get closest flux sites for each

# Get lat lon info for each of the flux towers and find the nearest available grid cell
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded")
site_locations <- read.csv("../../../ec_site_lat_lon.csv")

site_locations$Col12 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - lat))) # col is Lat
site_locations$Row12 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - lon))) # row is Lon

site_locations$FLUXCOM_GPP <- NA
for (i in 1:nrow(site_locations)){
  site_locations$FLUXCOM_GPP[i] <- fluxcom_gpp[site_locations$Row12[i], site_locations$Col12[i], 7] # test for July = 7 
}

site_locations_original_setup <- site_locations
# some of these have NAs... need to use sf package to get nearest point that's not missing

# Get lat lon info for each of the flux towers and find the nearest available grid cell from the GOSIF imagery
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded")
site_locations <- read.csv("ec_site_lat_lon.csv")

# load fluxcom rs gpp (fluxcom_gpp, above)
# setwd("source/GOSIF-GPP_v2_Monthly/Mean")
# gosif_gpp <- brick("GOSIF_GPP_2000.M03_Mean.tif")

# make site locations a spatial sf object
site_locations_sf <- st_as_sf(site_locations, coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# # locations look OK
# plot(gosif_gpp)
# plot(st_geometry(site_locations_sf), add = TRUE)

# Need mask out available points

# xres <- 0.05
# yres <- 0.05
# yres_vals <- rev(seq(-90+(yres/2), 90-(yres/2), yres)) # this needs to be reversed, top to bottom
# xres_vals <- seq(-180+(xres/2), 180-(xres/2), xres)

# fluxcom lat lon is stored opposite compared to gosif...
# Note these x and y assignments are reversed compared to earlier functions, but not an issue because it's a self-reference within earlier fns
yres_vals <- lon
xres_vals <- lat

yres_mat <- matrix(yres_vals, nrow = length(yres_vals), ncol = length(xres_vals))
xres_mat <- matrix(xres_vals, nrow = length(yres_vals), ncol = length(xres_vals), byrow = TRUE)
# this is now correct...
#### stopped here ####

# gosif_gpp_mask <- Which(gosif_gpp %in% c(65534, 65535))
# gosif_gpp_mask_arr <- raster::as.array(gosif_gpp_mask)

# gosif_gpp_arr <- raster::as.array(gosif_gpp)

# length(which(is.na(fluxcom_gpp[,,1])))
# checked this for different layers, all have the same length of NAs inds

# mask out lat lon cells that are NA in the data
# yres_mat[gosif_gpp_mask_arr] <- NA
# xres_mat[gosif_gpp_mask_arr] <- NA

yres_mat[which(is.na(fluxcom_gpp[,,1]))] <- NA
xres_mat[which(is.na(fluxcom_gpp[,,1]))] <- NA


# get actual closest cells
# fluxcom is transposed, so x is lat and y is lon
# site_locations$Row12 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - yres_vals)))
# site_locations$Col12 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - xres_vals)))
site_locations$Col12 <- sapply(site_locations$Latitude, function(x) which.min(abs(x - xres_vals))) # col is lat
site_locations$Row12 <- sapply(site_locations$Longitude, function(y) which.min(abs(y - yres_vals))) # row is lon

# Get GPP data
#site_locations$GOSIF_GPP_ras <- NA # raster block extracted version
#site_locations$GOSIF_GPP_arr <- NA # array extracted version
site_locations$FLUXCOM_RS_arr <- NA # array extracted version
for (i in 1:nrow(site_locations)){
  #print(i) # loop is very fast without getValuesBlock
  #site_locations$GOSIF_GPP_ras[i] <- getValuesBlock(gosif_gpp, row = site_locations$Row12[i], nrows = 1, col = site_locations$Col12[i], ncols = 1)
  #site_locations$GOSIF_GPP_arr[i] <- gosif_gpp_arr[site_locations$Row12[i], site_locations$Col12[i], 1]
  site_locations$FLUXCOM_RS_arr[i] <- fluxcom_gpp[site_locations$Row12[i], site_locations$Col12[i], 7] # use July
}

site_locations$nudged_pixel <- FALSE # true false for if the row col has been shifted to accomodate missing locations.

# for sites that are missing values, do search around them:
#missing_inds <- which(site_locations$GOSIF_GPP_arr %in% c(65534, 65535)) # only a few
missing_inds <- which(is.na(site_locations$FLUXCOM_RS_arr))
# get subset
win_size <- 11 # pick odd number
w <- floor(win_size/2)

for (i in 1:length(missing_inds)){
  #i <- missing_inds[1] # test for first row
  ind <- missing_inds[i]
  slrow <- site_locations$Row12[ind]
  slcol <- site_locations$Col12[ind]
  
  # get subsets of y and x mats
  yres_mat_sub <- yres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
  xres_mat_sub <- xres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
  #gosif_gpp_sub <- gosif_gpp_arr[(slrow - w):(slrow + w), (slcol - w):(slcol + w), 1]
  fluxcom_gpp_sub <- fluxcom_gpp[(slrow - w):(slrow + w), (slcol - w):(slcol + w), 7] # use July
  
  #latlon_sub <- cbind.data.frame(as.vector(gosif_gpp_sub), as.vector(yres_mat_sub), as.vector(xres_mat_sub))
  latlon_sub <- cbind.data.frame(as.vector(fluxcom_gpp_sub), as.vector(xres_mat_sub), as.vector(yres_mat_sub)) # x = lat and y = lon now
  #colnames(latlon_sub) <- c("GOSIF_GPP", "Lat", "Lon")
  colnames(latlon_sub) <- c("FLUXCOM_RS", "Lat", "Lon")
  latlon_sub <- na.omit(latlon_sub) # remove NA
  latlon_sub_sf <- st_as_sf(latlon_sub, coords = c("Lon", "Lat"),
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # # now the location is good
  # plot(gosif_gpp)
  # plot(st_geometry(latlon_sub_sf), add = TRUE)
  
  # get pixel with minimum distance  
  dist <- st_distance(site_locations_sf[ind,], latlon_sub_sf)
  latlon_sub_site <- latlon_sub[which.min(dist),]
  
  # get this index, need row col
  #row_col_index <- which(yres_mat == latlon_sub_site$Lat & xres_mat == latlon_sub_site$Lon)
  row_col_index <- which(xres_mat == latlon_sub_site$Lat & yres_mat == latlon_sub_site$Lon)
  new_row <- which(yres_vals == yres_mat[row_col_index])
  new_col <- which(xres_vals == xres_mat[row_col_index])
  
  site_locations$nudged_pixel[ind] <- TRUE
  site_locations$Row12[ind] <- new_row
  site_locations$Col12[ind] <- new_col
  #site_locations$GOSIF_GPP_arr[ind] <- latlon_sub_site$GOSIF_GPP
  site_locations$FLUXCOM_RS_arr[ind] <- latlon_sub_site$FLUXCOM_RS
}

# write out locations csv
write.csv(site_locations, "ec_site_locations_for_fluxcom_rs_v006.csv", row.names = FALSE)

#####
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_v006")
filelist <- list.files(pattern = glob2rx("GPP*.nc"))

# make sure site_locations is the correct, updated Site locations based on the nudged RS alignment: "ec_site_locations_for_fluxcom_rs_v006.csv"

year_index <- 8 # part of the file name string that contains the year after splitting on '_'

# this is very fast even with all these loops
for (ind in 1:length(filelist)){ # loop through all netcdf files
  
  fluxcom <- nc_open(filelist[ind])
  fluxcom_gpp <- ncvar_get(fluxcom, 'GPP')
  nc_close(fluxcom)
  rm(fluxcom)
  
  #ec_site_fluxcom_gpp <- site_locations
  ec_site_fluxcom_gpp <- subset(site_locations, select = c(ID, Category, Latitude, Longitude, Col12, Row12))
  
  ec_site_fluxcom_gpp$FLUXCOM_GPP <- NA
  ec_site_fluxcom_gpp$Year <- NA
  ec_site_fluxcom_gpp$Month <- NA
  
  #ind <- 1
  ec_site_fluxcom_gpp$Year <- unlist(strsplit(filelist[ind], '[.]'))[year_index] %>% as.numeric()
  
  for (mo in 1:12){ # loop through all months
    #print(paste(filelist[ind], ", Month ", mo, sep = ""))
    
    ec_site_fluxcom_gpp$Month <- mo
    
    for (i in 1:nrow(ec_site_fluxcom_gpp)){ # loop through all sites
      ec_site_fluxcom_gpp$FLUXCOM_GPP[i] <- fluxcom_gpp[ec_site_fluxcom_gpp$Row12[i], ec_site_fluxcom_gpp$Col12[i], mo]
    }
    
    if (ind == 1 & mo == 1){
      ec_site_fluxcom_gpp_all <- ec_site_fluxcom_gpp
    } else {
      ec_site_fluxcom_gpp_all <- rbind.data.frame(ec_site_fluxcom_gpp_all, ec_site_fluxcom_gpp)
    }
  }
  
}

setwd('../../../fluxcom')
#write.csv(ec_site_fluxcom_gpp_all, "ec_site_fluxcom_rs_meteo_era5_gpp_all_months.csv", row.names = FALSE)
write.csv(ec_site_fluxcom_gpp_all, "ec_site_fluxcom_rs_v006_gpp_all_months.csv", row.names = FALSE)

# Now turn into seasons
ec_site_fluxcom_gpp_all$season <- NA
ec_site_fluxcom_gpp_all$season[which(ec_site_fluxcom_gpp_all$Month %in% c(3,4,5))] <- "Spring"
ec_site_fluxcom_gpp_all$season[which(ec_site_fluxcom_gpp_all$Month %in% c(6,7,8))] <- "Summer"
ec_site_fluxcom_gpp_all$season[which(ec_site_fluxcom_gpp_all$Month %in% c(9,10,11))] <- "Fall"
ec_site_fluxcom_gpp_all <- na.omit(ec_site_fluxcom_gpp_all) # remove NAs, winter
ec_site_fluxcom_gpp_all$season <- factor(ec_site_fluxcom_gpp_all$season, levels = c("Spring", "Summer", "Fall"))

# FLUXCOM-GPP is g C m-2 d-1, so need to multiply each month by number of days and then sum up...
ec_site_fluxcom_gpp_all$nday <- 31 # months 3, 5, 7, 8, 10
ec_site_fluxcom_gpp_all$nday[which(ec_site_fluxcom_gpp$Month %in% c(4, 6, 9, 11))] <- 30 
ec_site_fluxcom_gpp_all$FLUXCOM_GPP_sum <- ec_site_fluxcom_gpp_all$FLUXCOM_GPP * ec_site_fluxcom_gpp_all$nday

# Get a seasonal sum from FLUXCOM_GPP? check units
season_sum <- aggregate(ec_site_fluxcom_gpp_all$FLUXCOM_GPP_sum,
                         by = list(ec_site_fluxcom_gpp_all$ID, ec_site_fluxcom_gpp_all$Category, ec_site_fluxcom_gpp_all$Latitude, ec_site_fluxcom_gpp_all$Longitude, 
                                   ec_site_fluxcom_gpp_all$Col12, ec_site_fluxcom_gpp_all$Row12, ec_site_fluxcom_gpp_all$Year, ec_site_fluxcom_gpp_all$season), FUN = sum)
colnames(season_sum) <- c("ID", "Category", "Latitude", "Longitude", "Col12", "Row12", "Year", "Season", "FLUXCOM_GPP_sum")

#write.csv(season_sum, "ec_site_fluxcom_rs_meteo_era5_gpp_all_seasons_sum.csv", row.names = FALSE)
write.csv(season_sum, "ec_site_fluxcom_rs_v006_gpp_all_seasons_sum.csv", row.names = FALSE)

# Now need to merge in rainfall data and also look at eddy covariance data, next will make all slopes and plots and things, re-do maps sketch out figures