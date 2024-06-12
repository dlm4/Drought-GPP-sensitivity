library(tidyverse)
library(raster)
library(sf)

# Get lat lon info for each of the flux towers and find the nearest available grid cell from the GOSIF imagery
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded")
site_locations <- read.csv("ec_site_lat_lon.csv")

# load gosif imagery
setwd("source/GOSIF-GPP_v2_Monthly/Mean")
gosif_gpp <- brick("GOSIF_GPP_2000.M03_Mean.tif")

# make site locations a spatial sf object
site_locations_sf <- st_as_sf(site_locations, coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# # locations look OK
# plot(gosif_gpp)
# plot(st_geometry(site_locations_sf), add = TRUE)

# Need mask out available points

xres <- 0.05
yres <- 0.05
yres_vals <- rev(seq(-90+(yres/2), 90-(yres/2), yres)) # this needs to be reversed, top to bottom
xres_vals <- seq(-180+(xres/2), 180-(xres/2), xres)

yres_mat <- matrix(yres_vals, nrow = length(yres_vals), ncol = length(xres_vals))
xres_mat <- matrix(xres_vals, nrow = length(yres_vals), ncol = length(xres_vals), byrow = TRUE)

gosif_gpp_mask <- Which(gosif_gpp %in% c(65534, 65535))
gosif_gpp_mask_arr <- raster::as.array(gosif_gpp_mask)

gosif_gpp_arr <- raster::as.array(gosif_gpp)

yres_mat[gosif_gpp_mask_arr] <- NA
#image(yres_mat)
xres_mat[gosif_gpp_mask_arr] <- NA

# get actual closest cells
site_locations$Row05 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - yres_vals)))
site_locations$Col05 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - xres_vals)))

# Get GPP data
#site_locations$GOSIF_GPP_ras <- NA # raster block extracted version
site_locations$GOSIF_GPP_arr <- NA # array extracted version
for (i in 1:nrow(site_locations)){
  #print(i) # loop is very fast without getValuesBlock
  #site_locations$GOSIF_GPP_ras[i] <- getValuesBlock(gosif_gpp, row = site_locations$Row05[i], nrows = 1, col = site_locations$Col05[i], ncols = 1)
  site_locations$GOSIF_GPP_arr[i] <- gosif_gpp_arr[site_locations$Row05[i], site_locations$Col05[i], 1] 
}

site_locations$nudged_pixel <- FALSE # true false for if the row col has been shifted to accomodate missing locations.

# for sites that are missing values, do search around them:
missing_inds <- which(site_locations$GOSIF_GPP_arr %in% c(65534, 65535)) # only a few

# get subset
win_size <- 11 # pick odd number
w <- floor(win_size/2)

for (i in 1:length(missing_inds)){
  #i <- missing_inds[1] # test for first row
  ind <- missing_inds[i]
  slrow <- site_locations$Row05[ind]
  slcol <- site_locations$Col05[ind]
  
  yres_mat_sub <- yres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
  xres_mat_sub <- xres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
  gosif_gpp_sub <- gosif_gpp_arr[(slrow - w):(slrow + w), (slcol - w):(slcol + w), 1]
  
  latlon_sub <- cbind.data.frame(as.vector(gosif_gpp_sub), as.vector(yres_mat_sub), as.vector(xres_mat_sub))
  colnames(latlon_sub) <- c("GOSIF_GPP", "Lat", "Lon")
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
  row_col_index <- which(yres_mat == latlon_sub_site$Lat & xres_mat == latlon_sub_site$Lon)
  new_row <- which(yres_vals == yres_mat[row_col_index])
  new_col <- which(xres_vals == xres_mat[row_col_index])
  
  site_locations$nudged_pixel[ind] <- TRUE
  site_locations$Row05[ind] <- new_row
  site_locations$Col05[ind] <- new_col
  site_locations$GOSIF_GPP_arr[ind] <- latlon_sub_site$GOSIF_GPP
}

# write out locations csv
write.csv(site_locations, "../../../ec_site_locations_for_gosif.csv", row.names = FALSE)

#####
# extract gosif gpp data for each site

ec_site_info <- subset(site_locations, select = c(ID, Category, Latitude, Longitude, Row05, Col05, nudged_pixel))

# for one tif file
GOSIF_output_sub <- ec_site_info
GOSIF_output_sub$Year <- NA
GOSIF_output_sub$Month <- NA
GOSIF_output_sub$GOSIF_GPP <- NA # unscaled for now

#getwd()
# get list of all tifs
file_list <- list.files(pattern = glob2rx("*.tif")) # get all tifs

for (i in 1:length(file_list)){
#for (i in 1:15){ # test on first 15
  #i <- 1
  print(file_list[i]) # print name of raster tif
  ym <- tail(unlist(strsplit(file_list[i], "_")), 2)[1] # access second from last element
  ym <- unlist(strsplit(ym, "[.]M"))
  yr <- as.integer(ym[1]) # get year
  mm <- as.integer(ym[2]) # get month
  
  GOSIF_output_sub$Year <- yr
  GOSIF_output_sub$Month <- mm
  
  img <- brick(file_list[i]) # read in raster tif to memory
  img_arr <- raster::as.array(img) # coerce raster tif to array
  
  # quick loop to access locations and add in
  for (j in 1:nrow(GOSIF_output_sub)){
    GOSIF_output_sub$GOSIF_GPP[j] <- img_arr[GOSIF_output_sub$Row05[j], GOSIF_output_sub$Col05[j], 1] 
  }
  
  # if first time, this is the output file
  if (i == 1){
    GOSIF_output <- GOSIF_output_sub
  } else {
    GOSIF_output <- rbind.data.frame(GOSIF_output, GOSIF_output_sub)
  }
  # else, rbind append
}
write.csv(GOSIF_output, "../../../gosif/ec_gosif_gpp_output_vals.csv", row.names = FALSE)


#####
# repeat extraction for regular gosif sif data
ec_site_info <- subset(site_locations, select = c(ID, Category, Latitude, Longitude, Row05, Col05, nudged_pixel))

# for one tif file
GOSIF_output_sub <- ec_site_info
GOSIF_output_sub$Year <- NA
GOSIF_output_sub$Month <- NA
GOSIF_output_sub$GOSIF_SIF <- NA # unscaled for now

#getwd()
# get list of all tifs
setwd("../../GOSIF_V2_Monthly")
file_list <- list.files(pattern = glob2rx("*.tif")) # get all tifs

for (i in 1:length(file_list)){
  #for (i in 1:15){ # test on first 15
  #i <- 1
  print(file_list[i]) # print name of raster tif
  #ym <- tail(unlist(strsplit(file_list[i], "_")), 2)[1] # access second from last element
  ym <- unlist(strsplit(file_list[i], "_"))[2] # access second from last element
  ym <- unlist(strsplit(ym, "[.]M"))
  yr <- as.integer(ym[1]) # get year
  ym2 <- unlist(strsplit(ym[2], "[.]"))
  mm <- as.integer(ym2[1]) # get month
  
  GOSIF_output_sub$Year <- yr
  GOSIF_output_sub$Month <- mm
  
  img <- brick(file_list[i]) # read in raster tif to memory
  img_arr <- raster::as.array(img) # coerce raster tif to array
  
  # quick loop to access locations and add in
  for (j in 1:nrow(GOSIF_output_sub)){
    GOSIF_output_sub$GOSIF_SIF[j] <- img_arr[GOSIF_output_sub$Row05[j], GOSIF_output_sub$Col05[j], 1] 
  }
  
  # if first time, this is the output file
  if (i == 1){
    GOSIF_output <- GOSIF_output_sub
  } else {
    GOSIF_output <- rbind.data.frame(GOSIF_output, GOSIF_output_sub)
  }
  # else, rbind append
}
write.csv(GOSIF_output, "../../gosif/ec_gosif_sif_output_vals.csv", row.names = FALSE)

# Testing junk below here
#####
# 
# flx_site_info_sub_sf <- st_as_sf(flx_site_info_sub, coords = c("LOCATION_LONG", "LOCATION_LAT"),
#                                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# dist <- st_distance(flx_site_info_sub_sf[i,], trendy_slopes_sf)
# flx_site_output[i, col_start - 1] <- min(dist)
# flx_site_output[i, col_start:col_end] <- trendy_slopes[which.min(dist),]
# 
# 
# 
# 
# #####
# site_locations$Row05 <- NA
# site_locations$Col05 <- NA
# 
# xres <- 0.05
# yres <- 0.05
# yres_vals <- rev(seq(-90+(yres/2), 90-(yres/2), yres)) # this needs to be reversed, top to bottom
# xres_vals <- seq(-180+(xres/2), 180-(xres/2), xres)
# 
# y <- site_locations$Latitude[1]
# x <- site_locations$Longitude[1]
# 
# yres_vals[which.min(abs(y - yres_vals))]
# xres_vals[which.min(abs(x - xres_vals))]
# 
# site_locations$Row05 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - yres_vals)))
# site_locations$Col05 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - xres_vals)))
# 
# 
# yres_vals[site_locations$Row05[1]]
# xres_vals[site_locations$Col05[1]]
# # these are the closest cells not accounting for potential gaps due to water and other missing data values
# 
# setwd("source/GOSIF-GPP_v2_Monthly/Mean")
# gosif_gpp <- brick("GOSIF_GPP_2000.M03_Mean.tif")
# gosif_gpp_arr <- raster::as.array(gosif_gpp)
# 
# gosif_gpp_mask <- Which(gosif_gpp %in% c(65534, 65535))
# 
# i <- 46 # US-NR1
# getValuesBlock(gosif_gpp, row = site_locations$Row05[i], nrows = 1, col = site_locations$Col05[i], ncols = 1)
# 
# getValuesBlock(gosif_gpp, row = 1400, nrows = 1, col = 3600, ncols = 1)
# # of course it's a water pixel... or is it flipped somehow?
# 
# # now that it's correct orientation, try extracting for each
# site_locations$GOSIF_GPP <- NA
# for (i in 1:nrow(site_locations)){
#   print(i)
#   site_locations$GOSIF_GPP[i] <- getValuesBlock(gosif_gpp, row = site_locations$Row05[i], nrows = 1, col = site_locations$Col05[i], ncols = 1)
# }
# # this is super slow
# # still has missing locations, so need to refine locations based on pixel availability from mask
# # but need to do nearest available with point sf locations distances because of lat lon vs meter distance
# 
# 
# 
# yres_mat <- matrix(rev(yres_vals), nrow = length(yres_vals), ncol = length(xres_vals))
# xres_mat <- matrix(xres_vals, nrow = length(yres_vals), ncol = length(xres_vals), byrow = TRUE)
# 
# gosif_gpp_mask_arr <- raster::as.array(gosif_gpp_mask)
# 
# 
# yres_mat[gosif_gpp_mask_arr] <- NA
# #image(yres_mat)
# xres_mat[gosif_gpp_mask_arr] <- NA
# 
# i <- 1
# init_row <- site_locations$Row05[i]
# init_col <- site_locations$Col05[i]
# 
# 
# range <- 10
# yres_mat[(init_row - range):(init_row + range), (init_col - range):(init_col + range)]
# 
# #####
# flx_site_info_sub_sf <- st_as_sf(flx_site_info_sub, coords = c("LOCATION_LONG", "LOCATION_LAT"),
#                                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# dist <- st_distance(flx_site_info_sub_sf[i,], trendy_slopes_sf)
# flx_site_output[i, col_start - 1] <- min(dist)
# flx_site_output[i, col_start:col_end] <- trendy_slopes[which.min(dist),]
