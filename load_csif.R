
# do everything again for each year since we reboot R?
for (year in 2019:2020){

# read in CSIF data
library(tidyverse)
library(ncdf4)
library(sf)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/CSIF/test/")
csif <- nc_open("OCO2.SIF.clear.inst.2012189.v2.nc")

# unsure what the exact location of the data is within this file...
#csif_daily <- csif$var$clear_daily_SIF
clear_daily_SIF <- ncvar_get(csif, "clear_daily_SIF")
# this is organized as lon, lat = row, col
# get lat and lon
csif_lat <- ncvar_get(csif, "lat")
csif_lon <- ncvar_get(csif, "lon")


# Get lat lon info for each of the flux towers and find the nearest available grid cell from the CSIF imagery
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded")
site_locations <- read.csv("ec_site_lat_lon.csv")

# make site locations a spatial sf object
site_locations_sf <- st_as_sf(site_locations, coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# get actual closest cells, csif lon, lat = row, col
site_locations$Col05 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - csif_lat)))
site_locations$Row05 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - csif_lon)))

site_locations$CSIF <- NA
for (i in 1:nrow(site_locations)){
  site_locations$CSIF[i] <- clear_daily_SIF[site_locations$Row05[i], site_locations$Col05[i]]
}

# DE-Hte is NA here, not sure if this is a missing value or off the each of the grid
# get subset
win_size <- 11 # pick odd number
w <- floor(win_size/2)

ind <- which(site_locations$ID == "DE-Hte")
slrow <- site_locations$Row05[ind]
slcol <- site_locations$Col05[ind]

clear_daily_SIF_sub <- clear_daily_SIF[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
clear_daily_SIF_sub

# DE-Hte is off the edge of the grid

yres_mat <- matrix(csif_lon, nrow = length(csif_lon), ncol = length(csif_lat)) # lon increasing down
xres_mat <- matrix(csif_lat, nrow = length(csif_lon), ncol = length(csif_lat), byrow = TRUE) # lat increasing to right

yres_mat_sub <- yres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]
xres_mat_sub <- xres_mat[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]

clear_daily_SIF_sub <- clear_daily_SIF[(slrow - w):(slrow + w), (slcol - w):(slcol + w)]

latlon_sub <- cbind.data.frame(as.vector(clear_daily_SIF_sub), as.vector(yres_mat_sub), as.vector(xres_mat_sub))
colnames(latlon_sub) <- c("CSIF", "Lon", "Lat")
latlon_sub <- na.omit(latlon_sub) # remove NA
latlon_sub_sf <- st_as_sf(latlon_sub, coords = c("Lon", "Lat"),
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# get pixel with minimum distance  
dist <- st_distance(site_locations_sf[ind,], latlon_sub_sf)
latlon_sub_site <- latlon_sub[which.min(dist),]

# get this index, need row col
row_col_index <- which(yres_mat == latlon_sub_site$Lon & xres_mat == latlon_sub_site$Lat)
new_row <- which(csif_lon == yres_mat[row_col_index])
new_col <- which(csif_lat == xres_mat[row_col_index])

# ends up shifting location of DE-Hte over by 1
site_locations$Col05[ind] <- new_col
site_locations$Row05[ind] <- new_row

# try again...
site_locations$CSIF <- NA
for (i in 1:nrow(site_locations)){
  site_locations$CSIF[i] <- clear_daily_SIF[site_locations$Row05[i], site_locations$Col05[i]]
}

#####

# access Dan Raid Disk remotely, get both daily and inst
#for (year in 2000:2020){ # range of years of CSIF on server
# year <- 2003
  #setwd("/Volumes/Dan_Raid_Disk/Data/CSIF/CSIF_v2/2000") # just test for 2000
  yr_char <- as.character(year)
  setwd(paste("/Volumes/Dan_Raid_Disk/Data/CSIF/CSIF_v2/", yr_char, sep = ""))
  #setwd(paste("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/CSIF/", yr_char, sep = ""))
  file_list <- list.files(recursive = TRUE) # this takes a little while to get all the file names
  #/Volumes/Dan_Raid_Disk/Data/CSIF/CSIF_v2/2012/OCO2.SIF.clear.inst.2012001.v2.nc
  
  site_locations_csif <- site_locations[,1:6]
  site_locations_csif$filename <- NA
  site_locations_csif$CSIF_clear_daily <- NA
  site_locations_csif$CSIF_clear_inst <- NA
  
  # preallocating makes things faster, output for a single year
  site_locations_csif_all <- data.frame(matrix(NA, nrow = length(file_list)*nrow(site_locations_csif), ncol = ncol(site_locations_csif)))
  colnames(site_locations_csif_all) <- colnames(site_locations_csif)
  
  n_site_inds <- nrow(site_locations_csif)
  start_ind <- 1
  end_ind <- n_site_inds
  for (i in 1:length(file_list)){
    #i <- 1
    #nc_file <- nc_open(file_list[i]) # this is pretty quick, thankfully
    print(paste(file_list[i], "; start: ", start_ind, ", end: ", end_ind, sep = ""))
    csif_new <- nc_open(file_list[i])
    clear_daily_SIF_new <- ncvar_get(csif_new, 'clear_daily_SIF')
    clear_inst_SIF_new <- ncvar_get(csif_new, 'clear_inst_SIF')
    site_locations_csif$filename <- file_list[i]
    for (site_ind in 1:nrow(site_locations)){
      site_locations_csif$CSIF_clear_daily[site_ind] <- clear_daily_SIF_new[site_locations$Row05[site_ind], site_locations$Col05[site_ind]]
      site_locations_csif$CSIF_clear_inst[site_ind] <- clear_inst_SIF_new[site_locations$Row05[site_ind], site_locations$Col05[site_ind]]
    }
    
    nc_close(csif_new) # close connection
    gc()
    
    site_locations_csif_all[start_ind:end_ind,] <- site_locations_csif
    start_ind <- end_ind + 1
    end_ind <- end_ind + n_site_inds
  }
  
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/csif")
  #write.csv(site_locations_csif_all, "ec_csif_clear_daily_2000.csv", row.names = FALSE)
  write.csv(site_locations_csif_all, paste("ec_csif_clear_daily_inst_", yr_char, ".csv", sep = ""), row.names = FALSE)
#}
  rm(list = ls()) # wipe everything
  gc() # garbage collection
}

# note: 2020 is incomplete on the Mac Pro, ends at DOY 221
