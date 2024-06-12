# aggregate FLUXCOM for each site

library(tidyverse)
library(ncdf4)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_meteo_era5")

fluxcom <- nc_open("GPP.RS_METEO.FP-ALL.MLM-ALL.METEO-ERA5.720_360.monthly.1991.nc")

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

site_locations$Col05 <- sapply(site_locations$Latitude, function(y) which.min(abs(y - lat))) # col is Lat
site_locations$Row05 <- sapply(site_locations$Longitude, function(x) which.min(abs(x - lon))) # row is Lon

site_locations$FLUXCOM_GPP <- NA
for (i in 1:nrow(site_locations)){
  site_locations$FLUXCOM_GPP[i] <- fluxcom_gpp[site_locations$Row05[i], site_locations$Col05[i], 7] # test for July = 7 
}

# all these have values, so the closest element is good, no need for using distance with sf package

#####

filelist <- list.files(pattern = glob2rx("GPP*.nc"))

year_index <- 8 # part of the file name string that contains the year after splitting on '_'

# this is very fast even with all these loops
for (ind in 1:length(filelist)){ # loop through all netcdf files
  
  fluxcom <- nc_open(filelist[ind])
  fluxcom_gpp <- ncvar_get(fluxcom, 'GPP')
  nc_close(fluxcom)
  rm(fluxcom)
  
  ec_site_fluxcom_gpp <- site_locations
  
  ec_site_fluxcom_gpp$FLUXCOM_GPP <- NA
  ec_site_fluxcom_gpp$Year <- NA
  ec_site_fluxcom_gpp$Month <- NA
  
  #ind <- 1
  ec_site_fluxcom_gpp$Year <- unlist(strsplit(filelist[ind], '[.]'))[year_index] %>% as.numeric()
  
  for (mo in 1:12){ # loop through all months
    #print(paste(filelist[ind], ", Month ", mo, sep = ""))
    
    ec_site_fluxcom_gpp$Month <- mo
    
    for (i in 1:nrow(ec_site_fluxcom_gpp)){ # loop through all sites
      ec_site_fluxcom_gpp$FLUXCOM_GPP[i] <- fluxcom_gpp[ec_site_fluxcom_gpp$Row05[i], ec_site_fluxcom_gpp$Col05[i], mo]
    }
    
    if (ind == 1 & mo == 1){
      ec_site_fluxcom_gpp_all <- ec_site_fluxcom_gpp
    } else {
      ec_site_fluxcom_gpp_all <- rbind.data.frame(ec_site_fluxcom_gpp_all, ec_site_fluxcom_gpp)
    }
  }
  
}

setwd('../../../fluxcom')
write.csv(ec_site_fluxcom_gpp_all, "ec_site_fluxcom_rs_meteo_era5_gpp_all_months.csv", row.names = FALSE)

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
                                   ec_site_fluxcom_gpp_all$Col05, ec_site_fluxcom_gpp_all$Row05, ec_site_fluxcom_gpp_all$Year, ec_site_fluxcom_gpp_all$season), FUN = sum)
colnames(season_sum) <- c("ID", "Category", "Latitude", "Longitude", "Col05", "Row05", "Year", "Season", "FLUXCOM_GPP_sum")

write.csv(season_sum, "ec_site_fluxcom_rs_meteo_era5_gpp_all_seasons_sum.csv", row.names = FALSE)
