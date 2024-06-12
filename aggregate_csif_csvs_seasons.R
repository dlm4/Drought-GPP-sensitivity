# aggregate_csif_csvs_seasons.R

# get year-season averages for spring, summer, fall for each of the extracted csv files
# Note that 2020 only has enough dates for spring, but not for summer or fall

# 2000 is zero before DOY 53

# Seasons are defined by months
# Spring = MAM, Summer = JJA, Fall = SON

library(tidyverse)
library(lubridate)


getCSIFdate <- function(x){
  # x is csif$filename[index]
  date_string <- unlist(strsplit(x, '[.]'))[5]
  yr_string <- substr(date_string, 1, 4)
  doy_string <- substr(date_string, 5, 7)
  file_date <- as.Date(as.numeric(doy_string) - 1, origin = paste(yr_string, "-01-01", sep = "")) 
  # need to subtract 1 off the DOY to get the date right since R origin is DOY = 0
  return(file_date)
}

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/csif")
filelist <- list.files(pattern = glob2rx("ec_csif_clear_daily_inst_*.csv"))

for (ind in 1:length(filelist)){
  #ind <- 1
  print(ind)
  csif <- read.csv(filelist[ind])
  
  # sapply works but returns numeric version of date, so need to pipe to R's origin for dates which is 1970-01-01
  csif$date <- sapply(csif$filename, FUN = getCSIFdate) %>% as.Date(origin = "1970-01-01")
  csif$month <- month(csif$date)
  
  csif$season <- NA # set to NA initially, will wipe out winter
  csif$season[which(csif$month %in% c(3,4,5))] <- "Spring"
  csif$season[which(csif$month %in% c(6,7,8))] <- "Summer"
  csif$season[which(csif$month %in% c(9,10,11))] <- "Fall"
  csif <- na.omit(csif) # remove NAs
  csif$season <- factor(csif$season, levels = c("Spring", "Summer", "Fall"))
  
  season_mean <- aggregate(subset(csif, select = c(CSIF_clear_daily, CSIF_clear_inst)),
                           by = list(csif$ID, csif$Category, csif$Latitude, csif$Longitude, csif$Col05, csif$Row05, csif$season), FUN = mean)
  
  colnames(season_mean) <- c("ID", "Category", "Latitude", "Longitude", "Col05", "Row05", "Season", "CSIF_clear_daily", "CSIF_clear_inst")
  season_mean$Year <- year(csif$date[1]) # all same year for each time
  
  if (ind == 1){
    season_mean_all <- season_mean
  } else {
    season_mean_all <- rbind.data.frame(season_mean_all, season_mean)
  }
}

# remove summer 2020 since its incomplete
season_mean_all[which(season_mean_all == 2020 & season_mean_all == "Summer"),] <- NA
season_mean_all <- na.omit(season_mean_all)

write.csv(season_mean_all, "ec_csif_season_means.csv", row.names = FALSE)
