library(tidyverse)


setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/csif")
filelist <- list.files(pattern = glob2rx("ec_csif_clear_daily_inst_*.csv"))

getCSIFdate <- function(x){
  # x is csif$filename[index]
  date_string <- unlist(strsplit(x, '[.]'))[5]
  yr_string <- substr(date_string, 1, 4)
  doy_string <- substr(date_string, 5, 7)
  file_date <- as.Date(as.numeric(doy_string) - 1, origin = paste(yr_string, "-01-01", sep = "")) 
  # need to subtract 1 off the DOY to get the date right since R origin is DOY = 0
  return(file_date)
}

csif <- read.csv(filelist[17]) # test for 2016
csif$date <- sapply(csif$filename, FUN = getCSIFdate) %>% as.Date(origin = "1970-01-01")

csif_sub <- subset(csif, subset = ID %in% c("AT-Neu", "US-Ha1", "US-NR1"))

ggplot(csif_sub) +
  geom_line(aes(x = date, y = CSIF_clear_daily, color = ID)) + 
  theme_classic()
