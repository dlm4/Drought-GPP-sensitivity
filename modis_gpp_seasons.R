# viewing modis gpp data from appears for ec sites
library(tidyverse)
library(lubridate)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/")
gpp <- read.csv("EC-sites-GPP-MOD17A2HGF-061-results.csv")
gpp$Date <- ymd(gpp$Date) # lubridate date formatting

site_id <- "CH-Cha"

gpp_sub <- subset(gpp, ID == site_id)

ggplot(data = gpp_sub) +
  geom_point(aes(x = Date, y = MOD17A2HGF_061_Gpp_500m))


#SDS Name	Description	                Units	        Data Type	            Fill Value	    No Data Value	  Valid Range	  Scale Factor
#Gpp_500m	Gross Primary Productivity  kgC/m²/8day	  16-bit signed integer	32761 to 32767	N/A	            0 to 30000	  0.0001

out_cols <- c("SITE_ID", "Year", "Season", "GPP")
gpp_sums <- data.frame(matrix(data = NA, nrow = 22*3, ncol = length(out_cols)))
colnames(gpp_sums) <- out_cols

gpp_sums$SITE_ID <- site_id
out_iter <- 1

for (y in 2000:2021){
  y <- 2002
  gpp_sub2 <- subset(gpp_sub, year(Date) == y, c(Date, MOD17A2HGF_061_Gpp_500m))
  
  annual_dates <- seq(ymd(gpp_sub2$Date[1]), (ymd(gpp_sub2$Date[nrow(gpp_sub2)]) + 7), by = "days")
  annual_dates_df <- cbind.data.frame(annual_dates, NA)
  
  # looks like scale factor is already applied for appeears
  
  #  why is there a huge gap in spring 2002?
  # Nothing between "2002-02-18" and "2002-05-25" for any site
  # Will try replacing with v006 and see if this issue is still there
  
  iter <- 1
  for (i in 1:nrow(gpp_sub2)){
    daily_gpp <- gpp_sub2$MOD17A2HGF_061_Gpp_500m[i]/8 # kg C / m2 / day, scale factor is already applied
    annual_dates_df[iter:(iter+7), 2] <- daily_gpp
    iter <- iter + 8
  }
  
  getSeasonSum <- function(annual_dates_df, month_range){
    return(annual_dates_df[which(month(annual_dates_df[,1]) %in% month_range), 2] %>% sum() * 1000)
  }
  
  gpp_spring <- getSeasonSum(annual_dates_df, 3:5)
  gpp_summer <- getSeasonSum(annual_dates_df, 6:8)
  gpp_fall <- getSeasonSum(annual_dates_df, 9:11)
  
  ## append info ##
  
  gpp_sums$Year[out_iter:(out_iter + 2)] <- y
  
  gpp_sums$Season[out_iter] <- "Spring"
  gpp_sums$GPP[out_iter] <- gpp_spring
  
  gpp_sums$Season[out_iter+1] <- "Summer"
  gpp_sums$GPP[out_iter+1] <- gpp_summer
  
  gpp_sums$Season[out_iter+2] <- "Fall"
  gpp_sums$GPP[out_iter+2] <- gpp_fall
  
  ## update output iterator ##
  out_iter <- out_iter + 3 # update by 3 for 3 seasons
  
}
