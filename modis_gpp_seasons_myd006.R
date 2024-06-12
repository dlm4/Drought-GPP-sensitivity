# viewing modis gpp data from appears for ec sites
library(tidyverse)
library(lubridate)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/appeears_requests")
#gpp <- read.csv("EC-sites-GPP-MOD17A2HGF-061-results.csv")
gpp <- read.csv("modis_gpp_ec_sites_request4/EC-sites-GPP-4-MYD17A2HGF-006-results.csv")
gpp$Date <- ymd(gpp$Date) # lubridate date formatting

#year_list <- unique(year(gpp$Date))
year_list <- 2003:2021 # should not include 2002 in aqua for seasons
num_years <- length(year_list)

site_id_list <- unique(gpp$ID)

# need to check over outputs to remove data gaps...

for (id_num in 1:length(site_id_list)){
  
  #site_id <- "FR-Pue"
  site_id <- site_id_list[id_num]
  
  gpp_sub <- subset(gpp, ID == site_id)
  
  # ggplot(data = gpp_sub) +
  #   geom_point(aes(x = Date, y = MYD17A2HGF_006_Gpp_500m))
  
  
  #SDS Name	Description	                Units	        Data Type	            Fill Value	    No Data Value	  Valid Range	  Scale Factor
  #Gpp_500m	Gross Primary Productivity  kgC/m²/8day	  16-bit signed integer	32761 to 32767	N/A	            0 to 30000	  0.0001
  
  out_cols <- c("SITE_ID", "Category", "Year", "Season", "GPP")
  gpp_sums <- data.frame(matrix(data = NA, nrow = num_years*3, ncol = length(out_cols))) # this has hard coded number of years...
  colnames(gpp_sums) <- out_cols
  
  gpp_sums$SITE_ID <- site_id
  gpp_sums$Category <- gpp$Category[which(gpp$ID == site_id)[1]]
  out_iter <- 1
  
  for (y in year_list){
    #y <- 2002
    gpp_sub2 <- subset(gpp_sub, year(Date) == y, c(Date, MYD17A2HGF_006_Gpp_500m))
    
    annual_dates <- seq(ymd(gpp_sub2$Date[1]), (ymd(gpp_sub2$Date[nrow(gpp_sub2)]) + 7), by = "days")
    annual_dates_df <- cbind.data.frame(annual_dates, NA)
    
    # looks like scale factor is already applied for appeears
    
    #iter <- 1
    for (i in 1:nrow(gpp_sub2)){
      daily_gpp <- gpp_sub2$MYD17A2HGF_006_Gpp_500m[i]/8 # kg C / m2 / day, scale factor is already applied
      date_row_ind <- which(annual_dates_df$annual_dates == gpp_sub2$Date[i])
      annual_dates_df[date_row_ind:(date_row_ind+7), 2] <- daily_gpp
      #iter <- iter + 8
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
  
  # if first time, use data set, else append onto end
  if (id_num == 1){
    gpp_out <- gpp_sums
  } else {
    gpp_out <- rbind.data.frame(gpp_out, gpp_sums)
  }
  
}

write.csv(gpp_out, file = "../ec_sites_seasons_modis_gpp_MYD17A2HGF_006_Gpp_500m.csv", row.names = F)
