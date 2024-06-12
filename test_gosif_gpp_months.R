library(tidyverse)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/gosif")
gpp <- read.csv("ec_gosif_gpp_output_vals.csv")
gpp$Date <- as.Date(paste(gpp$Year, gpp$Month, 15, sep = "-")) # set date for middle of each month

gpp_sub <- subset(gpp, ID %in% c("US-MMS", "US-Me2", "US-NR1"))

ggplot(gpp_sub, aes(x = Date, y = GOSIF_GPP*0.01)) +
  geom_line(aes(color = ID)) + 
  labs(y = bquote("GOSIF-GPP (g C "~m^-2*" " ~mo^-1*")")) +
  theme_bw()


sif <- read.csv("ec_gosif_sif_output_vals.csv")
sif$Date <- as.Date(paste(sif$Year, sif$Month, 15, sep = "-")) # set date for middle of each month

sif_sub <- subset(sif, ID %in% c("US-MMS", "US-Me2", "US-NR1"))

ggplot(sif_sub, aes(x = Date, y = GOSIF_SIF*0.0001)) +
  geom_line(aes(color = ID)) + 
  labs(y = bquote("GOSIF-SIF (W "~m^-2*" " ~mu*m^-1*" "~sr^-1*")")) + # mu symbol is just mu with spacing in bquote
  theme_bw()

#####
# Aggregate to seasonal data
# Monthly GPP scale factor: 0.01
# Monthly SIF scale factor: 0.0001
gpp_sif_merge <- merge(gpp, sif)
gpp_sif_merge <- subset(gpp_sif_merge, subset = gpp_sif_merge$Month %in% 3:11)

# assign season column
gpp_sif_merge$Season <- "Spring"
gpp_sif_merge$Season[which(gpp_sif_merge$Month %in% c(6:8))] <- "Summer"
gpp_sif_merge$Season[which(gpp_sif_merge$Month %in% c(9:11))] <- "Fall"
gpp_sif_merge$Season <- factor(gpp_sif_merge$Season, levels = c("Spring", "Summer", "Fall"))

# SIF
gpp_sif_merge$nday_month <- 31 # assume 31, true for 3, 5, 7, 8, 10
gpp_sif_merge$nday_month[which(gpp_sif_merge$Month %in% c(4, 6, 9, 11))] <- 30

gpp_sif_merge$GOSIF_SIF_month_sum <- gpp_sif_merge$GOSIF_SIF * gpp_sif_merge$nday_month

# GPP
# get sums for GPP
gpp_sif_total <- aggregate(subset(gpp_sif_merge, select = c(GOSIF_GPP, GOSIF_SIF_month_sum)), 
                                 by = list(gpp_sif_merge$ID, gpp_sif_merge$Year, gpp_sif_merge$Season), FUN = sum)
# label
colnames(gpp_sif_total) <- c("ID", "Year", "Season", "GOSIF_GPP_sum", "GOSIF_SIF_month_sum")
# fix scaling
gpp_sif_total$GOSIF_GPP_sum <- gpp_sif_total$GOSIF_GPP_sum * 0.01 # multiply by scale factor for three month sum
#gpp_sif_merge_total$GOSIF_SIF_mean <- gpp_sif_merge_total$GOSIF_SIF_mean * 0.0001 / 3 # multiply by scale factor and divide sum by 3 for 3 months average
# Should be weighted by number of days in month instead...
gpp_sif_total$GOSIF_SIF_mean <- gpp_sif_total$GOSIF_SIF_month_sum * 0.0001 
gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Spring")] <- gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Spring")] / 92 # days in Spring
gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Summer")] <- gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Summer")] / 92 # days in Summer
gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Fall")] <- gpp_sif_total$GOSIF_SIF_mean[which(gpp_sif_total$Season == "Fall")] / 91 # days in Fall

gpp_sif_total_output <- subset(gpp_sif_total, select = c("ID", "Year", "Season", "GOSIF_GPP_sum", "GOSIF_SIF_mean"))

write.csv(gpp_sif_total_output, "gosif_gpp_sif_seasons_scaled.csv", row.names = FALSE)
