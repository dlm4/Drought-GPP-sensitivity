library(tidyverse)
library(ggpubr)
library(reshape2)
library(ggpmisc)
library(ggrepel)
library(ggtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(moments) # for skewness

`%notin%` <- Negate(`%in%`)

#setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
#slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

#flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

# terraclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars_aridity <- read.csv("flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

# Read in list of sites and sensitivities
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

# get flx site info
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
flx_info_merged <- merge(flx_site_info, flx_tc_vars_aridity, by = "SITE_ID")
flx_slope_merged <- merge(flx_info_merged, flx_site_sens, by = "SITE_ID")

spring10 <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum" & Npts >= 10) # 61 sites in spring
summer10 <- subset(flx_slope_merged, Season == "Summer" & Met_Var == "P_F_sum" & Npts >= 10) # 62 sites in summer
fall10 <- subset(flx_slope_merged, Season == "Fall" & Met_Var == "P_F_sum" & Npts >= 10) # 63 sites in fall


setwd("site_info_lists")
write.csv(spring10, "site_list_spring10.csv", row.names = FALSE)

spring10$SITE_ID[which(spring10$SITE_ID %notin% summer10$SITE_ID)]
spring10$SITE_ID[which(spring10$SITE_ID %notin% fall10$SITE_ID)]

summer10$SITE_ID[which(summer10$SITE_ID %notin% spring10$SITE_ID)]
summer10$SITE_ID[which(summer10$SITE_ID %notin% fall10$SITE_ID)]

fall10$SITE_ID[which(fall10$SITE_ID %notin% spring10$SITE_ID)]
fall10$SITE_ID[which(fall10$SITE_ID %notin% summer10$SITE_ID)]

summer10_sub <- summer10[which(summer10$SITE_ID %notin% spring10$SITE_ID),]

write.csv(summer10_sub, "site_list_summer10_sub.csv", row.names = FALSE)
# fall includes all those listed in spring and summer except for CA-TP3.

all5 <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum" & Npts >= 5) # 112 sites in the 5+ set, same for all seasons

all5_sub <- all5[which(all5$SITE_ID %notin% c(spring10$SITE_ID, summer10$SITE_ID, fall10$SITE_ID)),]
write.csv(all5_sub, "site_list_all5_sub.csv", row.names = FALSE)
