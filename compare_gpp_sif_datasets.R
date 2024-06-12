# compare GPP datasets

library(tidyverse)
library(reshape2)

`%notin%` <- Negate(`%in%`)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/")

# MODIS
modis_terra_gpp <- read.csv("modis_gpp/ec_sites_seasons_modis_gpp_MOD17A2HGF_006_Gpp_500m.csv")
modis_aqua_gpp <- read.csv("modis_gpp/ec_sites_seasons_modis_gpp_MYD17A2HGF_006_Gpp_500m.csv")

# FLUXCOM
fluxcom_rs_meteo_era5_gpp <- read.csv("fluxcom/ec_site_fluxcom_rs_meteo_era5_gpp_all_seasons_sum.csv")
fluxcom_rs_v006_gpp <- read.csv("fluxcom/ec_site_fluxcom_rs_v006_gpp_all_seasons_sum.csv")

# GOSIF
gosif_gpp_sif <- read.csv("gosif/gosif_gpp_sif_seasons_scaled.csv")

# CSIF
csif_sif <- read.csv("csif/ec_csif_season_means.csv")

#####
# Subset to necessary pieces and merge into one data set

# All GPP data sets are seasonal sums: g C m-2 season-1
# All SIF data sets are seasonal averages: mW m-2 um-1 sr-1

# for each data set, need: site id, year, season, and variable(s) of interest

modis_terra_gpp <- subset(modis_terra_gpp, select = c("SITE_ID", "Year", "Season", "GPP"))
colnames(modis_terra_gpp)[4] <- "MODIS_Terra_GPP"

modis_aqua_gpp <- subset(modis_aqua_gpp, select = c("SITE_ID", "Year", "Season", "GPP"))
colnames(modis_aqua_gpp)[4] <- "MODIS_Aqua_GPP"

colnames(gosif_gpp_sif)[c(1,4,5)] <- c("SITE_ID", "GOSIF_GPP", "GOSIF_SIF")

fluxcom_rs_meteo_era5_gpp <- subset(fluxcom_rs_meteo_era5_gpp, select = c("ID", "Year", "Season", "FLUXCOM_GPP_sum"))
colnames(fluxcom_rs_meteo_era5_gpp)[c(1,4)] <- c("SITE_ID", "FLUXCOM_RS_METEO_ERA5_GPP")

fluxcom_rs_v006_gpp <- subset(fluxcom_rs_v006_gpp, select = c("ID", "Year", "Season", "FLUXCOM_GPP_sum"))
colnames(fluxcom_rs_v006_gpp)[c(1,4)] <- c("SITE_ID", "FLUXCOM_RS_V006_GPP")

csif_sif <- subset(csif_sif, select = c("ID", "Year", "Season", "CSIF_clear_daily")) # only need CSIF clear daily
colnames(csif_sif)[1] <- "SITE_ID"

# merge everything
# https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames
gpp_sif_merged <- Reduce(function(x, y) merge(x, y, all=TRUE), list(modis_terra_gpp, modis_aqua_gpp, fluxcom_rs_meteo_era5_gpp, fluxcom_rs_v006_gpp, gosif_gpp_sif, csif_sif))
#write.csv(gpp_sif_merged, "ec_site_locations_gpp_sif_gridded_seasons.csv", row.names = FALSE) # write out

# # test plots
# # May need to have MODIS Aqua as a backup for gaps in MODIS Terra 
# site_name <- "AT-Neu"
# gpp_sif_merged$Season <- factor(gpp_sif_merged$Season, levels = c("Spring", "Summer", "Fall"))
# test_sub <- subset(gpp_sif_merged, subset = SITE_ID %in% c(site_name)) # test site
# test_sub <- melt(test_sub, id.vars = c("SITE_ID", "Year", "Season"))
# test_sub_gpp <- subset(test_sub, subset = variable %in% c("MODIS_Terra_GPP", "MODIS_Aqua_GPP", "FLUXCOM_RS_METEO_ERA5_GPP", "FLUXCOM_RS_V006_GPP", "GOSIF_GPP"))
# test_sub_sif <- subset(test_sub, subset = variable %in% c("GOSIF_SIF", "CSIF_clear_daily"))
#                        
# ggplot(test_sub_gpp, aes(x = Year, y = value, color = variable)) +
#   geom_line() +
#   facet_wrap(vars(Season), nrow = 3) + #, scales = "free_y") +
#   labs(title = site_name) +
#   theme_classic()
# 
# ggplot(test_sub_sif, aes(x = Year, y = value, color = variable)) +
#   geom_line() +
#   facet_wrap(vars(Season), nrow = 3) + # , scales = "free_y") +
#   labs(title = site_name) +
#   theme_classic()

#####
# Load seasonal TRENDY data, and combine

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source_error_1992_2016/")
trendy_seasonal <- read.csv("all_model_data_for_ec_sites_season_1991_2016_cable_loc_info_sumna.csv")
# includes GPP from all models and CABLE model meteorology data (precip, air temp, PAR)

# make seasons upper case to match gpp_sif_merged season names
trendy_seasonal$Season[which(trendy_seasonal$Season == "spring")] <- "Spring"
trendy_seasonal$Season[which(trendy_seasonal$Season == "summer")] <- "Summer"
trendy_seasonal$Season[which(trendy_seasonal$Season == "fall")] <- "Fall"

trendy_seasonal_gpp <- subset(trendy_seasonal, subset = ModelName != "SDGVM", select = c("SITE_ID", "Year", "Season", "ModelName", "Model_GPP")) # just get the GPP variables and exclude SDGVM rows
trendy_seasonal_gpp_wide <- dcast(trendy_seasonal_gpp, SITE_ID + Year + Season ~ ModelName, value.var = "Model_GPP") # opposite of melt fn
trendy_seasonal_gpp_wide$TRENDY_model_mean <- rowMeans(trendy_seasonal_gpp_wide[,4:17]) # get mean GPP by row (across all models)
all_gpp_sif_merged <- merge(gpp_sif_merged, trendy_seasonal_gpp_wide, all=TRUE) # merge together and keep everything

# test plot
site_name <- "IL-Yat"
test_sub <- subset(all_gpp_sif_merged, subset = SITE_ID %in% c(site_name) & Year %in% 1992:2021) # test site and year range limit
test_sub <- melt(test_sub, id.vars = c("SITE_ID", "Year", "Season"))
test_sub_gpp <- subset(test_sub, subset = variable %notin% c("GOSIF_SIF", "CSIF_clear_daily"))

ggplot(test_sub_gpp, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(vars(Season), nrow = 3) + #, scales = "free_y") +
  labs(title = site_name) +
  theme_classic()
