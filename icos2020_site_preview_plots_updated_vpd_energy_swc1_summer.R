# ICOS 2020 data processed according to FLUXNET2015 format

#####
library(tidyverse)
library(lubridate) # might not need it
library(reshape2)
`%notin%` <- Negate(`%in%`) # %notin% function

# location of ICOS 2020 drought data
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")

# Site
#site_list <- read.csv("../fluxnet2015_sites_subset_list1_sorted.csv", header = T)
#site_list <- site_list[,1]
site_list <- read.csv("icos_2020_warm_winter_site_list.csv", header = T)
site_list <- site_list[,1]

# Loop over site list
for (flx_name in site_list){
  # Need to reset the path every time
  setwd("D:/berkeley/icos_warm_winter_2020")
  
  #flx_name <- "US-MMS"
  
  # southern hemisphere not relevant for icos
  south_hem_sites <- c("AU-How", "AU-Tum", "BR-Sa1", "ZM-Mon",
                       "AU-DaP", "AU-DaS", "AU-Dry", "AU-Stp")
  
  # Load in site monthly data
  flx <- read.csv(list.files(pattern = glob2rx(paste("*", flx_name, "*FULLSET_MM*.csv", sep = "")), recursive = T)[1])
  setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")
  
  flx[flx == -9999] <- NA
  
  # southern hemisphere not relevant for icos
  # shift dates back by six months if site is in the southern hemisphere site list
  if (flx_name %in% south_hem_sites){
    print("Southern Hemisphere site - Shifting dates by -6 months")
    timestamp_dates <- ymd(paste(as.character(flx$TIMESTAMP), "01", sep = "")) # get year-month-day
    timestamp_dates <- timestamp_dates - months(6) # subtract 6 months
    flx$year <- year(timestamp_dates)
    flx$month <- month(timestamp_dates)
  } else {
    # if site is not in the southern hemisphere site list, keep as standard
    print("Northern Hemisphere site - not shifting dates")
    flx$year <- as.numeric(substr(as.character(flx$TIMESTAMP), 1, 4)) # get year
    flx$month <- as.numeric(substr(as.character(flx$TIMESTAMP), 5, 6)) # get month 
  }
  
  #####
  # VARIABLES
  # GPP = GPP_NT_VUT_REF
  #   g c m-2 d-1, average from daily data
  
  # Precip
  #   P_F, P_ERA
  #   mm d-1, average from daily data (mm per day)
  
  # Temperature
  #   TA_F, TA_ERA
  #   deg C, average from daily data
  
  # PAR and SW_IN
  #   PPFD_IN
  #   umol m-2 s-1, average from daily data
  #
  #   SW_IN_F, SW_IN_ERA
  #   W m-2, average from daily data
  
  # VPD
  # hPa
  
  # RH
  # RH is not available in the monthly organized data, would need to go back and look at HH or hourly data
  # %
  
  # Energy
  # NETRAD, net radiation
  # W m-2, average from daily data (only periods with more than 50% records available)
  # NETRAD_QC, 0-1 percentage of measured and good quality gapfill
  
  # G_F_MDS, soil heat flux
  # W m-2, average from daily data
  # G_F_MDS_QC, 0-1 percentage of measured and good quality gapfill
  
  # LE_F_MDS, latent heat flux
  # W m-2, average from daily data
  # LE_F_MDS_QC, 0-1 percentage of measured and good quality gapfill
  # LE_CORR, corrected LE_F_MDS
  
  # H_F_MDS, sensible heat flux
  # W m-2, average from daily data
  # H_F_MDS_QC, 0-1 percentage of measured and good quality gapfill
  # H_CORR, corrected LE_F_MDS
  
  # SWC_F_MDS_1
  # soil water content at the shallowest depth
  # SWC_F_MDS_1_QC
  
  # Add PPFD as column of NA if it's missing (e.g. NL-Hor doesn't have it)
  if ("PPFD_IN" %notin% colnames(flx)){
    flx$PPFD_IN <- NA
  }
  if ("PPFD_IN_QC" %notin% colnames(flx)){
    flx$PPFD_IN_QC <- NA
  }
  
  # Same for NETRAD (Ch-Lae doesn't have it)
  if ("NETRAD" %notin% colnames(flx)){
    flx$NETRAD <- NA
  }
  if ("NETRAD_QC" %notin% colnames(flx)){
    flx$NETRAD_QC <- NA
  }
  
  # same for SWC_F_MDS_1 (many sites may not have it)
  if ("SWC_F_MDS_1" %notin% colnames(flx)){
    flx$SWC_F_MDS_1 <- NA
  }
  if ("SWC_F_MDS_1_QC" %notin% colnames(flx)){
    flx$SWC_F_MDS_1_QC <- NA
  }
  
  # Set output to site name directory, Create directory of site name if necessary
  #setwd("../plots/flux_site_summary_updated_vpd/")
  setwd("plots/flux_site_summary_energy_swc1_summer/")
  site_dirs <- list.files()
  if (flx_name %notin% site_dirs){
    dir.create(flx_name) 
  }
  setwd(flx_name)
  
  # Standard plot output sizes
  width_size <- 8
  height_size <- 5
  units_var <- "in"
  
  #####
  # summer PLOTS
  # Add in ERA and SW_IN_F columns for completeness and make relevant plots...
  # Add energy
  # adjust month range here for summer
  flx_summer <- subset(flx, month %in% 6:8, select = c(month, year,
                                                       GPP_NT_VUT_REF, NEE_VUT_REF_QC, 
                                                       P_F, P_F_QC, P_ERA,
                                                       TA_F, TA_F_QC, TA_ERA,
                                                       PPFD_IN, PPFD_IN_QC, SW_IN_F, SW_IN_F_QC, SW_IN_ERA,
                                                       VPD_F, VPD_F_QC, VPD_ERA,
                                                       NETRAD, NETRAD_QC,
                                                       G_F_MDS, G_F_MDS_QC,
                                                       LE_F_MDS, LE_F_MDS_QC, LE_CORR,
                                                       H_F_MDS, H_F_MDS_QC, H_CORR,
                                                      SWC_F_MDS_1, SWC_F_MDS_1_QC))
  flx_summer$ndays <- NA
  flx_summer$ndays[flx_summer$month == 6] <- 30 # june has 30 days
  flx_summer$ndays[flx_summer$month == 7] <- 31 # july has 31 days
  flx_summer$ndays[flx_summer$month == 8] <- 31 # aug has 31 days
  
  
  summer_years <- unique(flx_summer$year)
  # sum or mean values, and mean of QC as well
  flx_summer_summary_colnames <- c("year", 
                                   "GPP_NT_VUT_REF_sum", "NEE_VUT_REF_QC_mean",
                                   "P_F_sum", "P_F_QC_mean", "P_ERA_sum",
                                   "TA_F_mean", "TA_F_QC_mean", "TA_ERA_mean", 
                                   "PPFD_IN_mean", "PPFD_IN_QC_mean", "SW_IN_F_mean", "SW_IN_F_QC_mean", "SW_IN_ERA_mean",
                                   "VPD_F_mean", "VPD_F_QC_mean", "VPD_ERA_mean",
                                   "NETRAD_mean", "NETRAD_QC_mean",
                                   "G_F_MDS_mean", "G_F_MDS_QC_mean",
                                   "LE_F_MDS_mean", "LE_F_MDS_QC_mean", "LE_CORR_mean",
                                   "H_F_MDS_mean", "H_F_MDS_QC_mean", "H_CORR_mean",
                                 "SWC_F_MDS_1_mean", "SWC_F_MDS_1_QC_mean")
  
  flx_summer_summary <- data.frame(matrix(NA, nrow = length(summer_years), ncol = length(flx_summer_summary_colnames)))
  colnames(flx_summer_summary) <- flx_summer_summary_colnames
  flx_summer_summary$year <- summer_years
  
  for (i in 1:length(summer_years)){
    yr <- summer_years[i]
    
    # GPP
    flx_summer_summary$GPP_NT_VUT_REF_sum[i] <- sum(flx_summer$GPP_NT_VUT_REF[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr])
    flx_summer_summary$NEE_VUT_REF_QC_mean[i] <- sum(flx_summer$NEE_VUT_REF_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # Precip
    flx_summer_summary$P_F_sum[i] <- sum(flx_summer$P_F[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr])
    flx_summer_summary$P_F_QC_mean[i] <- sum(flx_summer$P_F_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$P_ERA_sum[i] <- sum(flx_summer$P_ERA[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr])
    
    # TA
    flx_summer_summary$TA_F_mean[i] <- sum(flx_summer$TA_F[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$TA_F_QC_mean[i] <- sum(flx_summer$TA_F_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$TA_ERA_mean[i] <- sum(flx_summer$TA_ERA[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # PAR and SW_IN
    flx_summer_summary$PPFD_IN_mean[i] <- sum(flx_summer$PPFD_IN[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$PPFD_IN_QC_mean[i] <- sum(flx_summer$PPFD_IN_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr], na.rm = T) / (30 + 31 + 31)
    flx_summer_summary$SW_IN_F_mean[i] <- sum(flx_summer$SW_IN_F[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$SW_IN_F_QC_mean[i] <- sum(flx_summer$SW_IN_F_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$SW_IN_ERA_mean[i] <- sum(flx_summer$SW_IN_ERA[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # VPD
    flx_summer_summary$VPD_F_mean[i] <- sum(flx_summer$VPD_F[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$VPD_F_QC_mean[i] <- sum(flx_summer$VPD_F_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$VPD_ERA_mean[i] <- sum(flx_summer$VPD_ERA[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # ENERGY
    # NETRAD
    flx_summer_summary$NETRAD_mean[i] <- sum(flx_summer$NETRAD[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$NETRAD_QC_mean[i] <- sum(flx_summer$NETRAD_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # G_F_MDS
    flx_summer_summary$G_F_MDS_mean[i] <- sum(flx_summer$G_F_MDS[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$G_F_MDS_QC_mean[i] <- sum(flx_summer$G_F_MDS_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # LE
    flx_summer_summary$LE_F_MDS_mean[i] <- sum(flx_summer$LE_F_MDS[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$LE_F_MDS_QC_mean[i] <- sum(flx_summer$LE_F_MDS_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$LE_CORR_mean[i] <- sum(flx_summer$LE_CORR[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # H
    flx_summer_summary$H_F_MDS_mean[i] <- sum(flx_summer$H_F_MDS[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$H_F_MDS_QC_mean[i] <- sum(flx_summer$H_F_MDS_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    flx_summer_summary$H_CORR_mean[i] <- sum(flx_summer$H_CORR[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
    
    # SWC_F_MDS_1
    flx_summer_summary$SWC_F_MDS_1_mean[i] <- sum(flx_summer$SWC_F_MDS_1[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr])/ (30 + 31 + 31)
    flx_summer_summary$SWC_F_MDS_1_QC_mean[i] <- sum(flx_summer$SWC_F_MDS_1_QC[flx_summer$year == yr] * flx_summer$ndays[flx_summer$year == yr]) / (30 + 31 + 31)
  }
  
  # output flx_summer, monthly data without averaging
  write.csv(flx_summer, file = paste(flx_name, "_flx_summer_months.csv", sep = ""), row.names = F)
  
  # output: flx_summer_summary data frame as csv
  # flx_summer_summary_output <- flx_summer_summary
  # flx_summer_summary_output[, 2:length(flx_summer_summary_colnames)] <- round(flx_summer_summary[, 2:length(flx_summer_summary_colnames)], digits = 3)
  write.csv(flx_summer_summary, file = paste(flx_name, "_flx_summer_summary.csv", sep = ""), row.names = F)
  


  #####
  # Plotting against one another

  ##
  # Precip vs. GPP
  ggplot(data = flx_summer_summary, aes(x = P_F_sum, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_p_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = P_ERA_sum, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_p_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)


  ##
  # TA vs. GPP
  ggplot(data = flx_summer_summary, aes(x = TA_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = TA_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ##
  # PPFD or SW vs. GPP
  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ## VPD vs. GPP
  ggplot(data = flx_summer_summary, aes(x = VPD_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_vpd_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = VPD_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_vpd_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## NETRAD vs GPP
  ggplot(data = flx_summer_summary, aes(x = NETRAD_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_netrad_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## NETRAD - G_F_MDS vs GPP
  ggplot(data = flx_summer_summary, aes(x = NETRAD_mean-G_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_netrad_min_g_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## LE and/or H vs GPP
  ggplot(data = flx_summer_summary, aes(x = LE_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_le_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = H_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_h_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = LE_F_MDS_mean+H_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_le_h_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = LE_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_le_corr_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = H_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_h_corr_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = LE_CORR_mean+H_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_le_h_corr_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # SWC_F_MDS_1 vs GPP
  ggplot(data = flx_summer_summary, aes(x = SWC_F_MDS_1_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_swc1_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  

  ##
  # PPFD or SW vs TA_F_mean
  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = TA_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_ta.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_F_mean, y = TA_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_f_ta.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_ERA_mean, y = TA_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_era_ta_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ##
  # PPFD or SW vs. Precip
  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  # PPFD or SW vs VPD
  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_vpd.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_F_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_f_vpd.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_ERA_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_era_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  
  
  
  ##
  # TA vs. Precip
  ggplot(data = flx_summer_summary, aes(x = TA_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = TA_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # TA vs. VPD
  ggplot(data = flx_summer_summary, aes(x = TA_F_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_f_vpd_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = TA_ERA_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_era_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # VPD vs. Precip
  ggplot(data = flx_summer_summary, aes(x = VPD_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_vpd_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = VPD_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_vpd_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  #####
  # Plot similar variables against themselves for comparison, ERA values on Y if possible
  # Precip
  ggplot(data = flx_summer_summary, aes(x = P_F_sum, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_p_f_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # TA
  ggplot(data = flx_summer_summary, aes(x = TA_F_mean, y = TA_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ta_f_ta_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # PPFD and SW
  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = SW_IN_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_sw_in_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = PPFD_IN_mean, y = SW_IN_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_ppfd_in_sw_in_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_summer_summary, aes(x = SW_IN_F_mean, y = SW_IN_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_sw_in_f_sw_in_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # VPD
  ggplot(data = flx_summer_summary, aes(x = VPD_F_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_vpd_f_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # LE
  ggplot(data = flx_summer_summary, aes(x = LE_F_MDS_mean, y = LE_CORR_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_le_f_mds_le_corr.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # H
  ggplot(data = flx_summer_summary, aes(x = H_F_MDS_mean, y = H_CORR_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_h_f_mds_h_corr.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # NETRAD
  ggplot(data = flx_summer_summary, aes(x = NETRAD_mean, y = LE_F_MDS_mean+H_F_MDS_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_netrad_le_h_f_mds.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_summer_summary, aes(x = NETRAD_mean-G_F_MDS_mean, y = LE_F_MDS_mean+H_F_MDS_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_netrad_min_g_f_mds_le_h_f_mds.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  
  
  
  #####
  # Plot by years on x, facet wrap
  flx_summer_summary_melt <- melt(flx_summer_summary, id.vars = list("year"))
  ggplot(data = flx_summer_summary_melt, aes(x = year, y = value)) +
    geom_point() + geom_line() +
    #geom_text(aes(label = as.character(year)), size = 3)+
    facet_wrap(~variable, scales = "free_y") +
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_summer_summary.png", sep = ""), width = width_size*2, height = height_size*2, units = units_var)
  
  #####
  # Plot these as a grid against one another
  # fss_sub <- flx_summer_summary[, 2:5]
  # plot(fss_sub)
  
  #####
  # # Normalize and plot all together
  # flx_summer_summary_norm <- flx_summer_summary
  # # convert these to z scores to plot against each other
  # normVector <- function(x){
  #   return((x - mean(x)) / sd(x))
  # }
  # flx_summer_summary_norm[, 2:5] <- apply(flx_summer_summary[, 2:5], MARGIN = 2, FUN = normVector)
  # fssn_melt <- melt(flx_summer_summary_norm, id.vars = c("year"))
  # 
  # ggplot(data = fssn_melt, aes(x = year, y = value, color = variable)) +
  #   geom_hline(yintercept = 0, linetype = "dashed") +
  #   geom_line(size = 1) + geom_point() +
  #   scale_color_manual(values = c("forestgreen", "blue", "red", "orange")) +
  #   theme_bw()
}