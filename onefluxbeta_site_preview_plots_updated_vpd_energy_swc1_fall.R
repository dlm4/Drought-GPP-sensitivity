

#####
library(tidyverse)
library(lubridate) # might not need it
library(reshape2)
`%notin%` <- Negate(`%in%`) # %notin% function

# location of FLUXNET2015 data
#setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/FLUXNET2015-latest")
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/ameriflux/oneflux_beta/allsites_FULLSET")

# Site
#site_list <- read.csv("../fluxnet2015_sites_subset_list1_sorted.csv", header = T)
site_list <- read.csv("../oneflux_beta_sitelist.csv", header = T)
site_list <- site_list[,1]

# Loop over site list
for (flx_name in site_list){
  # Need to reset the path every time
  #setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/FLUXNET2015-latest")
  setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/ameriflux/oneflux_beta/allsites_FULLSET")
  
  #flx_name <- "US-MMS"
  
  south_hem_sites <- c("AU-How", "AU-Tum", "BR-Sa1", "ZM-Mon",
                       "AU-DaP", "AU-DaS", "AU-Dry", "AU-Stp")
  
  # Load in site monthly data
  flx <- read.csv(list.files(pattern = glob2rx(paste("*", flx_name, "*FULLSET_MM*.csv", sep = "")), recursive = T)[1])
  
  flx[flx == -9999] <- NA
  
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
  setwd("../plots/flux_site_summary_energy_swc1_fall/")
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
  # FALL PLOTS
  # Add in ERA and SW_IN_F columns for completeness and make relevant plots...
  # Add energy
  # adjust month range here for fall
  flx_fall <- subset(flx, month %in% 9:11, select = c(month, year,
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
  flx_fall$ndays <- NA
  flx_fall$ndays[flx_fall$month == 9] <- 30 # sept has 30 days
  flx_fall$ndays[flx_fall$month == 10] <- 31 # oct has 31 days
  flx_fall$ndays[flx_fall$month == 11] <- 30 # nov has 30 days
  
  
  fall_years <- unique(flx_fall$year)
  # sum or mean values, and mean of QC as well
  flx_fall_summary_colnames <- c("year", 
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
  
  flx_fall_summary <- data.frame(matrix(NA, nrow = length(fall_years), ncol = length(flx_fall_summary_colnames)))
  colnames(flx_fall_summary) <- flx_fall_summary_colnames
  flx_fall_summary$year <- fall_years
  
  for (i in 1:length(fall_years)){
    yr <- fall_years[i]
    
    # GPP
    flx_fall_summary$GPP_NT_VUT_REF_sum[i] <- sum(flx_fall$GPP_NT_VUT_REF[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr])
    flx_fall_summary$NEE_VUT_REF_QC_mean[i] <- sum(flx_fall$NEE_VUT_REF_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # Precip
    flx_fall_summary$P_F_sum[i] <- sum(flx_fall$P_F[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr])
    flx_fall_summary$P_F_QC_mean[i] <- sum(flx_fall$P_F_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$P_ERA_sum[i] <- sum(flx_fall$P_ERA[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr])
    
    # TA
    flx_fall_summary$TA_F_mean[i] <- sum(flx_fall$TA_F[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$TA_F_QC_mean[i] <- sum(flx_fall$TA_F_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$TA_ERA_mean[i] <- sum(flx_fall$TA_ERA[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # PAR and SW_IN
    flx_fall_summary$PPFD_IN_mean[i] <- sum(flx_fall$PPFD_IN[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$PPFD_IN_QC_mean[i] <- sum(flx_fall$PPFD_IN_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr], na.rm = T) / (30 + 31 + 30)
    flx_fall_summary$SW_IN_F_mean[i] <- sum(flx_fall$SW_IN_F[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$SW_IN_F_QC_mean[i] <- sum(flx_fall$SW_IN_F_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$SW_IN_ERA_mean[i] <- sum(flx_fall$SW_IN_ERA[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # VPD
    flx_fall_summary$VPD_F_mean[i] <- sum(flx_fall$VPD_F[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$VPD_F_QC_mean[i] <- sum(flx_fall$VPD_F_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$VPD_ERA_mean[i] <- sum(flx_fall$VPD_ERA[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # ENERGY
    # NETRAD
    flx_fall_summary$NETRAD_mean[i] <- sum(flx_fall$NETRAD[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$NETRAD_QC_mean[i] <- sum(flx_fall$NETRAD_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # G_F_MDS
    flx_fall_summary$G_F_MDS_mean[i] <- sum(flx_fall$G_F_MDS[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$G_F_MDS_QC_mean[i] <- sum(flx_fall$G_F_MDS_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # LE
    flx_fall_summary$LE_F_MDS_mean[i] <- sum(flx_fall$LE_F_MDS[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$LE_F_MDS_QC_mean[i] <- sum(flx_fall$LE_F_MDS_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$LE_CORR_mean[i] <- sum(flx_fall$LE_CORR[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # H
    flx_fall_summary$H_F_MDS_mean[i] <- sum(flx_fall$H_F_MDS[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$H_F_MDS_QC_mean[i] <- sum(flx_fall$H_F_MDS_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    flx_fall_summary$H_CORR_mean[i] <- sum(flx_fall$H_CORR[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
    
    # SWC_F_MDS_1
    flx_fall_summary$SWC_F_MDS_1_mean[i] <- sum(flx_fall$SWC_F_MDS_1[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr])/ (30 + 31 + 30)
    flx_fall_summary$SWC_F_MDS_1_QC_mean[i] <- sum(flx_fall$SWC_F_MDS_1_QC[flx_fall$year == yr] * flx_fall$ndays[flx_fall$year == yr]) / (30 + 31 + 30)
  }
  
  # output flx_fall, monthly data without averaging
  write.csv(flx_fall, file = paste(flx_name, "_flx_fall_months.csv", sep = ""), row.names = F)
  
  # output: flx_fall_summary data frame as csv
  # flx_fall_summary_output <- flx_fall_summary
  # flx_fall_summary_output[, 2:length(flx_fall_summary_colnames)] <- round(flx_fall_summary[, 2:length(flx_fall_summary_colnames)], digits = 3)
  write.csv(flx_fall_summary, file = paste(flx_name, "_flx_fall_summary.csv", sep = ""), row.names = F)
  


  #####
  # Plotting against one another

  ##
  # Precip vs. GPP
  ggplot(data = flx_fall_summary, aes(x = P_F_sum, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_p_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = P_ERA_sum, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_p_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)


  ##
  # TA vs. GPP
  ggplot(data = flx_fall_summary, aes(x = TA_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = TA_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ##
  # PPFD or SW vs. GPP
  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ## VPD vs. GPP
  ggplot(data = flx_fall_summary, aes(x = VPD_F_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_vpd_f_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = VPD_ERA_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_vpd_era_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## NETRAD vs GPP
  ggplot(data = flx_fall_summary, aes(x = NETRAD_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_netrad_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## NETRAD - G_F_MDS vs GPP
  ggplot(data = flx_fall_summary, aes(x = NETRAD_mean-G_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_netrad_min_g_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ## LE and/or H vs GPP
  ggplot(data = flx_fall_summary, aes(x = LE_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_le_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = H_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_h_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = LE_F_MDS_mean+H_F_MDS_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_le_h_f_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = LE_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_le_corr_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = H_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_h_corr_mds_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = LE_CORR_mean+H_CORR_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_le_h_corr_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # SWC_F_MDS_1 vs GPP
  ggplot(data = flx_fall_summary, aes(x = SWC_F_MDS_1_mean, y = GPP_NT_VUT_REF_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_swc1_gpp.png", sep = ""), width = width_size, height = height_size, units = units_var)
  

  ##
  # PPFD or SW vs TA_F_mean
  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = TA_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_ta.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_F_mean, y = TA_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_f_ta.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_ERA_mean, y = TA_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_era_ta_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ##
  # PPFD or SW vs. Precip
  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  # PPFD or SW vs VPD
  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_vpd.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_F_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_f_vpd.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_ERA_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_era_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  
  
  
  ##
  # TA vs. Precip
  ggplot(data = flx_fall_summary, aes(x = TA_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = TA_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # TA vs. VPD
  ggplot(data = flx_fall_summary, aes(x = TA_F_mean, y = VPD_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_f_vpd_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = TA_ERA_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_era_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # VPD vs. Precip
  ggplot(data = flx_fall_summary, aes(x = VPD_F_mean, y = P_F_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_vpd_f_p_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = VPD_ERA_mean, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_vpd_era_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)


  #####
  # Plot similar variables against themselves for comparison, ERA values on Y if possible
  # Precip
  ggplot(data = flx_fall_summary, aes(x = P_F_sum, y = P_ERA_sum)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_p_f_p_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # TA
  ggplot(data = flx_fall_summary, aes(x = TA_F_mean, y = TA_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ta_f_ta_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # PPFD and SW
  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = SW_IN_F_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_sw_in_f.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = PPFD_IN_mean, y = SW_IN_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_ppfd_in_sw_in_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  ggplot(data = flx_fall_summary, aes(x = SW_IN_F_mean, y = SW_IN_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_sw_in_f_sw_in_era.png", sep = ""), width = width_size, height = height_size, units = units_var)

  # VPD
  ggplot(data = flx_fall_summary, aes(x = VPD_F_mean, y = VPD_ERA_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_vpd_f_vpd_era.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # LE
  ggplot(data = flx_fall_summary, aes(x = LE_F_MDS_mean, y = LE_CORR_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_le_f_mds_le_corr.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # H
  ggplot(data = flx_fall_summary, aes(x = H_F_MDS_mean, y = H_CORR_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_h_f_mds_h_corr.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  # NETRAD
  ggplot(data = flx_fall_summary, aes(x = NETRAD_mean, y = LE_F_MDS_mean+H_F_MDS_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_netrad_le_h_f_mds.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  ggplot(data = flx_fall_summary, aes(x = NETRAD_mean-G_F_MDS_mean, y = LE_F_MDS_mean+H_F_MDS_mean)) +
    geom_point() +
    geom_text(aes(label = as.character(year)), size = 3)+
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_netrad_min_g_f_mds_le_h_f_mds.png", sep = ""), width = width_size, height = height_size, units = units_var)
  
  
  
  
  #####
  # Plot by years on x, facet wrap
  flx_fall_summary_melt <- melt(flx_fall_summary, id.vars = list("year"))
  ggplot(data = flx_fall_summary_melt, aes(x = year, y = value)) +
    geom_point() + geom_line() +
    #geom_text(aes(label = as.character(year)), size = 3)+
    facet_wrap(~variable, scales = "free_y") +
    labs(title = flx_name) +
    theme_bw()
  ggsave(paste(flx_name, "_fall_summary.png", sep = ""), width = width_size*2, height = height_size*2, units = units_var)
  
  #####
  # Plot these as a grid against one another
  # fss_sub <- flx_fall_summary[, 2:5]
  # plot(fss_sub)
  
  #####
  # # Normalize and plot all together
  # flx_fall_summary_norm <- flx_fall_summary
  # # convert these to z scores to plot against each other
  # normVector <- function(x){
  #   return((x - mean(x)) / sd(x))
  # }
  # flx_fall_summary_norm[, 2:5] <- apply(flx_fall_summary[, 2:5], MARGIN = 2, FUN = normVector)
  # fssn_melt <- melt(flx_fall_summary_norm, id.vars = c("year"))
  # 
  # ggplot(data = fssn_melt, aes(x = year, y = value, color = variable)) +
  #   geom_hline(yintercept = 0, linetype = "dashed") +
  #   geom_line(size = 1) + geom_point() +
  #   scale_color_manual(values = c("forestgreen", "blue", "red", "orange")) +
  #   theme_bw()
}