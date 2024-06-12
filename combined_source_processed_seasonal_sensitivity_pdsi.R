library(tidyverse)
library(reshape2)
library(trend)
library(zyp)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
`%notin%` <- Negate(`%in%`) # %notin% function

# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons_site_id_intersect_pdsi.csv")

#####
########### Filtering, GPP detrending, and climate relabeling prep work  ##########

# Filter by keep_year column, keep only good years
df_all <- df_all[df_all$keep_year,]

# Filter by NEE QC > 0.5
df_all <- df_all[which(df_all$NEE_VUT_REF_QC_mean > 0.5),]

# Detrend GPP using Sen's slope (if needed) for each site, season
df_all$GPP_detrend <- df_all$GPP_NT_VUT_REF_sum # set GPP detrend column to original GPP column to start
df_all$detrended <- FALSE # is the GPP detrended? Set to FALSE by default then replace

# list of sites that will need to be removed because not enough data in one of the seasons
remove_site_id <- c()
remove_site_id_season <- c()

# Loop over seasons: Spring, Summer, Fall
for (seas in unique(df_all$season)){
  # Loop over sites
  for (site in unique(df_all$SITE_ID)){
    # Which for site and season
    site_inds <- which(df_all$season == seas & df_all$SITE_ID == site)
    
    if (length(site_inds) < 5){
      df_all$GPP_NT_VUT_REF_sum[site_inds] <- NA
      remove_site_id <- c(remove_site_id, site)
      remove_site_id_season <- c(remove_site_id_season, seas)
    } else {
      mktest_result <- trend::mk.test(df_all$GPP_NT_VUT_REF_sum[site_inds]) # do mann kendall test for trend over time
      
      # If trend is signficant at 0.05, then detrend GPP, otherwise do nothing
      if (mktest_result$p.value < 0.05){
        x <- df_all$year[site_inds]
        y <- df_all$GPP_NT_VUT_REF_sum[site_inds]
        zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
        # subtract zyp.sen trend from original GPP values to get detrended version of GPP
        df_all$GPP_detrend[site_inds] <- df_all$GPP_NT_VUT_REF_sum[site_inds] - (zypsen_form$coefficients[2]*df_all$year[site_inds] + zypsen_form$coefficients[1])
        df_all$detrended[site_inds] <- TRUE # specify that this site-season combination is detrended
        
        # Fix nonzero center due to sen's slope if needed for anomalies, don't need this right now
        #df_spring$anom_GPP[site_inds] <- df_spring$anom_GPP[site_inds] - mean(df_spring$anom_GPP[site_inds])
        print(paste("Detrended", site, "for", seas, sep = " "))
      }
    }
  }
}

#df_sub <- df_all[which(df_all$SITE_ID == "GL-NuF"),]

# Column for two-letter Climate label for coloring plots
#df_all$Climate2 <- substring(df_all$Climate, 1, 2) # two letter version of climate for simpler summary
#df_all$Climate2[which(df_all$Climate == "Bwh")] <- "Bs" # moving SRC from Bw (where it would be by itself) to Bs in Climate2 column

# # Convert SW_IN_F_mean into a PAR sum
# df_all$PAR_F_sum <- df_all$SW_IN_F_mean*3600*24/1000000 * 0.5 # daily PAR estimate in MJ m-2 from SW W m-2
# df_all$PAR_F_sum[which(df_all$season == "Spring")] <- df_all$PAR_F_sum[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$PAR_F_sum[which(df_all$season == "Summer")] <- df_all$PAR_F_sum[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$PAR_F_sum[which(df_all$season == "Fall")] <- df_all$PAR_F_sum[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall
# 
# # Apply energy conversion to seasonal sum for NETRAD, NETRAD - G, LE + H, and LE_CORR + H_CORR
# # At least filter with NA for NETRAD_QC
# df_all$NETRAD_sum_goodqc <- NA
# netrad_inds <- which(df_all$NETRAD_QC_mean > 0.5) # indices for where NETRAD_QC_mean is above 0.5, good data
# df_all$NETRAD_sum_goodqc[netrad_inds] <- df_all$NETRAD_mean[netrad_inds]
# 
# df_all$NETRAD_sum_goodqc <- df_all$NETRAD_sum_goodqc*3600*24/1000000
# df_all$NETRAD_sum_goodqc[which(df_all$season == "Spring")] <- df_all$NETRAD_sum_goodqc[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$NETRAD_sum_goodqc[which(df_all$season == "Summer")] <- df_all$NETRAD_sum_goodqc[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$NETRAD_sum_goodqc[which(df_all$season == "Fall")] <- df_all$NETRAD_sum_goodqc[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall
# 
# # Do same thing with G_F difference
# df_all$NETRAD_min_G_F_sum_goodqc <- NA
# netrad_G_F_inds <- which(df_all$NETRAD_QC_mean > 0.5 & df_all$G_F_MDS_QC_mean > 0.5) # indices for where NETRAD_QC_mean and G_F_MDS_QC_mean are above 0.5, good data
# df_all$NETRAD_min_G_F_sum_goodqc[netrad_G_F_inds] <- df_all$NETRAD_mean[netrad_G_F_inds] - df_all$G_F_MDS_mean[netrad_G_F_inds]
# 
# df_all$NETRAD_min_G_F_sum_goodqc <- df_all$NETRAD_min_G_F_sum_goodqc*3600*24/1000000
# df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Spring")] <- df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Summer")] <- df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Fall")] <- df_all$NETRAD_min_G_F_sum_goodqc[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall
# 
# # And LE + H
# df_all$LE_plus_H_MDS_sum_goodqc <- NA
# le_h_inds <- which(df_all$LE_F_MDS_QC_mean > 0.5 & df_all$H_F_MDS_QC_mean > 0.5) # indices for where LE_F_MDS_QC_mean and H_F_MDS_QC_mean are above 0.5, good data
# df_all$LE_plus_H_MDS_sum_goodqc[le_h_inds] <- df_all$LE_F_MDS_mean[le_h_inds] + df_all$H_F_MDS_mean[le_h_inds]
# 
# df_all$LE_plus_H_MDS_sum_goodqc <- df_all$LE_plus_H_MDS_sum_goodqc*3600*24/1000000
# df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Spring")] <- df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Summer")] <- df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Fall")] <- df_all$LE_plus_H_MDS_sum_goodqc[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall
# 
# # And LE + H corr
# df_all$LE_plus_H_corr_sum_goodqc <- NA
# df_all$LE_plus_H_corr_sum_goodqc[le_h_inds] <- df_all$LE_CORR_mean[le_h_inds] + df_all$H_CORR_mean[le_h_inds]
# 
# df_all$LE_plus_H_corr_sum_goodqc <- df_all$LE_plus_H_corr_sum_goodqc*3600*24/1000000
# df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Spring")] <- df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Summer")] <- df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Fall")] <- df_all$LE_plus_H_corr_sum_goodqc[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall
# 
# # And SWC
# df_all$SWC_F_MDS_1_mean_goodqc <- NA
# swc1_inds <- which(df_all$SWC_F_MDS_1_QC_mean > 0.5) # indices for where SWC_F_MDS_1_QC_mean is above 0.5, good data
# df_all$SWC_F_MDS_1_mean_goodqc[swc1_inds] <- df_all$SWC_F_MDS_1_mean[swc1_inds]
# 
# ###
# # Need to do Bowen Ratio and Evaporative Fraction
# # don't need to convert to MJ because these are ratios anyway
# # Both MDS and corr versions
# 
# # Bowen Ratio (H / LE), log10 scaled, produces NaN when ratio is negative due to random error in H or LE
# df_all$Bowen_MDS_goodqc <- NA
# df_all$Bowen_MDS_goodqc[le_h_inds] <- log10(df_all$H_F_MDS_mean[le_h_inds] / df_all$LE_F_MDS_mean[le_h_inds])
# 
# df_all$Bowen_corr_goodqc <- NA
# df_all$Bowen_corr_goodqc[le_h_inds] <- log10(df_all$H_CORR_mean[le_h_inds] / df_all$LE_CORR_mean[le_h_inds])
# 
# # EF = LE / (H + LE)
# df_all$EF_MDS_goodqc <- NA
# df_all$EF_MDS_goodqc[le_h_inds] <- df_all$LE_F_MDS_mean[le_h_inds] / (df_all$LE_F_MDS_mean[le_h_inds] + df_all$H_F_MDS_mean[le_h_inds]) 
# 
# df_all$EF_corr_goodqc <- NA
# df_all$EF_corr_goodqc[le_h_inds] <- df_all$LE_CORR_mean[le_h_inds] / (df_all$LE_CORR_mean[le_h_inds] + df_all$H_CORR_mean[le_h_inds])

# unique(df_all$SITE_ID[which(is.na(df_all$LOCATION_ELEV))])
# only some of the Australian sites are missing elevation
# Added in site elevation estimates from google earth: AU-DaS, AU-DaP, AU-Dry, AU-How, AU-Stp, MY-PSO

# PDSI does not have any units

#####
# Univariate regressions for each met variable, season, and site
# Median of each met variable for each site and season too
# met_vars <- c("P_F_sum", "TA_F_mean", "PAR_F_sum", "VPD_F_mean", 
#               "NETRAD_sum_goodqc", "NETRAD_min_G_F_sum_goodqc",
#               "LE_plus_H_MDS_sum_goodqc", "LE_plus_H_corr_sum_goodqc",
#               "Bowen_MDS_goodqc", "Bowen_corr_goodqc",
#               "EF_MDS_goodqc", "EF_corr_goodqc",
#               "SWC_F_MDS_1_mean_goodqc")
met_vars <- c("PDSI_mean")
num_rows <- length(met_vars)*length(unique(df_all$season))*length(unique(df_all$SITE_ID)) # number of rows

# adding in climate (three letters) and elevation data

# originally had all these data appended but will need add afterwards and filter
df_sens_colnames <- c("SITE_ID", "Met_Var", "Season", "Slope", "pvalue", "Climate", "Climate2", "IGBP", "Elevation", "Met_Var_Median", "Npts",
                      "PearsonR", "SpearmanR", "RMSE", "Slope_SE")
df_sens <- data.frame(matrix(data = NA, nrow = num_rows, ncol = length(df_sens_colnames)))
colnames(df_sens) <- df_sens_colnames

i <- 1 # indexing adding rows

# Need to do new version of this loop but doing multilinear regression for met_vars: precip, par, and air temp
# Could just call it shortwave instead of par and multiply by 2?

for (met in met_vars){
  for (seas in unique(df_all$season)){
    for (site in unique(df_all$SITE_ID)){
      
      # Variable labels etc
      df_sens$SITE_ID[i] <- site
      df_sens$Met_Var[i] <- met
      df_sens$Season[i] <- seas

      # Which for site and season
      site_inds <- which(df_all$season == seas & df_all$SITE_ID == site)
      #climate2 <- df_all$Climate2[site_inds]
      #igbp <- df_all$IGBP[site_inds]
      #climate <- df_all$Climate[site_inds]
      #elev <- df_all$LOCATION_ELEV[site_inds]
      
      #df_sens$Climate2[i] <- climate2[1]
      #df_sens$IGBP[i] <- igbp[1]
      #df_sens$Climate[i] <- climate[1]
      #df_sens$Elevation[i] <- elev[1]
      
      # Which column has the met variable
      met_col <- which(colnames(df_all) == met)
      
      # univariate linear regression
      x <- df_all[site_inds, met_col] # x value, met variable column
      y <- df_all$GPP_detrend[site_inds] # GPP or detrended GPP (if needed)
      
      # if x is empty, median is NA; can happen for NETRAD - G_F
      if (length(x) == 0){
        df_sens$Met_Var_Median[i] <- NA
      } else {
        df_sens$Met_Var_Median[i] <- median(x, na.rm = T) # median of the met val, could have NA
      }
      
      # Need to test for length of site_inds, don't do regression if there are too few points?
      x_not_NA_inds <- which(!is.na(x))
      x <- x[x_not_NA_inds] # only keep x values that aren't NA
      y <- y[x_not_NA_inds] # same for y, GPP
      df_sens$Npts[i] <- length(x) # number of points in regression ie number of years
      
      if (length(x) < 3){
        df_sens$Slope[i] <- NA
        df_sens$pvalue[i] <- NA
      } else {
        # linear model
        lm_result <- lm(y~x) # linear model
        
        df_sens$Slope[i] <- lm_result$coefficients[2] # slope
        df_sens$pvalue[i] <- summary(lm_result)$coefficients[2,4] # p value 
        
        # Correlations
        df_sens$PearsonR[i] <- cor(x, y, method = "pearson")
        df_sens$SpearmanR[i] <- cor(x, y, method = "spearman")
        
        # (added Mar 29, 2022)
        # RMSE
        df_sens$RMSE[i] <- sqrt(mean(lm_result$residuals^2))
        
        # Slope standard error
        df_sens$Slope_SE[i] <- summary(lm_result)$coefficients[2,2]
        
      }
      
      i <- i + 1
    }
  }
}
# factors for different seasons
df_sens$Season <- factor(df_sens$Season, levels = c("Spring", "Summer", "Fall"))

# Remove sites that have less than 5 years of data for any season
site_list <- unique(df_sens$SITE_ID)

# get the only three met vars we're actually using for now
#df_sens_sub <- df_sens[which(df_sens$Met_Var %in% c("P_F_sum", "TA_F_mean", "PAR_F_sum")),]
df_sens_sub <- df_sens[which(df_sens$Met_Var %in% c("PDSI_mean")),]

# Remove site slopes with too short of time series (less than 5 years) in any season
for (s in site_list){
  inds <- which(df_sens_sub$SITE_ID == s)
  if(any(df_sens_sub$Npts[inds] < 5)){
    df_sens_sub$Slope[inds] <- NA 
  }
}

df_sens_sub <- df_sens_sub[which(!is.na(df_sens_sub$Slope)),]
# output
df_sens_sub_output <- df_sens_sub[, colnames(df_sens_sub) %notin% c("Climate", "Climate2", "IGBP", "Elevation")]
write.csv(df_sens_sub_output, "combined_source_ec_univariate_slopes2_pdsi.csv", row.names = F)

# # reduce sites in the original list
# original_site_info <- read_csv("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_fixedsource.csv")
# subset_site_info <- original_site_info[which(original_site_info$SITE_ID %in% df_sens_sub_output$SITE_ID),]
# write_csv(subset_site_info, "/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
