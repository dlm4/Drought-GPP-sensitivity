library(tidyverse)
library(reshape2)
library(trend)
library(zyp)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
`%notin%` <- Negate(`%in%`) # %notin% function

# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons_site_id_intersect_pdsi.csv")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars_aridity <- read.csv("flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

df_all<- merge(flx_tc_vars_aridity, subset(df_all, subset = season == "Spring"), by = "SITE_ID")

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


#####
# Question: In spring, is there a significant difference in detrended GPP when PDSI is negative (drought) vs when PDSI is positive (wet)?

df_all$PDSI_mean_sign <- "Negative"
df_all$PDSI_mean_sign[df_all$PDSI_mean > 0] <- "Positive"
df_all$TC_aridity_label <- "Energy_limited"
df_all$TC_aridity_label[df_all$TC_aridity < 0.65] <- "Water_limited"

df_sub <- subset(df_all, select = c(SITE_ID, TC_aridity, GPP_detrend, PDSI_mean_sign))
for (id in unique(df_sub$SITE_ID)){
  for (sign in df_sub$PDSI_mean_sign){
    which(df_sub$SITE_ID == id)
  }
}

site <- ""
x <- subset(df_sub, subset = SITE_ID == site & PDSI_mean_sign == "Positive")
y <- subset(df_sub, subset = SITE_ID == site & PDSI_mean_sign == "Negative")
wt <- wilcox.test(x$GPP_detrend, y$GPP_detrend)
wt
# Need to think about is this additional analysis worth it

ggplot(df_all) + 
  geom_boxplot(aes(x = SITE_ID, y = GPP_detrend, color = PDSI_mean_sign, fill = TC_aridity)) + 
  theme(
    axis.text = element_text(size = 7, color = "black", angle = 45, vjust = 1, hjust=1)
  )
