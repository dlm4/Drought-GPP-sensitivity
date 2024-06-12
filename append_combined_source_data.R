
#####
library(tidyverse)
library(reshape2)
`%notin%` <- Negate(`%in%`) # %notin% function

setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")
flx_site_info <- read_csv("combined_source_site_list_info_gpp_ignore_years_cru_aridity_fixedsource.csv")

# setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/plots/flux_site_summary_energy_swc1_spring/")
# file_list <- list.files(pattern = "*flx_spring_summary.csv", recursive = T)

getSourcePath <- function(x, site_id){
  if (x == "FLUXNET2015"){
    sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/plots"
    # alternate path for sites that were additional down to 5 years min
    if (site_id == "US-Me3" ){
      sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/plots/added_sites_5_6years"
    }
  } else if (x == "ICOS-2020"){
    sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter/plots"
  } else if (x == "ICOS-2018"){
    sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2018_drought_archive/plots"
  } else if (x == "OneFlux") {
    sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/ameriflux/oneflux_beta/plots"
  # } else if (x == "OneFlux_FLUXNET2015") {
  #   sourcepath <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/ameriflux/oneflux_beta/onefluxbeta_extend_fluxnet2015/plots"
    # no longer doing this substitution, will just be OneFlux only for these specific sites
  } else {
    simpleError(message = "Flux data source unknown, please correct")
  }
  return(sourcepath)
}

# repeat for each season
#season <- "spring"
#season <- "summer"
season <- "fall"

for (i in 1:nrow(flx_site_info)){
  
  sourcepath <- paste(getSourcePath(flx_site_info$SOURCE[i], flx_site_info$SITE_ID[i]), "/flux_site_summary_energy_swc1_", season, "/", flx_site_info$SITE_ID[i], "/", flx_site_info$SITE_ID[i], "_flx_", season, "_summary.csv", sep = "")
  
  if (i == 1){
    df_season <- read.csv(sourcepath, header = T)
    df_season$SITE_ID <- flx_site_info$SITE_ID[i] #unlist(strsplit(file_list[1], "/"))[1]
  } else {
    new_df <- read.csv(sourcepath, header = T)
    new_df$SITE_ID <- flx_site_info$SITE_ID[i] #unlist(strsplit(file_list[i], "/"))[1]
    df_season <- rbind.data.frame(df_season, new_df)
  }
}

#df_season <- subset(df_season, SITE_ID %notin% ignore_sites)

#setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/")
#site_info <- read.csv("fluxnet2015_sites_subset_list1_sorted_info_edited.csv")
#df_season <- merge(df_season, site_info, by = "SITE_ID")

# Read ignore years column and append
# flx_source_info now has the ignore years built-in
#gpp_ignore_years <- read.csv("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/ec_gpp_spring_summer_fall_sensitivity_gpp_ignore_years.csv")
#colnames(gpp_ignore_years)[1] <- "SITE_ID"
df_season$keep_year <- TRUE
for (site in unique(df_season$SITE_ID)){
  ignore_years_vec <- eval(parse(text = flx_site_info$SPRING[which(flx_site_info$SITE_ID == site)]))
  if (length(ignore_years_vec) > 0){
    df_season$keep_year[which(df_season$SITE_ID == site & df_season$year %in% ignore_years_vec)] <- FALSE
  }
}

# remove low or missing NEE
df_season$keep_year[df_season$NEE_VUT_REF_QC_mean < 0.5 | is.na(df_season$NEE_VUT_REF_QC_mean)] <- FALSE

write.csv(df_season, paste("processed_data/combined_source_", season, "_info_swc1.csv", sep = ""), row.names = F)

#####
# Combine all outputted season files

# COMBINE ALL #

# READ in
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter")
df_spring <- read.csv("processed_data/combined_source_spring_info_swc1.csv")
df_summer <- read.csv("processed_data/combined_source_summer_info_swc1.csv")
df_fall <- read.csv("processed_data/combined_source_fall_info_swc1.csv")

# ADD season column
df_spring$season <- "Spring"
df_summer$season <- "Summer"
df_fall$season <- "Fall"

# # FILL summer and fall P_jfmam columns with NA
# df_summer$P_F_jfmam_sum <- NA
# df_summer$P_F_QC_jfmam_mean <- NA
# df_summer$P_ERA_jfmam_sum <- NA
# 
# df_fall$P_F_jfmam_sum <- NA
# df_fall$P_F_QC_jfmam_mean <- NA
# df_fall$P_ERA_jfmam_sum <- NA

# RBIND with bind_rows from dplyr, able to handle different ordering of columns
df_all <- bind_rows(df_spring, df_summer, df_fall)

# WRITE out
write.csv(df_all, "processed_data/combined_source_info_swc1_all_seasons.csv", row.names = F)

