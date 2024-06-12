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

flx_slope_merged_10 <- subset(flx_slope_merged, subset = Npts >= 10)

# Spring
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Spring" & TC_aridity > 0.65 & Slope < 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Spring" & TC_aridity > 0.65)
# EL sites < 0: 38/46 = 83%
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Spring" & TC_aridity < 0.65 & Slope > 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Spring" & TC_aridity < 0.65)
# WL sites > 0: 12/15 = 80%

# Summer
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Summer" & TC_aridity > 0.65 & Slope < 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Summer" & TC_aridity > 0.65)
# EL sites < 0: 13/46 = 83%
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Summer" & TC_aridity < 0.65 & Slope > 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Summer" & TC_aridity < 0.65)
# WL sites > 0: 15/16 = 94%

# Fall
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Fall" & TC_aridity > 0.65 & Slope < 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Fall" & TC_aridity > 0.65)
# EL sites < 0: 25/46 = 83%
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Fall" & TC_aridity < 0.65 & Slope > 0)
x <- subset(flx_slope_merged_10, subset = Met_Var == "P_F_sum" & Season == "Fall" & TC_aridity < 0.65)
# WL sites > 0: 15/16 = 94%