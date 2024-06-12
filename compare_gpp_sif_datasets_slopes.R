# compare GPP datasets

library(tidyverse)
library(reshape2)
library(ggpubr)
library(ggpmisc)
library(ggrepel)
library(shadowtext)

`%notin%` <- Negate(`%in%`)
# detrendGPP function
library(trend)
library(zyp)

detrendGPP <- function(gpp_vec, yr_vec){
  # Needs library(trend) and library(zyp)
  # Usual inputs example
  # detrendGPP(gpp_sub_spring_sum[i,j,], unique(yr_sub))
  
  # If less than half the values are NaN, then process, otherwise skip and do nothing
  # Change if trend is on less than 5 points, don't detrend
  #if (length(which(is.na(gpp_vec)))/length(gpp_vec) < 0.5){
  if (length(gpp_vec) - length(which(is.na(gpp_vec))) >= 5){ # Needs at least 5 points to detrend!
    
    # list of indexes that are NOT NaN
    good_inds <- which(!is.na(gpp_vec))
    
    mktest_result <- trend::mk.test(gpp_vec[good_inds]) # do mann kendall test for trend over time for gpp, values that are NOT NaN
    
    # needed to fix detrend setting, otherwise NaN occurs for p.value if they all have the same value
    if (!is.na(mktest_result$p.value)){
      
      if (mktest_result$p.value < 0.05){
        x <- yr_vec[good_inds] # x is years
        y <- gpp_vec[good_inds] # y is gpp
        zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
        # subtract zyp.sen trend from original GPP values to get detrended version of GPP
        # add mean to put values in correct detrended order of magnitude
        gpp_vec[good_inds] <- gpp_vec[good_inds] - (zypsen_form$coefficients[2]*x + zypsen_form$coefficients[1]) + mean(gpp_vec[good_inds])
      }
      
    }
  }
  return(gpp_vec)
}

# Now includes slope se!
getSlopeIntInfo <- function(gpp_vec, met_vec){
  # if it's all NA or NaN or less than 2 elements not NA, don't calculate
  if (all(is.na(gpp_vec)) || all(is.na(met_vec)) ||
      length(which(!is.na(gpp_vec))) < 2 ||
      length(which(!is.na(met_vec))) < 2){
    slope_val <- NA
    int_val <- NA
    r2_val <- NA
    p_val <- NA
    slope_se <- NA
  } else {
    lm_summary <- summary(lm(gpp_vec ~ met_vec))
    r2_val <- lm_summary$r.squared
    # if there are not two rows of coeffs, then it failed
    if (nrow(lm_summary$coefficients) == 2) {
      slope_val <- lm_summary$coefficients[2,1]
      int_val <- lm_summary$coefficients[1,1]
      p_val <- lm_summary$coefficients[2,4]
      slope_se <- lm_summary$coefficients[2,2] # new: added slope standard error
    } else {
      slope_val <- NA
      int_val <- NA
      p_val <- NA
      slope_se <- NA
    }
  }
  return(c(slope_val, int_val, r2_val, p_val, slope_se))
}

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

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source_error_1992_2016_detrend/")
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
test_sub <- subset(all_gpp_sif_merged, subset = SITE_ID %in% c(site_name) & Year %in% 1992:2016) # test site and year range limit
test_sub <- melt(test_sub, id.vars = c("SITE_ID", "Year", "Season"))
test_sub_gpp <- subset(test_sub, subset = variable %notin% c("GOSIF_SIF", "CSIF_clear_daily"))

ggplot(test_sub_gpp, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(vars(Season), nrow = 3) + #, scales = "free_y") +
  labs(title = site_name) +
  theme_classic()

#####
# Get met vars and gpp together
trendy_seasonal_gpp_met <- subset(trendy_seasonal, subset = ModelName != "SDGVM", select = c("SITE_ID", "Year", "Season", "ModelName", "Model_GPP",
                                                                                             "CABLE_loc_precip",    "CABLE_loc_tas",      "CABLE_loc_PAR")) # will use CABLE loc variables for consistency, otherwise the precise met variable value changes
trendy_seasonal_gpp_met_wide <- dcast(trendy_seasonal_gpp_met, SITE_ID + Year + Season + CABLE_loc_precip + CABLE_loc_tas + CABLE_loc_PAR ~ ModelName, value.var = "Model_GPP") # opposite of melt fn
trendy_seasonal_gpp_met_wide$TRENDY_model_mean <- rowMeans(trendy_seasonal_gpp_met_wide[,7:20]) # get mean GPP by row (across all models)
all_gpp_sif_met_merged <- merge(gpp_sif_merged, trendy_seasonal_gpp_met_wide, all=TRUE) # merge together and keep everything

# subset to just be 1992-2016
# 1992 at lower end is when EC era begins
# 2016 at upper end is TRENDY v6 CABLE met data ends
# can extend this if we decide to use Terraclimate met data instead...
all_gpp_sif_met_merged_92_16 <- subset(all_gpp_sif_met_merged, Year %in% 1992:2016 & SITE_ID != "IT-Ro1")

# remelt GPP and SIF columns
all_gpp_sif_met_merged_92_16_melt <- melt(all_gpp_sif_met_merged_92_16, id.vars = c("SITE_ID", "Year", "Season", "CABLE_loc_precip", "CABLE_loc_tas", "CABLE_loc_PAR"), variable.name = "ModelName", value.name = "GPP_or_SIF")

# Detrend GPP and SIF for each model, then calculate slope for each met_var
detrend_combos <- expand.grid(unique(all_gpp_sif_met_merged_92_16_melt$SITE_ID), 
                              unique(all_gpp_sif_met_merged_92_16_melt$ModelName), 
                              unique(all_gpp_sif_met_merged_92_16_melt$Season))
colnames(detrend_combos) <- c("SITE_ID", "ModelName", "Season")
all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF_detrend <- NA

slope_cols <- c("Precip_Slope", "Precip_Intercept", "Precip_R2", "Precip_pval", "Precip_Slope_SE",
  "Ta_Slope", "Ta_Intercept", "Ta_R2", "Ta_pval", "Ta_Slope_SE",
  "PAR_Slope", "PAR_Intercept", "PAR_R2", "PAR_pval", "PAR_Slope_SE")
slope_init <- data.frame(matrix(data = NA, nrow = nrow(detrend_combos), ncol = length(slope_cols)))
colnames(slope_init) <- slope_cols

model_site_slopes <- cbind.data.frame(detrend_combos, slope_init)
#options(warn = 1) # display warnings as they occur
options(warn = 0) # default, suppress warnings until external function completes
#options(warn = 2) # treat warnings as errors and break

# IT-Ro1 is bad GPP data from MODIS_Terra_GPP and MODIS_Aqua_GPP, no idea why
# the site is not in the core 10+ years eddy covariance site list so we can remove it, but it's still weird that it's missing
# will subset the data later to be just the 61 core sites for spring and few extras for summer and fall

for (i in 1:nrow(detrend_combos)){
  print(i)
  site_id <- detrend_combos$SITE_ID[i]
  modelname <- detrend_combos$ModelName[i]
  s <- detrend_combos$Season[i]
  sub_inds <- which(all_gpp_sif_met_merged_92_16_melt$SITE_ID == site_id & all_gpp_sif_met_merged_92_16_melt$ModelName == modelname & all_gpp_sif_met_merged_92_16_melt$Season == s)
  new_vec <- detrendGPP(all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF[sub_inds], all_gpp_sif_met_merged_92_16_melt$Year[sub_inds])
  all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF_detrend[sub_inds] <- new_vec
  
  # Now calculate the slopes
  #[slope_val, int_val, r2_val, p_val, slope_se] <- getSlopeIntInfo(gpp_vec, met_vec)
  
  # GPP ~ Precip
  gpp_precip <- getSlopeIntInfo(all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF_detrend[sub_inds],
                  all_gpp_sif_met_merged_92_16_melt$CABLE_loc_precip[sub_inds])
  
  # GPP ~ Tas
  gpp_tas <- getSlopeIntInfo(all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF_detrend[sub_inds],
                  all_gpp_sif_met_merged_92_16_melt$CABLE_loc_tas[sub_inds])
  
  # GPP ~ PAR
  gpp_par <- getSlopeIntInfo(all_gpp_sif_met_merged_92_16_melt$GPP_or_SIF_detrend[sub_inds],
                  all_gpp_sif_met_merged_92_16_melt$CABLE_loc_PAR[sub_inds])
  
  # fill rest of model_site_slopes with outputted slope information at each row
  model_site_slopes[i, 4:ncol(model_site_slopes)] <- c(gpp_precip, gpp_tas, gpp_par)
  
  # need to re-do for MLM versions as well
}

#####
# will likely put this part as another script

flx_site_info <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_10springyrs.csv")
# terraclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars_aridity <- read.csv("flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

# Read in list of EC sites and sensitivities
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

flx_site_info_tc <- merge(flx_site_info, flx_tc_vars_aridity, by = "SITE_ID")
flx_site_info_tc_sub <- subset(flx_site_info_tc, select = c(SITE_ID, IGBP, TC_aridity))

model_site_slopes_tc_spring <- merge(flx_site_info_tc_sub, subset(model_site_slopes, subset = Season == "Spring"), by = "SITE_ID")

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# igbp color scheme
igbp_names <- c("ENF", "EBF", "DBF", "MF", 
                "CSH", "OSH", "SAV", "WSA", "GRA",  
                "WET")
igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
                 "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
                 "#2a6495")

model_site_slopes_tc_spring_gpp <- subset(model_site_slopes_tc_spring, ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))

p_spring_slope_log10x <- ggplot(data = model_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Precip_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.9) +
  #coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  #coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "IGBP") +#,
      # title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  facet_wrap(~ModelName, ncol = 4) + #, scales = "free_y") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_log10x


arid_threshold <- 0.65
model_site_slopes_tc_spring_gpp$threshold <- "Energy-limited (Wet)"
model_site_slopes_tc_spring_gpp$threshold[which(model_site_slopes_tc_spring_gpp$TC_aridity < arid_threshold)] <- "Water-limited (Dry)"
ggplot(model_site_slopes_tc_spring_gpp, aes(x = ModelName, y = Precip_Slope, fill = threshold)) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  geom_boxplot(outlier.size = 0.2) +
  scale_fill_manual(values = c("#969696", "#f0f0f0")) +
  theme(legend.position = c(0.1, 0.9), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(model_site_slopes_tc_spring_gpp, aes(x = ModelName, y = Precip_Slope, fill = threshold)) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  geom_boxplot(outlier.size = 0.2) +
  scale_fill_manual(values = c("#969696", "#f0f0f0")) +
  coord_flip() +
  theme(legend.position = c(0.8, 0.1), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))

#
# subset flx_site_sens to prep ec site data to be able to merged in to the model_site_slopes
flx_site_sens_sub <- subset(flx_site_sens, subset = SITE_ID %in% flx_site_info$SITE_ID, select = c(SITE_ID, Met_Var, Season, Slope, pvalue, PearsonR, Slope_SE))
flx_site_sens_sub$PearsonR <- flx_site_sens_sub$PearsonR^2 # square this for R2
flx_site_sens_sub_precip <- subset(flx_site_sens_sub, subset = Met_Var == "P_F_sum")
flx_site_sens_sub_ta <- subset(flx_site_sens_sub, subset = Met_Var == "TA_F_mean")
flx_site_sens_sub_par <- subset(flx_site_sens_sub, subset = Met_Var == "PAR_F_sum")

colnames(flx_site_sens_sub_precip) <- c("SITE_ID", "ModelName", "Season", "Precip_Slope", "Precip_pval", "Precip_R2", "Precip_Slope_SE")
colnames(flx_site_sens_sub_ta) <- c("SITE_ID", "ModelName", "Season", "Ta_Slope", "Ta_pval", "Ta_R2", "Ta_Slope_SE")
colnames(flx_site_sens_sub_par) <- c("SITE_ID", "ModelName", "Season", "PAR_Slope", "PAR_pval", "PAR_R2", "PAR_Slope_SE")

flx_site_sens_sub_precip$ModelName <- "EC"
flx_site_sens_sub_ta$ModelName <- "EC"
flx_site_sens_sub_par$ModelName <- "EC"

flx_site_sens_sub_wide <- merge(flx_site_sens_sub_precip, flx_site_sens_sub_ta) %>% merge(flx_site_sens_sub_par)

model_ec_site_slopes <- bind_rows(model_site_slopes, flx_site_sens_sub_wide) # rbind but lets EC columns for intercepts be NA
model_ec_site_slopes_tc_spring <- merge(flx_site_info_tc_sub, subset(model_ec_site_slopes, subset = Season == "Spring"), by = "SITE_ID")
model_ec_site_slopes_tc_spring_gpp <- subset(model_ec_site_slopes_tc_spring, ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))

p_spring_slope_log10x <- ggplot(data = model_ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Precip_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.9) +
  #coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  #coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "IGBP") +#,
  # title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  facet_wrap(~ModelName, ncol = 4) + #, scales = "free_y") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_log10x


# Wanted to have EC data overlaid on every single subplot...
ec_site_slopes_tc_spring_gpp <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName == "EC")
colnames(ec_site_slopes_tc_spring_gpp)[4] <- "EC" # need to change this name so it will overlay on everything!
a_level <- 1
point_size <- 0.75
line_size <- 0.5
ec_allmodel_precip_compare <- ggplot(data = model_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_point(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), color = "gray80", alpha = a_level, size = point_size) +
  geom_linerange(data = ec_site_slopes_tc_spring_gpp, aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), color = "gray80",  alpha = a_level, size = line_size) +
  geom_smooth(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), method = "lm", color = "gray75", se = FALSE, size = line_size) +
  geom_point(color = "gray20",  alpha = a_level, size = point_size) +
  geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), color = "gray20",  alpha = a_level, size = line_size) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = point_size) +
  scale_x_continuous(trans = "log10") +
  facet_wrap(~ModelName, ncol = 5) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_blank(), strip.text = element_text(size = 7),
    axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7))
ec_allmodel_precip_compare
# need to output a nice version of this for a bonus figure
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
#ggsave("ec_model_precip_spring_slope_compare.eps", ec_allmodel_precip_compare, width = 180, height = 150, units = "mm", dpi = 300)
# Need to figure out out to change the names of each of the panels, didn't work by the expected new faceting column...
# Maybe should have equations on this? Or do each equation manually in Illustrator or something?


# try smaller version without ALL of the TRENDY versions
gpp_list <- c("MODIS_Terra_GPP", "MODIS_Aqua_GPP", "FLUXCOM_RS_METEO_ERA5_GPP", "FLUXCOM_RS_V006_GPP", "GOSIF_GPP", "TRENDY_model_mean")
ec_allmodel_precip_compare_limited <- ggplot(data = subset(model_site_slopes_tc_spring_gpp, subset = ModelName %in% gpp_list), 
       aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_point(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), color = "gray80", alpha = a_level, size = point_size) +
  geom_linerange(data = ec_site_slopes_tc_spring_gpp, aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), color = "gray80",  alpha = a_level, size = line_size) +
  geom_smooth(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), method = "lm", color = "gray75", se = FALSE, size = line_size) +
  geom_point(color = "gray20",  alpha = a_level, size = point_size) +
  geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), color = "gray20",  alpha = a_level, size = line_size) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = point_size) +
  scale_x_continuous(trans = "log10") +
  facet_wrap(~ModelName, ncol = 3) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7))
ec_allmodel_precip_compare_limited 
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
#ggsave("ec_model_precip_spring_slope_compare_limited.eps", ec_allmodel_precip_compare_limited, width = 180, height = 120, units = "mm", dpi = 300)
# Putting equations on plots?


# Do a version that's a straight comparison between Eddy covariance and the trendy model mean data, try density plots like SW suggested
a_level <- 1
point_size <- 1.5
line_size <- 0.5
yrange <- c(-1.7, 2.9)
# need to set x range values too!
ec_allmodel_precip_compare_ec_trendy <- ggplot(data = subset(model_site_slopes_tc_spring_gpp, subset = ModelName %in% c("TRENDY_model_mean")), 
                                             aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_linerange(data = ec_site_slopes_tc_spring_gpp, aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), color = "gray80",  alpha = a_level, size = line_size) +
  geom_point(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), color = "gray80", alpha = a_level, size = point_size) +
  geom_smooth(data = ec_site_slopes_tc_spring_gpp, aes(x = TC_aridity, y = Precip_Slope), method = "lm", color = "gray75", se = FALSE, size = line_size) +
  geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), size = line_size, color = "gray20") + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = line_size) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  coord_cartesian(ylim = yrange) +
  #guides(fill = guide_legend(nrow = 5)) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7), axis.ticks = element_line(color = "black"),
        legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec_trendy
#ggsave("ec_model_precip_spring_slope_compare_ec_trendy.eps", ec_allmodel_precip_compare_ec_trendy, width = 89, height = 80, units = "mm", dpi = 300)

# And EC only
ec_allmodel_precip_compare_ec <- ggplot(data = ec_site_slopes_tc_spring_gpp, 
                                               aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), size = line_size, color = "gray20") + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = line_size) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec
#ggsave("ec_model_precip_spring_slope_compare_ec.eps", ec_allmodel_precip_compare_ec, width = 89, height = 80, units = "mm", dpi = 300)
#ggsave("ec_model_precip_spring_slope_compare_ec_se.eps", ec_allmodel_precip_compare_ec, width = 89, height = 80, units = "mm", dpi = 300)

# version with site names
ec_allmodel_precip_compare_ec_sitenames <- ggplot(data = ec_site_slopes_tc_spring_gpp, 
                                        aes(x = TC_aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  #geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), size = line_size, color = "gray20") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21) +
  geom_shadowtext(aes(color = IGBP, label = SITE_ID, bg.color="gray20", bg.r=0.1), size = 5/.pt, fontface = "bold") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = line_size) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec_sitenames

# EC regression equation, type on manually
summary(lm(ec_site_slopes_tc_spring_gpp$Precip_Slope ~ log10(ec_site_slopes_tc_spring_gpp$TC_aridity)))
# y = -1.26*log10(x) - 0.08, R2 = 0.47, p < 0.001

# TRENDY regression equation, type on manually
trendy_model_mean <- subset(model_site_slopes_tc_spring_gpp, subset = ModelName %in% c("TRENDY_model_mean"))
summary(lm(trendy_model_mean$Precip_Slope ~ log10(trendy_model_mean$TC_aridity)))
# y = -0.90*log10(x) + 0.07, R2 = 0.55, p < 0.001

# compare density plots, needs height fixed to match up
ec_allmodel_precip_compare_ec_trendy_density <- ggplot(data = subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName %in% c("EC", "TRENDY_model_mean")), 
                                               aes(y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = median(model_ec_site_slopes_tc_spring_gpp$Precip_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]), linetype = "dotted", color = "gray80") +
  geom_hline(yintercept = median(model_ec_site_slopes_tc_spring_gpp$Precip_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "TRENDY_model_mean")]), linetype = "dotted", color = "gray20") +
  geom_density(aes(color = ModelName)) + 
  #scale_x_continuous(trans = "log10") +
  scale_color_manual(values = c("gray80", "gray20"), guide = "none") +
  coord_cartesian(ylim = yrange) +
  #guides(fill = guide_legend(nrow = 5)) +
  labs(x = "Density", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec_trendy_density
#ggsave("ec_model_precip_spring_slope_compare_ec_trendy_density.eps", ec_allmodel_precip_compare_ec_trendy_density, width = 89, height = 80, units = "mm", dpi = 300)

# All 3 in a single plot
fig1_arrange <- ggarrange(ec_allmodel_precip_compare_ec, ec_allmodel_precip_compare_ec_trendy, ec_allmodel_precip_compare_ec_trendy_density,
                          widths = c(3, 3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("ec_model_precip_spring_slope_compare_arranged.eps", fig1_arrange, width = 180, height = 75, units = "mm", dpi = 300)

# make version of plot that excludes TRENDY comparison
# Needs new density plot
ec_only_density <- ggplot(data = subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName %in% c("EC")), 
                                                       aes(y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = median(model_ec_site_slopes_tc_spring_gpp$Precip_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]), linetype = "dotted", color = "gray20") +
  geom_density(color = "gray20") + 
  coord_cartesian(ylim = yrange) +
  labs(x = "Density", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_only_density
ggsave("ec_model_precip_spring_slope_compare_ec_only_density.eps", ec_only_density, width = 89, height = 80, units = "mm", dpi = 300)

fig1_arrange_ec <- ggarrange(ec_allmodel_precip_compare_ec, ec_only_density,
                          widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("ec_only_precip_spring_slope_se_density_arranged.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)

#with sitenames
fig1_arrange_ec_sitenames <- ggarrange(ec_allmodel_precip_compare_ec_sitenames, ec_only_density,
                             widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("ec_only_precip_spring_slope_se_density_arranged_sitenames.eps", fig1_arrange_ec_sitenames, width = 120, height = 75, units = "mm", dpi = 300)

# need to compare slopes and sensitivities and figure out best way to organize plotting
# Seems like a lot of the MODIS based GPP models get the wet end of the gradent, but they actually don't get the sensitivity on the DRY end
# this could change framing in the paper to be something looking at the gradient altogether, and talk about the sensitivity on wet, energy-limited side in a model-oriented capacity
# And then discussion of different models limits Josh's concern that it's just a spatial scaling thing, because LPJ is way way more sensitive than anything else
# need to think about which versions of these figures to include and how to talk about this in the paper....

# try for arid_threshold of 0.65 (default), 0.5. and 1
arid_threshold <- 1
model_ec_site_slopes_tc_spring$threshold <- "Energy-limited (Wet)"
model_ec_site_slopes_tc_spring$threshold[which(model_ec_site_slopes_tc_spring$TC_aridity < arid_threshold)] <- "Water-limited (Dry)"

se <- function(x){sqrt(var(x)/length(x))}

agg_models <- aggregate(model_ec_site_slopes_tc_spring$Precip_Slope, 
          by = list(model_ec_site_slopes_tc_spring$ModelName, model_ec_site_slopes_tc_spring$threshold),
          FUN = mean)
agg_models_se <- aggregate(model_ec_site_slopes_tc_spring$Precip_Slope, 
                        by = list(model_ec_site_slopes_tc_spring$ModelName, model_ec_site_slopes_tc_spring$threshold),
                        FUN = se)

colnames(agg_models) <- c("ModelName", "threshold", "Precip_Slope")
agg_models2 <- cbind.data.frame(agg_models$ModelName[1:23], agg_models$Precip_Slope[1:23], agg_models$Precip_Slope[24:46])
colnames(agg_models2) <- c("ModelName", "Precip_Slope_Wet", "Precip_Slope_Dry")

colnames(agg_models_se) <- c("ModelName", "threshold", "Precip_Slope_se")
agg_models_se2 <- cbind.data.frame(agg_models_se$ModelName[1:23], agg_models_se$Precip_Slope[1:23], agg_models_se$Precip_Slope[24:46])
colnames(agg_models_se2) <- c("ModelName", "Precip_Slope_se_Wet", "Precip_Slope_se_Dry")

library(ggrepel)
library(ggtext)

agg_models_merge2 <- merge(agg_models2, agg_models_se2)

# SIF are different units and shoudn't be included
agg_models3 <- subset(agg_models_merge2, subset = ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))
agg_models3$ModelRef <- LETTERS[1:nrow(agg_models3)]
agg_models3$ModelName2 <- agg_models3$ModelName
agg_models3 <- mutate(agg_models3, ModelName2=recode(ModelName2, 
                                                     FLUXCOM_RS_METEO_ERA5_GPP="FC RS METEO",
                                                     FLUXCOM_RS_V006_GPP="FC RS",
                                                     GOSIF_GPP="GOSIF",
                                                     MODIS_Aqua_GPP="MODIS-Aqua",
                                                     MODIS_Terra_GPP="MODIS-Terra",
                                                     TRENDY_model_mean="TRENDY Mean",
                                                     `ORCHIDEE-MICT`="OR.-MICT"))
ec_ind <- which(agg_models3$ModelName == "EC") # get ec row index for rectange plotting
wet_dry_compare <- ggplot(agg_models3, aes(x = Precip_Slope_Wet, y = Precip_Slope_Dry)) +
  #geom_rect(aes(x = Precip_Slope_Wet[ec_ind], y = Precip_Slope_Dry[ec_ind], width = Precip_Slope_se_Wet*2), color = "gray95") +
  annotate("rect", 
           xmin = agg_models3$Precip_Slope_Wet[ec_ind] - agg_models3$Precip_Slope_se_Wet[ec_ind], 
           xmax = agg_models3$Precip_Slope_Wet[ec_ind] + agg_models3$Precip_Slope_se_Wet[ec_ind], 
           ymin = -4, 
           ymax = 4,
           #alpha = .1,
           fill = "gray95") +
  annotate("rect", 
           xmin = -4, 
           xmax = 4, 
           ymin = agg_models3$Precip_Slope_Dry[ec_ind] - agg_models3$Precip_Slope_se_Dry[ec_ind], 
           ymax = agg_models3$Precip_Slope_Dry[ec_ind] + agg_models3$Precip_Slope_se_Dry[ec_ind],
           #alpha = .1,
           fill = "gray95") +
  geom_hline(yintercept = 0, linetype  = "dashed", size = 0.5) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_linerange(aes(ymin = Precip_Slope_Dry - Precip_Slope_se_Dry, ymax = Precip_Slope_Dry + Precip_Slope_se_Dry), color = "gray80", size = 0.25) +
  geom_linerange(aes(xmin = Precip_Slope_Wet - Precip_Slope_se_Wet, xmax = Precip_Slope_Wet + Precip_Slope_se_Wet), color = "gray80", size = 0.25) +
  geom_point(size = 0.5) +
  #geom_text(aes(label = ModelRef), size = 5/.pt) + 
  # geom_text_repel(aes(label = ModelName2), size = 5/.pt,
  #                 color = "gray30", max.overlaps = 15, bg.color = "white", bg.r = 0.05,
  #                 segment.color = "gray30", segment.size = 0.1, fontface = "bold") +
  #coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-0.2, 1.8)) +
  coord_fixed(ratio = agg_models3$Precip_Slope_se_Wet[ec_ind]/agg_models3$Precip_Slope_se_Dry[ec_ind], xlim = c(-0.4, 0.4), ylim = c(-0.2, 1.8)) + 
  labs(x = bquote("Energy-limited (Wet), GPP ~ Precip slope [g C" ~m^-2*" / mm]"), y = bquote("Water-limited (Dry), GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7))
wet_dry_compare
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
# 0.65
#ggsave("gpp_model_wet_dry_compare.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300)
#ggsave("gpp_model_wet_dry_compare_clean.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300) # version without text
# adding text in Illustrator instead

#ggsave("gpp_model_wet_dry_compare_050.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300)
#ggsave("gpp_model_wet_dry_compare_050_clean.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300) # version without text

#ggsave("gpp_model_wet_dry_compare_100.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300)
ggsave("gpp_model_wet_dry_compare_100_clean.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300) # version without text
# adding text in Illustrator instead


#####
# Will want to add this to another script.
# Need to show that the aridity threshold chosen doesn't matter that much for conclusions about energy vs. water limited takeaways for EC sites
# range of values, seq(0.5, 1, 0.05)

# Spring data:
# ec_site_slopes_tc_spring

thresh_range <- seq(0.5, 1, 0.05)
for (i in 1:length(thresh_range)){
  thresh <- thresh_range[i]
  wlim_sub <- subset(ec_site_slopes_tc_spring_gpp, subset = TC_aridity < thresh, select = c(SITE_ID, IGBP, TC_aridity, Precip_Slope))
  elim_sub <- subset(ec_site_slopes_tc_spring_gpp, subset = TC_aridity >= thresh, select = c(SITE_ID, IGBP, TC_aridity, Precip_Slope),)
  wlim_sub$lim_label <- "Water-limited"
  elim_sub$lim_label <- "Energy-limited"
  # number positive or negative?
  thresh_out_sub <- bind_rows(wlim_sub, elim_sub)
  thresh_out_sub$threshold <- thresh
  if(i == 1){thresh_out <- thresh_out_sub}
  else{thresh_out <- bind_rows(thresh_out, thresh_out_sub)}
}

#thresh_out_test <- subset(thresh_out, subset = threshold == 0.65 & lim_label == "Energy_lim")
thresh_compare_plot <- ggplot(data = thresh_out, aes(x = as.factor(threshold), y = Precip_Slope, fill = lim_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
  geom_boxplot(outlier.size = 0.25, lwd = 0.25) +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365")) +
  labs(x = "Energy- vs. Water-limited Wetness Index Threshold", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "Limitation") +
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 5),
        legend.key.size = unit(0.5,"line"), legend.position = "bottom")
thresh_compare_plot
ggsave("aridity_threshold_compare.eps", thresh_compare_plot, width = 89, height = 70, units = "mm", dpi = 300) # version without text
# energy limited sensitivity doesn't move hardly at all
# water limited sensitivity declines as wetness index increases, as expected, because so few truly dry sites.
# More compact way to show this?

# plot the averages of these or the lengths positive or negative or something else?

# How to incorporate/evaluate the SIF products since they are in different units ( not gpp ) ?

