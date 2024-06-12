# Do trendy analysis for only flux sites selected in data sets

# For spring, summer, and fall
# Get list of flux sites for precip/ta/par
#   data output from the slope calculation script
# Get locations of sites
#   match flux site names with original flux location/description file

# for each flux site, get nearest trendy grid cell from output csv per model
#   allow multiple copies of same location
#   (alt version: get 4 nearest grid cells and weighted average?)
#   will need to check if location is NA, may need to get next nearest
# load southern hem version to get Australia sites

# Do same plotting and regression checks as with the original >30 N version
# use text or points instead of hex for model specific plots

#####
library(tidyverse)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
library(sf) # for distance calculation

library(foreach)
library(doParallel)

# read in list of flux sites
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/plots/flux_site_summary_energy_swc1_sensitivity/new_aridity")
#ec_gpp_sens <- read.csv("ec_gpp_sens_P_F_sum.csv") # would need to swap this for temperature and PAR

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
ec_gpp_sens <- read.csv("combined_source_ec_univariate_slopes2.csv") # this includes everything
ec_gpp_sens <- subset(ec_gpp_sens, Met_Var == "P_F_sum")

# get original site info and locations
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/")
#flx_site_info <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/fluxnet2015/fluxnet2015_site_locations_plaintext.csv")

# New site list from combined source
flx_site_info <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
colnames(flx_site_info)[1] <- "SITE_ID" # need to make sure first column is labeled correctly
flx_site_info <- flx_site_info[order(flx_site_info$SITE_ID),] # alphabetical reorder

# Load in trendy data
# need to load and merge southern and northern hem
# will need to loop through different models
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4") # this is changed
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_allmodel_slope_error")
file_list <- list.files()

# formulas for plotting
my.formula <- y ~ x
log.formula <- y ~ log10(x)

j <- 1 # iterator

for (trendyfile in file_list){
#getTrendy <- function(file_num, ec_gpp_sens, flx_site_info, file_list, log.formula, my.formula){
#foreach (file_num = 1:length(file_list)) %do% {
  # test
  #trendyfile <- file_list[file_num]
  
  print(trendyfile)
  #trendy_slopes <- read.csv("CLM_S2_gppgppproc_met_CABLE_S2_precip_ta_par_df_spring_slope.csv")
  trendy_slopes <- read.csv(trendyfile)
  
  #setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4_southhem")
  #trendy_slopes_s <- read.csv("CLM_S2_gppgppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_southhem.csv")
  # trendy_slopes_s <- read.csv(paste("../gpp_precip_slope_v4_southhem/", unlist(strsplit(trendyfile, "[.]"))[1], "_southhem.csv", sep = ""))
  # 
  # trendy_slopes <- rbind.data.frame(trendy_slopes, trendy_slopes_s)
  # rm(trendy_slopes_s)
  
  ###
  # Load in flux site data
  # get sites for particular season
  
  # get season
  splitname <- unlist(str_split(trendyfile, "_"))
  season_name <- splitname[length(splitname)-2] # this needed to be modified to -2 from -1 to extract the season correctly
  
  # get site list based on season
  site_list <- ec_gpp_sens$SITE_ID[which(tolower(ec_gpp_sens$Season) == season_name)]
  
  # subset list
  flx_site_info_sub <- flx_site_info[which(flx_site_info$SITE_ID %in% site_list),]
  
  # backwards subset ec_gpp_sens to get climate info
  ec_gpp_sens_sub <- ec_gpp_sens[which(ec_gpp_sens$SITE_ID %in% flx_site_info_sub$SITE_ID &
                                         tolower(ec_gpp_sens$Season) == season_name),]
  
  # climate already in flx_site_info in new format
  # get climate
  #flx_site_info_sub$Climate <- ec_gpp_sens_sub$Climate
  #flx_site_info_sub$Climate2 <- ec_gpp_sens_sub$Climate2 # no climate2 in new dataset
  
  flx_site_info_sub$Distance <- NA # retain distance column
  
  ###
  
  flx_site_output <- cbind.data.frame(flx_site_info_sub, data.frame(matrix(NA, nrow = nrow(flx_site_info_sub), ncol = ncol(trendy_slopes))))
  colnames(flx_site_output) <- c(colnames(flx_site_info_sub), colnames(trendy_slopes))
  
  col_start <- ncol(flx_site_info_sub) + 1
  col_end <- ncol(flx_site_output)
  
  # only do complete slopes, no missing values! excludes NA
  trendy_slopes <- trendy_slopes[complete.cases(trendy_slopes),]
  trendy_slopes_sf <- st_as_sf(trendy_slopes, coords = c("Lon", "Lat"),
                               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  #st_is_longlat(trendy_slopes_sf)
  
  flx_site_info_sub_sf <- st_as_sf(flx_site_info_sub, coords = c("LOCATION_LONG", "LOCATION_LAT"),
                                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  #st_is_longlat(flx_site_info_sub_15N_sf)
  
  col_start <- ncol(flx_site_info_sub) + 1
  col_end <- ncol(flx_site_output)
  
  # slow but it works much much better
  for (i in 1:nrow(flx_site_info_sub)){
    # site_lat <- flx_site_info_sub_15N$LOCATION_LAT[i]
    # site_lon <- flx_site_info_sub_15N$LOCATION_LONG[i]
    # 
    # flx_site_output[i, col_start:col_end] <- trendy_slopes[which(trendy_slopes$Lat == trendy_slopes$Lat[which.min(abs(trendy_slopes$Lat - site_lat))] &
    #                                                               trendy_slopes$Lon == trendy_slopes$Lon[which.min(abs(trendy_slopes$Lon - site_lon))]),]
    print(i)
    dist <- st_distance(flx_site_info_sub_sf[i,], trendy_slopes_sf)
    flx_site_output[i, col_start - 1] <- min(dist)
    flx_site_output[i, col_start:col_end] <- trendy_slopes[which.min(dist),]
  }
  
  # # make apply version to increase speed ?
  # getMinDist <- function(list(flx_site_info_sub_sf_row, flx_site_output_row)){
  #   dist <- st_distance(flx_site_info_sub_sf_row, trendy_slopes_sf)
  #   flx_site_output_row[col_start - 1] <- min(dist)
  #   flx_site_output_row[col_start:col_end] <- trendy_slopes[which.min(dist),]
  #   return(flx_site_output_row)
  # }
  # 
  # flx_site_output <- apply(list(flx_site_info_sub_sf, flx_site_output), MARGIN = 1, FUN = getMinDist)
  
  # flx_site_output, season and modelname
  flx_site_output$Season <- season_name
  flx_site_output$ModelName <- splitname[1]
  
  # need to do some string splitting for naming
  
  # plot
  ggplot(data = flx_site_output, aes(x = Aridity, y = Precip_Slope)) +
    geom_hline(yintercept = 0) +
    geom_text(aes(label = SITE_ID, color = Climate), size = 2, fontface = "bold") +
    geom_smooth(method = "lm", formula = log.formula, color = "black") + 
    stat_poly_eq(formula = my.formula, 
                 aes(x = log10(Aridity), y = Precip_Slope, label = paste(..eq.label.., sep = "")),
                 eq.x.rhs = "log(x)",
                 output.type = "text",
                 parse = FALSE, size = 3, na.rm = TRUE,
                 label.y = 0.97, label.x = 0.97) +
    stat_poly_eq(formula = log.formula, 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE, size = 3, na.rm = TRUE,
                 label.y = 0.93, label.x = 0.97) +
    labs(x = "Aridity (P/PET)", y = "Slope GPP ~ Precip [g C m-2 / mm]",
         title = paste(splitname[1], season_name, ": GPP ~ Precip", sep = " ")) + 
    theme_bw()
  
  # save plot
  ggsave(paste("../results/v4_ec_site_locations_combined_source_error/", splitname[1], "_gpp_precip_ec_sites_", season_name, ".png", sep = ""), width = 5, height = 4, units = "in")
  
  
  # log10 scaled x plot
  ggplot(data = flx_site_output, aes(x = log10(Aridity), y = Precip_Slope)) +
    geom_hline(yintercept = 0) +
    geom_text(aes(label = SITE_ID, color = Climate), size = 2, fontface = "bold") +
    geom_smooth(method = "lm", color = "black") + # linear formula
    stat_poly_eq(formula = my.formula, 
                 aes(x = log10(Aridity), y = Precip_Slope, label = paste(..eq.label.., sep = "")),
                 #eq.x.rhs = "log(x)",
                 output.type = "text",
                 parse = FALSE, size = 3, na.rm = TRUE,
                 label.y = 0.97, label.x = 0.97) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE, size = 3, na.rm = TRUE,
                 label.y = 0.93, label.x = 0.97) +
    labs(x = "Log10 Aridity (P/PET)", y = "Slope GPP ~ Precip [g C m-2 / mm]",
         title = paste(splitname[1], season_name, ": GPP ~ Precip", sep = " ")) + 
    theme_bw()
  
  # save plot
  ggsave(paste("../results/v4_ec_site_locations_combined_source/", splitname[1], "_gpp_precip_ec_sites_", season_name, "log10aridity.png", sep = ""), width = 5, height = 4, units = "in")
  
  # write out data frame to csv
  # actually includes gpp sensitivity to precip, ta, and par
  write.csv(flx_site_output, paste("../results/v4_ec_site_locations_combined_source_error/", splitname[1], "_gpp_allvar_ec_sites_combined_source_data_error_", season_name, ".csv", sep = ""), row.names = F)
  
  # output all together
  if (j == 1){
    # if first time, label as output file
    flx_site_output_all <- flx_site_output
  } else {
    # if after first time, append by rows
    flx_site_output_all <- rbind.data.frame(flx_site_output_all, flx_site_output)
  }

  j <- j + 1 # iterator
}

# for (file_num in 1:length(file_list)) {
#   getTrendy(file_num, ec_gpp_sens, flx_site_info, file_list, log.formula, my.formula)
# }

# write out ALL data frame to csv
# this is the key output file we're looking for
write.csv(flx_site_output_all, paste("../results/v4_ec_site_locations_combined_source_error/allmodels_gpp_allvar_ec_sites_combined_source_data_error.csv", sep = ""), row.names = F)

# CRU_Aridity_P_PET column is 1991-2020 (EC time range)
# Aridity column is 1991-2016 (TRENDY time range)

#####
# model regression using all models (excluding SDGVM)

setwd("../results/v4_ec_site_locations_combined_source_error/")

# flx_site_output_all <- read.csv()

flx_site_output_all_noSDGVM <- flx_site_output_all[which(flx_site_output_all$ModelName != "SDGVM"),]

flx_site_output_all_noSDGVM$Season <- factor(flx_site_output_all_noSDGVM$Season, levels = c("spring", "summer", "fall"), labels = c("Spring", "Summer", "Fall"))

ggplot(data = flx_site_output_all_noSDGVM, aes(x = Aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0) +
  #geom_text(aes(label = SITE_ID, color = Climate2), size = 2, fontface = "bold") +
  geom_point(aes(color = Climate), size = 1) +
  geom_smooth(method = "lm", formula = log.formula, color = "black") +
  facet_wrap(~Season, ncol = 3) +
  scale_y_continuous(breaks = seq(-3, 10)) + # y ticks
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(Aridity), y = Precip_Slope, label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.97, label.x = 0.97) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.97) +
  labs(x = "Aridity (P/PET)", y = "Slope GPP ~ Precip [g C m-2 / mm]",
       title = "All models: GPP ~ Precip") + 
  theme_bw()
ggsave("all_models_gpp_precip.png", width = 12, height = 4, units = "in")

# new version with log scaled x
ggplot(data = flx_site_output_all_noSDGVM, aes(x = log10(Aridity), y = Precip_Slope)) +
  geom_hline(yintercept = 0) +
  #geom_text(aes(label = SITE_ID, color = Climate2), size = 2, fontface = "bold") +
  geom_point(aes(color = Climate), size = 1) +
  geom_smooth(method = "lm", color = "black") + # formula is now linear
  facet_wrap(~Season, ncol = 3) +
  scale_y_continuous(breaks = seq(-3, 10)) + # y ticks
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(Aridity), y = Precip_Slope, label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.97, label.x = 0.97) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.97) +
  labs(x = "Log10 Aridity (P/PET)", y = "Slope GPP ~ Precip [g C m-2 / mm]",
       title = "All models: GPP ~ Precip") + 
  theme_bw()
ggsave("all_models_gpp_precip_log10aridity.png", width = 12, height = 4, units = "in")

# model regression using mean of models per site (excluding SDGVM)
flx_site_output_all_noSDGVM_mean <- aggregate(flx_site_output_all_noSDGVM, 
                                              by = list(flx_site_output_all_noSDGVM$SITE_ID, flx_site_output_all_noSDGVM$Season,
                                                        flx_site_output_all_noSDGVM$Climate), 
                                              FUN = mean, na.rm = T)

flx_site_output_all_noSDGVM_mean$Climate<- flx_site_output_all_noSDGVM_mean$Group.3
flx_site_output_all_noSDGVM_mean$SITE_ID <- flx_site_output_all_noSDGVM_mean$Group.1
flx_site_output_all_noSDGVM_mean$Season <- flx_site_output_all_noSDGVM_mean$Group.2

ggplot(data = flx_site_output_all_noSDGVM_mean, aes(x = Aridity, y = Precip_Slope)) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = SITE_ID, color = Climate), size = 2, fontface = "bold") +
  #geom_point(aes(color = Climate2), size = 1) +
  geom_smooth(method = "lm", formula = log.formula, color = "black") +
  facet_wrap(~Season, ncol = 3) +
  scale_y_continuous(breaks = seq(-3, 10)) + # y ticks
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(Aridity), y = Precip_Slope, label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.97, label.x = 0.97) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.97) +
  labs(x = "Aridity (P/PET)", y = "Slope GPP ~ Precip [g C m-2 / mm]",
      title = "Model mean: GPP ~ Precip") + 
  theme_bw()
ggsave("all_models_mean_gpp_precip.png", width = 12, height = 4, units = "in")


#####
# 
# # haven't run this part yet
# 
# # read in
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4")
# setwd("../results/v4_ec_site_locations_combined_source/")
# 
# flx_site_output_all <- read.csv("allmodels_gpp_precip_ec_sites_data.csv")
# 
# # calculate log-linear regression slopes for each model, and then model mean, compare with original EC slopes in analysis 
# 
# # # test
# # s <- "fall"
# # m <- "CABLE"
# # 
# # flx_model_sub <- subset(flx_site_output_all, Season == s & ModelName == m)
# # # log.formula <- y ~ log10(x) # reminder about log formula in case it wasn't already loaded
# # fit <- lm(flx_model_sub$Precip_Slope ~ log10(flx_model_sub$Aridity))
# # sfit <- summary(fit)
# 
# seasons <- unique(flx_site_output_all$Season)
# modelnames <- unique(flx_site_output_all$ModelName)
# 
# model_slope_output_colnames <- c("ModelName", "Season", "intercept", "slope", "slope_se", "pvalue", "R2")
# model_slope_output <- data.frame(matrix(NA, nrow = length(seasons)*length(modelnames), ncol = length(model_slope_output_colnames)))
# colnames(model_slope_output) <- model_slope_output_colnames
# 
# j <- 1 # row indexer
# 
# for (s in seasons){
#   for (m in modelnames){
#     
#     flx_model_sub <- subset(flx_site_output_all, Season == s & ModelName == m)
#     fit <- lm(flx_model_sub$Precip_Slope ~ log10(flx_model_sub$Aridity))
#     sfit <- summary(fit)
#     
#     model_slope_output[j, 1:2] <- c(m, s)
#     model_slope_output[j, 3:ncol(model_slope_output)] <- c(sfit$coefficients[1,1], sfit$coefficients[2,1], sfit$coefficients[2,2], sfit$coefficients[2,4], sfit$r.squared)
#     
#     j <- j + 1
#   }
# }
# 
# # plotting
# model_slope_output$Season <- factor(model_slope_output$Season, levels = c("spring", "summer", "fall"), labels = c("Spring", "Summer", "Fall"))
# 
# model_slope_output2 <- subset(model_slope_output, ModelName != "SDGVM")
# 
# # new theme, modified theme_bw
# theme_new <- theme_set(theme_bw())
# theme_new <- theme_update(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# # this is now generic theme applied to plot? weird
# # careful re-running this, something borked with the color...
# 
# # plot slopes
# # all in one
# ggplot(model_slope_output2) +
#   geom_col(aes(x = ModelName, y = slope, fill = Season), width = 0.7, position = position_dodge2(preserve = "single", padding = 0.1)) +
#   geom_hline(yintercept = 0) +
#   #geom_point(aes(x = ModelName, y = slope, color = Season), size = 2, position = position_dodge2(preserve = "single")) +
#   #geom_errorbar(aes(x = ModelName, ymin = slope - slope_se, ymax = slope + slope_se)) + 
#   geom_errorbar(aes(x = ModelName, ymin = slope - slope_se, ymax = slope + slope_se, group = Season), width = 0.7, size = 0.5, position = position_dodge2(width = 0.5, padding = 0.1)) + 
#   #facet_wrap(~Season, ncol = 3) + 
#   labs(x = "Model Name", y = "Log10 Slope") + 
#   scale_fill_manual(values = c("blue", "forestgreen", "orange2")) + 
#   geom_hline(yintercept = -1.34, color = "blue", linetype = "dashed") + 
#   geom_hline(yintercept = -0.813, color = "forestgreen", linetype = "dashed") + 
#   geom_hline(yintercept = -0.646, color = "orange2", linetype = "dashed")
# 
# ggsave("gpp_precip_model_means_logslopes.png", width = 7, height = 5, units = "in")
# 
# # faceted version
# 
# ec_slopes <- data.frame(Season = factor(c("spring", "summer", "fall"), levels = c("spring", "summer", "fall"), labels = c("Spring", "Summer", "Fall")), ec_slope = c(-1.34, -0.813, -0.646))
# 
# ggplot(model_slope_output2) +
#   geom_col(aes(x = ModelName, y = slope), width = 0.7) + #, position = position_dodge2(preserve = "single", padding = 0.1)) +
#   geom_hline(yintercept = 0) +
#   #geom_point(aes(x = ModelName, y = slope, color = Season), size = 2, position = position_dodge2(preserve = "single")) +
#   #geom_errorbar(aes(x = ModelName, ymin = slope - slope_se, ymax = slope + slope_se)) + 
#   geom_errorbar(aes(x = ModelName, ymin = slope - slope_se, ymax = slope + slope_se, group = Season), width = 0.7, size = 0.5) + #, position = position_dodge2(width = 0.5, padding = 0.1)) + 
#   facet_wrap(~Season, ncol = 3) + 
#   labs(x = "Model Name", y = "Log10 Slope") +
#   geom_hline(data = ec_slopes, aes(yintercept = ec_slope), linetype = "dashed")
# 
# ggsave("gpp_precip_model_means_logslopes_facets.png", width = 10, height = 5, units = "in")
# 
# 
# ###
# # get mean slope values
# 
# mean_slopes <- aggregate(model_slope_output2$slope, by = list(model_slope_output2$Season), FUN = mean)
