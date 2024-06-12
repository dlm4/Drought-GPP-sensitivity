


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

# https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/
pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")

pie(rep(1,15), col=pal)

# pick up from this script
# compare_gpp_sif_datasets_slopes_mainfigs.R


# Would read this file in
# write.csv(model_site_slopes, file = "tbm_seasons_site_slopes.csv", row.names = F)
model_site_slopes <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source_error_1992_2016_detrend/tbm_seasons_site_slopes.csv")

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

# igbp color scheme # revise these colors
igbp_names <- c("ENF", "EBF", "DBF", "MF", 
                "CSH", "OSH", "WSA", "SAV", "GRA",  
                "WET")

igbp_colors <- c("#377e22", "#9acd32", 
                 "#b1fca3",
                 pal[3], 
                 pal[4], 
                 pal[5],  
                 pal[15], 
                 pal[12], 
                 "#f19e38",
                 "#006ddb")

# # experimenting
# igbp_colors <- c("#377e22", "#9acd32", #pal[14], #"#75fb4c", 
#                  "#b1fca3",
#                  pal[3], #"#52976a",
#                  pal[4], #"#8d3a64", 
#                  pal[5], #"#f7cea0", 
#                  pal[15], #"#f19e38", #"#f7ce46",
#                  pal[12],  #"#d6fed0", 
#                  "#f19e38",
#                  "#006ddb") #, "#2a6495")
# #igbp_colors <- c(pal[6], pal[3], pal[4], pal[8], pal[11], pal[12], pal[13], pal[15], pal[14], pal[10])

# original colors are OK, but could shift a couple and/or collapse some classes
# CSH -> OSH
# SAV -> WSA

model_site_slopes_tc_spring_gpp <- subset(model_site_slopes_tc_spring, ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))
arid_threshold <- 0.65
model_site_slopes_tc_spring_gpp$threshold <- "Energy-limited (Wet)"
model_site_slopes_tc_spring_gpp$threshold[which(model_site_slopes_tc_spring_gpp$TC_aridity < arid_threshold)] <- "Water-limited (Dry)"

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

ec_site_slopes_tc_spring_gpp <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName == "EC")
colnames(ec_site_slopes_tc_spring_gpp)[4] <- "EC" # need to change this name so it will overlay on everything!


a_level <- 1
point_size <- 2
line_size <- 0.3
yrange <- c(-1.7, 2.9)

# original colors are OK, but could shift a couple and/or collapse some classes
# CSH -> OSH
# SAV -> WSA
#ec_site_slopes_tc_spring_gpp$IGBP[ec_site_slopes_tc_spring_gpp$IGBP == "CSH"] <- "OSH"
#ec_site_slopes_tc_spring_gpp$IGBP[ec_site_slopes_tc_spring_gpp$IGBP == "SAV"] <- "WSA"

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")


# EDITS FOR MAP BASED ON REVIEWER 1'S COMMENTS
ec_data <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName == "EC")
trendy_mean_data <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName == "TRENDY_model_mean")
ec_and_trendy_mean_data <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName %in% c("EC", "TRENDY_model_mean"))

ggplot(ec_and_trendy_mean_data) +
  geom_point(aes(x = TC_aridity, y = Precip_Slope, color = ModelName)) +
  scale_x_continuous(trans = "log10") +
  theme_classic()

# Do: TRENDY Precip_Slope - EC Precip_Slope
trendy_ec_dif <- subset(trendy_mean_data, select = c("SITE_ID", "TC_aridity", "Precip_Slope"))
trendy_ec_dif$Precip_Slope_dif <- trendy_mean_data$Precip_Slope - ec_data$Precip_Slope

ggplot(trendy_ec_dif) +
  geom_point(aes(x = TC_aridity, y = Precip_Slope_dif)) +
  scale_x_continuous(trans = "log10") +
  theme_classic()

# Get linear regression to apply to WI map
dif_fit <- lm(trendy_ec_dif$Precip_Slope_dif ~ log10(trendy_ec_dif$TC_aridity))
summary(dif_fit)
# y = 0.35*log10(x) + 0.15, R2 = 0.058, p = 0.061
