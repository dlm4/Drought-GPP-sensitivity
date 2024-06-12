


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

#pie(rep(1,15), col=pal)

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
#flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2_pdsi.csv")

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
# flx_site_sens_sub_precip <- subset(flx_site_sens_sub, subset = Met_Var == "P_F_sum")
# flx_site_sens_sub_ta <- subset(flx_site_sens_sub, subset = Met_Var == "TA_F_mean")
# flx_site_sens_sub_par <- subset(flx_site_sens_sub, subset = Met_Var == "PAR_F_sum")
flx_site_sens_sub_pdsi <- subset(flx_site_sens_sub, subset = Met_Var == "PDSI_mean")

# colnames(flx_site_sens_sub_precip) <- c("SITE_ID", "ModelName", "Season", "Precip_Slope", "Precip_pval", "Precip_R2", "Precip_Slope_SE")
# colnames(flx_site_sens_sub_ta) <- c("SITE_ID", "ModelName", "Season", "Ta_Slope", "Ta_pval", "Ta_R2", "Ta_Slope_SE")
# colnames(flx_site_sens_sub_par) <- c("SITE_ID", "ModelName", "Season", "PAR_Slope", "PAR_pval", "PAR_R2", "PAR_Slope_SE")
colnames(flx_site_sens_sub_pdsi) <- c("SITE_ID", "ModelName", "Season", "PDSI_Slope", "PDSI_pval", "PDSI_R2", "PDSI_Slope_SE")

# flx_site_sens_sub_precip$ModelName <- "EC"
# flx_site_sens_sub_ta$ModelName <- "EC"
# flx_site_sens_sub_par$ModelName <- "EC"
flx_site_sens_sub_pdsi$ModelName <- "EC"

# flx_site_sens_sub_wide <- merge(flx_site_sens_sub_precip, flx_site_sens_sub_ta) %>% merge(flx_site_sens_sub_par)
# 
# model_ec_site_slopes <- bind_rows(model_site_slopes, flx_site_sens_sub_wide) # rbind but lets EC columns for intercepts be NA
# model_ec_site_slopes_tc_spring <- merge(flx_site_info_tc_sub, subset(model_ec_site_slopes, subset = Season == "Spring"), by = "SITE_ID")
# model_ec_site_slopes_tc_spring_gpp <- subset(model_ec_site_slopes_tc_spring, ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))
# 
# ec_site_slopes_tc_spring_gpp <- subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName == "EC")
# colnames(ec_site_slopes_tc_spring_gpp)[4] <- "EC" # need to change this name so it will overlay on everything!

ec_site_slopes_tc_spring_gpp <- merge(subset(flx_site_sens_sub_pdsi,subset = Season == "Spring"), flx_site_info_tc, by = "SITE_ID") 


a_level <- 1
point_size <- 2
line_size <- 0.3
yrange <- c(-60, 60) # will need to change this

# original colors are OK, but could shift a couple and/or collapse some classes
# CSH -> OSH
# SAV -> WSA
#ec_site_slopes_tc_spring_gpp$IGBP[ec_site_slopes_tc_spring_gpp$IGBP == "CSH"] <- "OSH"
#ec_site_slopes_tc_spring_gpp$IGBP[ec_site_slopes_tc_spring_gpp$IGBP == "SAV"] <- "WSA"

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
# And EC only
# test if including only signficant slopes

# try again with only middle 95%

ec_allmodel_pdsi_compare_ec <- ggplot(data = ec_site_slopes_tc_spring_gpp, # subset(ec_site_slopes_tc_spring_gpp, Precip_pval < 0.05), 
                                        aes(x = TC_aridity, y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = PDSI_Slope - PDSI_Slope_SE, ymax = PDSI_Slope + PDSI_Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ PDSI slope [g C" ~m^-2*" / PDSI]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.8, 0.9), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_pdsi_compare_ec

#median(model_ec_site_slopes_tc_spring_gpp$Precip_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]) # -0.143
ec_only_density <- ggplot(data = ec_site_slopes_tc_spring_gpp, #subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName %in% c("EC")), #& Precip_pval < 0.05), 
                          aes(y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  #geom_hline(yintercept = median(model_ec_site_slopes_tc_spring_gpp$PDSI_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]), linetype = "dotted", color = "gray20") +
  geom_hline(yintercept = median(ec_site_slopes_tc_spring_gpp$PDSI_Slope), linetype = "dotted", color = "gray20") +
  geom_density(color = "gray20") + 
  coord_cartesian(ylim = yrange) + 
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Density", y  = bquote("GPP ~ PDSI slope [g C" ~m^-2*" / PDSI]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_only_density

fig1_arrange_ec <- ggarrange(ec_allmodel_pdsi_compare_ec, ec_only_density,
                             widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
#ggsave("ec_only_precip_spring_slope_se_density_arranged_update_p_lt_05.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)
ggsave("ec_only_pdsi_spring_slope_se_density_arranged_update.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)

y <- ec_site_slopes_tc_spring_gpp$PDSI_Slope
x <- ec_site_slopes_tc_spring_gpp$TC_aridity
fit <- summary(lm(y ~ log10(x))) # y = -21.07*log10(x) -4.22, R2 = 0.27, p < 0.001

# get number of water-lim sites with positive GPP~PDSI relationship
length(which(x < 0.65)) # 15
length(which(x < 0.65 & y > 0)) # 14; 93%

length(which(x >= 0.65)) # 46
length(which(x >= 0.65 & y < 0)) # 33; 72%



# Need to add regression relationship and labels.
# this is the main figure 1, needing to add labels in Illustrator

# Version with site names
ec_allmodel_precip_compare_ec_sitenames <- ggplot(data = ec_site_slopes_tc_spring_gpp, # subset(ec_site_slopes_tc_spring_gpp, Precip_pval < 0.05), 
                                        aes(x = TC_aridity, y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  #geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  #geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_shadowtext(aes(color = IGBP, label = SITE_ID, bg.color="gray20", bg.r=0.1), size = 5/.pt, fontface = "bold") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  #scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors) +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec_sitenames
ggsave("ec_only_pdsi_spring_slope_se_arranged_update_sitenames.eps", ec_allmodel_precip_compare_ec_sitenames, width = 120, height = 120, units = "mm", dpi = 300)
#ggsave("ec_only_pdsi_spring_slope_se_arranged_update_sitenames_p_lt_05.eps", ec_allmodel_precip_compare_ec_sitenames, width = 120, height = 120, units = "mm", dpi = 300)

# fig1_arrange_ec_sitenames <- ggarrange(ec_allmodel_precip_compare_ec_sitenames, ec_only_density,
#                              widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
# ggsave("ec_only_precip_spring_slope_se_density_arranged_update_sitenames.eps", fig1_arrange_ec_sitenames, width = 120, height = 75, units = "mm", dpi = 300)


# ____
# test again, only keep middle 95% of PDSI slopes
q <- quantile(ec_site_slopes_tc_spring_gpp$PDSI_Slope, probs = c(0, 0.025, 0.975, 1))
ec_site_slopes_tc_spring_gpp_mid <- subset(ec_site_slopes_tc_spring_gpp, subset = PDSI_Slope > q[2] & PDSI_Slope < q[3])

# try again with only middle 95%

ec_allmodel_pdsi_compare_ec <- ggplot(data = ec_site_slopes_tc_spring_gpp_mid, # subset(ec_site_slopes_tc_spring_gpp, Precip_pval < 0.05), 
                                      aes(x = TC_aridity, y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = PDSI_Slope - PDSI_Slope_SE, ymax = PDSI_Slope + PDSI_Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ PDSI slope [g C" ~m^-2*" / PDSI]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.8, 0.9), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_pdsi_compare_ec

#median(model_ec_site_slopes_tc_spring_gpp$Precip_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]) # -0.143
ec_only_density <- ggplot(data = ec_site_slopes_tc_spring_gpp_mid, #subset(model_ec_site_slopes_tc_spring_gpp, subset = ModelName %in% c("EC")), #& Precip_pval < 0.05), 
                          aes(y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  #geom_hline(yintercept = median(model_ec_site_slopes_tc_spring_gpp$PDSI_Slope[which(model_ec_site_slopes_tc_spring_gpp$ModelName == "EC")]), linetype = "dotted", color = "gray20") +
  geom_hline(yintercept = median(ec_site_slopes_tc_spring_gpp_mid$PDSI_Slope), linetype = "dotted", color = "gray20") +
  geom_density(color = "gray20") + 
  coord_cartesian(ylim = yrange) + 
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Density", y  = bquote("GPP ~ PDSI slope [g C" ~m^-2*" / PDSI]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_only_density

fig1_arrange_ec <- ggarrange(ec_allmodel_pdsi_compare_ec, ec_only_density,
                             widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("ec_only_pdsi_spring_slope_se_density_arranged_update_mid95.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)

y <- ec_site_slopes_tc_spring_gpp_mid$PDSI_Slope
x <- ec_site_slopes_tc_spring_gpp_mid$TC_aridity
fit <- summary(lm(y ~ log10(x))) # y = -5.16*x + 3.10, R2 = 0.11, p = 0.013

# get number of water-lim sites with positive GPP~PDSI relationship
length(which(x < 0.65)) # 13
length(which(x < 0.65 & y > 0)) # 12; 92%

length(which(x >= 0.65)) # 44
length(which(x >= 0.65 & y < 0)) # 31; 70%



# Need to add regression relationship and labels.
# this is the main figure 1, needing to add labels in Illustrator

# Version with site names
ec_allmodel_precip_compare_ec_sitenames <- ggplot(data = ec_site_slopes_tc_spring_gpp_mid, # subset(ec_site_slopes_tc_spring_gpp, Precip_pval < 0.05), 
                                                  aes(x = TC_aridity, y = PDSI_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  #geom_linerange(aes(ymin = Precip_Slope - Precip_Slope_SE, ymax = Precip_Slope + Precip_Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  #geom_point(aes(fill = IGBP), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_shadowtext(aes(color = IGBP, label = SITE_ID, bg.color="gray20", bg.r=0.1), size = 5/.pt, fontface = "bold") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  #scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors) +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ PDSI slope [g C" ~m^-2*" / PDSI]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_allmodel_precip_compare_ec_sitenames
ggsave("ec_only_pdsi_spring_slope_se_arranged_update_sitenames_mid95.eps", ec_allmodel_precip_compare_ec_sitenames, width = 120, height = 120, units = "mm", dpi = 300)


# #####
# 
# #Figure 2
# 
# 
# # try for arid_threshold of 0.65 (default), 0.5. and 1
# arid_threshold <- 0.65
# model_ec_site_slopes_tc_spring$threshold <- "Energy-limited (Wet)"
# model_ec_site_slopes_tc_spring$threshold[which(model_ec_site_slopes_tc_spring$TC_aridity < arid_threshold)] <- "Water-limited (Dry)"
# 
# se <- function(x){sqrt(var(x)/length(x))}
# 
# agg_models <- aggregate(model_ec_site_slopes_tc_spring$Precip_Slope, 
#                         by = list(model_ec_site_slopes_tc_spring$ModelName, model_ec_site_slopes_tc_spring$threshold),
#                         FUN = mean)
# agg_models_se <- aggregate(model_ec_site_slopes_tc_spring$Precip_Slope, 
#                            by = list(model_ec_site_slopes_tc_spring$ModelName, model_ec_site_slopes_tc_spring$threshold),
#                            FUN = se)
# 
# colnames(agg_models) <- c("ModelName", "threshold", "Precip_Slope")
# agg_models2 <- cbind.data.frame(agg_models$ModelName[1:23], agg_models$Precip_Slope[1:23], agg_models$Precip_Slope[24:46])
# colnames(agg_models2) <- c("ModelName", "Precip_Slope_Wet", "Precip_Slope_Dry")
# 
# colnames(agg_models_se) <- c("ModelName", "threshold", "Precip_Slope_se")
# agg_models_se2 <- cbind.data.frame(agg_models_se$ModelName[1:23], agg_models_se$Precip_Slope[1:23], agg_models_se$Precip_Slope[24:46])
# colnames(agg_models_se2) <- c("ModelName", "Precip_Slope_se_Wet", "Precip_Slope_se_Dry")
# 
# library(ggrepel)
# library(ggtext)
# 
# agg_models_merge2 <- merge(agg_models2, agg_models_se2)
# 
# # SIF are different units and shoudn't be included
# agg_models3 <- subset(agg_models_merge2, subset = ModelName %notin% c("GOSIF_SIF", "CSIF_clear_daily"))
# agg_models3$ModelRef <- LETTERS[1:nrow(agg_models3)]
# agg_models3$ModelName2 <- agg_models3$ModelName
# agg_models3 <- mutate(agg_models3, ModelName2=recode(ModelName2, 
#                                                      FLUXCOM_RS_METEO_ERA5_GPP="FC RS METEO",
#                                                      FLUXCOM_RS_V006_GPP="FC RS",
#                                                      GOSIF_GPP="GOSIF",
#                                                      MODIS_Aqua_GPP="MODIS-Aqua",
#                                                      MODIS_Terra_GPP="MODIS-Terra",
#                                                      TRENDY_model_mean="TRENDY Mean",
#                                                      `ORCHIDEE-MICT`="OR.-MICT"))
# ec_ind <- which(agg_models3$ModelName == "EC") # get ec row index for rectange plotting
# 
# # for producing data for table
# #write.csv(agg_models3, "figure2_plot_data.csv", row.names = F)
# 
# # New column to colorize by data type (EC, RS, TBM)
# rs_models <- c("FLUXCOM_RS_METEO_ERA5_GPP", "FLUXCOM_RS_V006_GPP", "GOSIF_GPP",
#                "MODIS_Aqua_GPP", "MODIS_Terra_GPP")
# agg_models3$DataType <- "TBM" # tbm by default
# agg_models3$DataType[which(agg_models3$ModelName == "EC")] <- "EC"
# agg_models3$DataType[which(agg_models3$ModelName %in% rs_models)] <- "RS"
# 
# wet_dry_compare <- ggplot(agg_models3, aes(x = Precip_Slope_Wet, y = Precip_Slope_Dry)) +
#   #geom_rect(aes(x = Precip_Slope_Wet[ec_ind], y = Precip_Slope_Dry[ec_ind], width = Precip_Slope_se_Wet*2), color = "gray95") +
#   annotate("rect", 
#            xmin = agg_models3$Precip_Slope_Wet[ec_ind] - agg_models3$Precip_Slope_se_Wet[ec_ind], 
#            xmax = agg_models3$Precip_Slope_Wet[ec_ind] + agg_models3$Precip_Slope_se_Wet[ec_ind], 
#            ymin = -4, 
#            ymax = 4,
#            #alpha = .1,
#            fill = "gray95") +
#   annotate("rect", 
#            xmin = -4, 
#            xmax = 4, 
#            ymin = agg_models3$Precip_Slope_Dry[ec_ind] - agg_models3$Precip_Slope_se_Dry[ec_ind], 
#            ymax = agg_models3$Precip_Slope_Dry[ec_ind] + agg_models3$Precip_Slope_se_Dry[ec_ind],
#            #alpha = .1,
#            fill = "gray95") +
#   geom_hline(yintercept = 0, linetype  = "dashed", size = 0.5) + 
#   geom_vline(xintercept = 0, linetype = "dashed", size = 0.5) +
#   geom_linerange(aes(ymin = Precip_Slope_Dry - Precip_Slope_se_Dry, ymax = Precip_Slope_Dry + Precip_Slope_se_Dry), color = "gray50", size = 0.25) +
#   geom_linerange(aes(xmin = Precip_Slope_Wet - Precip_Slope_se_Wet, xmax = Precip_Slope_Wet + Precip_Slope_se_Wet), color = "gray50", size = 0.25) +
#   #geom_point(aes(color = DataType), size = 1) +
#   geom_point(aes(color = DataType), size = 1.5) + #, shape = 21, stroke = 0.25) +
#   scale_color_manual(values = c("#dc143c", "#ff8c00", "#1e90ff")) +
#   #geom_density2d() + # this makes it too busy
#   #geom_text(aes(label = ModelRef), size = 5/.pt) +
#   # geom_text_repel(aes(label = ModelName2, color = DataType), size = 6/.pt,
#   #                 #color = "gray30", 
#   #                 max.overlaps = 15, bg.color = "white", bg.r = 0.05,
#   #                 segment.color = "gray30", segment.size = 0.1) +
#   coord_fixed(ratio = agg_models3$Precip_Slope_se_Wet[ec_ind]/agg_models3$Precip_Slope_se_Dry[ec_ind], xlim = c(-0.4, 0.4), ylim = c(-0.2, 1.8)) + 
#   #labs(x = bquote(atop("GPP ~ Precip slope [g C" ~m^-2*" / mm]","Energy-limited sites")), y = bquote(atop("Water-limited sites", "GPP ~ Precip slope [g C" ~m^-2*" / mm]")), color = "Source") +
#   labs(x = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), color = "Source") +
#   theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
#         strip.background = element_blank(), strip.text = element_text(size = 7),
#         axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
#         legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
#         legend.position = c(0.9, 0.9), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
# wet_dry_compare
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
# # 0.65
# #ggsave("gpp_model_wet_dry_compare.eps", wet_dry_compare, width = 120, height = 120, units = "mm", dpi = 300)
# ggsave("gpp_model_wet_dry_compare_clean_new3.eps", wet_dry_compare, width = 120, height = 95, units = "mm", dpi = 300) # version without text
# # adding text in Illustrator instead
# #ggsave("gpp_model_wet_dry_compare_labeled_new.eps", wet_dry_compare, width = 120, height = 90, units = "mm", dpi = 300) # version without text
# 
# 
# 
# 
# # Show the contour only
# ggplot(data, aes(x=x, y=y) ) +
#   geom_density_2d()