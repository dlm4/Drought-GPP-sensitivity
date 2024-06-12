# Figure 3!

library(tidyverse)
library(ggpubr)
library(reshape2)
library(ggpmisc)
library(ggrepel)
library(ggtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x


# climate color scheme
# color scheme from peel et al. 2007
# https://hess.copernicus.org/articles/11/1633/2007/
climate_names <- c("Bwh", "Bsh", "Bsk",
                   "Csa", "Csb", 
                   "Cfa", "Cfb",
                   "Dfa", "Dfb", "Dfc", "Dwc")
climate_colors <- c("#e13122", "#eba83a", "#f8e074",
                    "#f6f651", "#c7c941",
                    "#d1f56e", "#8ae756",
                    "#75f9fb", "#67c4f3", "#33757f", "#4f54aa")

# igbp color scheme
igbp_names <- c("ENF", "EBF", "DBF", "MF", 
                "CSH", "OSH", "SAV", "WSA", "GRA",  
                "WET")
igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
                 "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
                 "#2a6495")

# Load MLM results in same style as original TRENDY univariate output data
# Same thing but now with slope error for more representative plotting
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source_error_mlm/")
trendy_slope_merged <- read_csv("allmodels_gpp_mlm_allvar_ec_sites_combined_source_data_error.csv") # trendy output, univariate

trendy_slope_merged <- subset(trendy_slope_merged, ModelName != "SDGVM") # exclude SDGVM

# Get list of sites that are >10 years of data
# use MLM data
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_mlm_slopes.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

flx_slope_merged_sub <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10)

# plotting setup

unique(trendy_slope_merged$SITE_ID)
unique(flx_slope_merged_sub$SITE_ID)
unique(trendy_slope_merged$ModelName)

trendy_met_sub <- subset(trendy_slope_merged, select = c(CRU_Aridity_P_PET, Precip_Slope, Precip_Slope_SE, Ta_Slope, Ta_Slope_SE, PAR_Slope, PAR_Slope_SE))
trendy_met_mean <- aggregate(trendy_met_sub, by = list(trendy_slope_merged$SITE_ID), FUN = mean)
colnames(trendy_met_mean)[1] <- "SITE_ID"

trendy_met_mean_slope <- subset(trendy_met_mean, select = c(SITE_ID, CRU_Aridity_P_PET, Precip_Slope, Ta_Slope, PAR_Slope))
trendy_met_mean_slope_melt <- melt(trendy_met_mean_slope, id.vars = c("SITE_ID", "CRU_Aridity_P_PET"))
colnames(trendy_met_mean_slope_melt)[3:4] <- c("Met_Var", "Slope")

trendy_met_mean_slope_se <- subset(trendy_met_mean, select = c(SITE_ID, CRU_Aridity_P_PET, Precip_Slope_SE, Ta_Slope_SE, PAR_Slope_SE))
trendy_met_mean_slope_se_melt <- melt(trendy_met_mean_slope_se, id.vars = c("SITE_ID", "CRU_Aridity_P_PET"))
colnames(trendy_met_mean_slope_se_melt)[3:4] <- c("Met_Var", "Slope_SE")

trendy_met_mean_slope_and_se <- cbind.data.frame(trendy_met_mean_slope_melt, trendy_met_mean_slope_se_melt$Slope_SE)
colnames(trendy_met_mean_slope_and_se)[5] <- "Slope_SE"

# need to remake plots in the same style as the trends from the univariate version with colors...
ggplot(trendy_met_mean_slope_and_se, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE)) + 
  geom_point() +
  facet_wrap(~Met_Var, nrow = 3, scales = "free_y") +
  theme_bw()

# can sub in trendy_met_mean_slope_and_se for the flx output data into new version... note this currently has all 112 sites, not the 61 site version
# merge to get slopes of met var, compare with box plot to ec data?

#####
# box plot comparisons at CRU threshold level
# slope comparisons

# flx data:
# flx_slope_merged_sub
flx_slopes_allvars <- subset(flx_slope_merged_sub, select = c(SITE_ID, Met_Var, Season, Npts, MLM_Slope, Slope_SE, LOCATION_LAT, LOCATION_LONG, LOCATION_ELEV, IGBP, MAT, MAP, Climate, CRU_Aridity_P_PET, CRU_P_Annual_Mean_Total_mm, CRU_PET_Annual_Mean_Total_mm))
flx_slopes_allvars$Source <- "EC"
colnames(flx_slopes_allvars)[which(colnames(flx_slopes_allvars) == "MLM_Slope")] <- "Slope" # relabel
flx_slopes_allvars$aridity_bins <- cut(flx_slopes_allvars$CRU_Aridity_P_PET, breaks = c(0, 0.65, 10))
flx_slopes_allvars$aridity_labels <- "wet"
flx_slopes_allvars$aridity_labels[which(flx_slopes_allvars$aridity_bins == "(0,0.65]")] <- "dry"

# break out each of the different variables into new data frames
flx_slopes_precip <- subset(flx_slopes_allvars, subset = Met_Var == "Precip")
flx_slopes_ta <- subset(flx_slopes_allvars, subset = Met_Var == "Ta")
flx_slopes_par <- subset(flx_slopes_allvars, subset = Met_Var == "PAR")

# make new versions for trendy
trendy_slopes_precip <- flx_slopes_precip
trendy_slopes_ta <- flx_slopes_ta
trendy_slopes_par <- flx_slopes_par

trendy_slopes_precip$Source <- "TRENDY"
trendy_slopes_ta$Source <- "TRENDY"
trendy_slopes_par$Source <- "TRENDY"

trendy_met_mean_slope_and_se_sub <- trendy_met_mean_slope_and_se[which(trendy_met_mean_slope_and_se$SITE_ID %in% flx_slopes_allvars$SITE_ID),] # get subset of sites, 61
trendy_slopes_precip$Slope <- trendy_met_mean_slope_and_se_sub$Slope[which(trendy_met_mean_slope_and_se_sub$Met_Var == "Precip_Slope")]
trendy_slopes_precip$Slope_SE <- trendy_met_mean_slope_and_se_sub$Slope_SE[which(trendy_met_mean_slope_and_se_sub$Met_Var == "Precip_Slope")]
trendy_slopes_ta$Slope <- trendy_met_mean_slope_and_se_sub$Slope[which(trendy_met_mean_slope_and_se_sub$Met_Var == "Ta_Slope")]
trendy_slopes_ta$Slope_SE <- trendy_met_mean_slope_and_se_sub$Slope_SE[which(trendy_met_mean_slope_and_se_sub$Met_Var == "Ta_Slope")]
trendy_slopes_par$Slope <- trendy_met_mean_slope_and_se_sub$Slope[which(trendy_met_mean_slope_and_se_sub$Met_Var == "PAR_Slope")]
trendy_slopes_par$Slope_SE <- trendy_met_mean_slope_and_se_sub$Slope_SE[which(trendy_met_mean_slope_and_se_sub$Met_Var == "PAR_Slope")]

# these are the data frames for regular plotting
flx_trendy_precip <- rbind.data.frame(flx_slopes_precip, trendy_slopes_precip)
flx_trendy_ta <- rbind.data.frame(flx_slopes_ta, trendy_slopes_ta)
flx_trendy_par <- rbind.data.frame(flx_slopes_par, trendy_slopes_par)

# trendy data frames with all models for spring and sites with >= 10 years
trendy_slope_merged_sub_spring <- subset(trendy_slope_merged, subset = SITE_ID %in% flx_trendy_precip$SITE_ID & Season == "spring")

# test if wet trendy precip slopes are different from zero
#t.test(trendy_slopes_precip$Slope[which(trendy_slopes_precip$aridity_labels == "wet")])

# plotting

dry_hex <- "#f35e5a"
wet_hex <- "#17b3b7"

# Box plots
p_flx_trendy_precip_mean_color <- ggplot(flx_trendy_precip) +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x = Source, y = Slope, fill = aridity_labels), outlier.shape = NA) +
  #geom_hline(yintercept = c(ec_dry_slope_mean), linetype = "dashed", color = dry_hex) +
  #geom_hline(yintercept = c(ec_wet_slope_mean), linetype = "dashed", color = wet_hex) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
  #guides(fill="none") +
  #scale_fill_brewer(palette="Set1") +
  coord_cartesian(ylim = c(-2, 6)) +
  labs(y = bquote("GPP ~ Precip slope [g C " ~m^2*" / mm]"), fill = "Aridity") +
  theme(legend.position = c(0.7, 0.8),
        legend.key = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank())
p_flx_trendy_precip_mean_color

p_flx_trendy_par_mean_color <- ggplot(flx_trendy_par) +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x = Source, y = Slope, fill = aridity_labels), outlier.shape = NA) +
  #geom_hline(yintercept = c(ec_dry_slope_mean), linetype = "dashed", color = dry_hex) +
  #geom_hline(yintercept = c(ec_wet_slope_mean), linetype = "dashed", color = wet_hex) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
  guides(fill="none") +
  #scale_fill_brewer(palette="Set1") +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  labs(y = bquote("GPP ~ PAR slope [g C / MJ]")) +
  theme(legend.key = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank())
p_flx_trendy_par_mean_color

p_flx_trendy_ta_mean_color <- ggplot(flx_trendy_ta) +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x = Source, y = Slope, fill = aridity_labels), outlier.shape = NA) +
  #geom_hline(yintercept = c(ec_dry_slope_mean), linetype = "dashed", color = dry_hex) +
  #geom_hline(yintercept = c(ec_wet_slope_mean), linetype = "dashed", color = wet_hex) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
  guides(fill="none") +
  #scale_fill_brewer(palette="Set1") +
  coord_cartesian(ylim = c(-110, 200)) +
  labs(y = bquote("GPP ~ Ta slope [g C " ~m^2*" / \u00b0C]")) +
  theme(legend.key = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank())
p_flx_trendy_ta_mean_color

ggarrange(p_flx_trendy_precip_mean_color, p_flx_trendy_par_mean_color, p_flx_trendy_ta_mean_color, nrow = 3, labels = "auto")

t.test(flx_slopes_precip$Slope[which(flx_slopes_precip$aridity_labels == "wet")]) # flx wet sites precip not significantly different from zero (p = 0.2697)
t.test(trendy_slopes_precip$Slope[which(trendy_slopes_precip$aridity_labels == "wet")]) # trendy wet sites precip, significantly positive (p << 0.0001)


# Regression plots

# Precip
# flx
p_spring_slope_precip_igbp <- ggplot(data = flx_slopes_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + 
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2, 6)) + # ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C " ~ m^-2*" / mm]"), fill = "IGBP") +
  #scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  guides(fill = "none") +
  annotate("text", x = 0.25, y = 6, label = "EC") +
  annotate("text", 0, y = -2, label = "Dry", color = dry_hex) + annotate("text", 1.25, y = -2, label = "Wet \u2192", color = wet_hex) + 
  theme(#legend.position = c(0.6, 0.6), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.x=element_blank(), axis.title.y=element_blank())
p_spring_slope_precip_igbp

# trendy
p_trendy_spring_slope_precip_igbp <- ggplot(data = trendy_slopes_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(data = trendy_slope_merged_sub_spring, aes(x = CRU_Aridity_P_PET, y = Precip_Slope, group = ModelName), 
              method = "lm", se = FALSE, formula = log.formula, size = 0.5, color = "gray80") + # add this for individual models
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + 
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2, 6)) + # ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C " ~ m^-2*" / mm]"), fill = "IGBP") +
  #scale_color_discrete(guide = "none") +
  #guides(fill = "none") + 
  guides(fill = guide_legend(ncol = 4)) +
  annotate("text", x = 0.25, y = 6, label = "TRENDY") +
  annotate("text", 0, y = -2, label = "Dry", color = dry_hex) + annotate("text", 1.25, y = -2, label = "Wet \u2192", color = wet_hex) + 
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  theme(legend.position = c(0.6, 0.6), 
    legend.key = element_blank(), legend.background = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.x=element_blank(), axis.title.y=element_blank())
p_trendy_spring_slope_precip_igbp


# PAR
# flx
p_spring_slope_par_igbp <- ggplot(data = flx_slopes_par, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) +
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2.5, 2.5)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "IGBP") +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  guides(fill = "none") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.x=element_blank(), axis.title.y=element_blank())
p_spring_slope_par_igbp

# trendy
p_trendy_spring_slope_par_igbp <- ggplot(data = trendy_slopes_par, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(data = trendy_slope_merged_sub_spring, aes(x = CRU_Aridity_P_PET, y = PAR_Slope, group = ModelName), 
              method = "lm", se = FALSE, formula = log.formula, size = 0.5, color = "gray80") + # add this for individual models
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) +
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2.5, 2.5)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "IGBP") +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  guides(fill = "none") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.x=element_blank(), axis.title.y=element_blank())
p_trendy_spring_slope_par_igbp

# Ta
# flx
p_spring_slope_ta_igbp <- ggplot(data = flx_slopes_ta, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + 
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-110, 200)) + #ylim = c(-110, 110)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Ta slope [g C " ~m^-2*" / \u00b0C]"), fill = "IGBP") +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  guides(fill = "none") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.y=element_blank())
p_spring_slope_ta_igbp

# trendy
p_trendy_spring_slope_ta_igbp <- ggplot(data = trendy_slopes_ta, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(data = trendy_slope_merged_sub_spring, aes(x = CRU_Aridity_P_PET, y = Ta_Slope, group = ModelName), 
              method = "lm", se = FALSE, formula = log.formula, size = 0.5, color = "gray80") + # add this for individual models
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + 
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-110, 200)) + #ylim = c(-110, 110)) + # ylim = c(-1.3, 1.3)
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Ta slope [g C " ~m^-2*" / \u00b0C]"), fill = "IGBP") +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  guides(fill = "none") + 
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.y=element_blank())
p_trendy_spring_slope_ta_igbp

# arrange everything in a super plot
p_compare_mlm <- ggarrange(p_flx_trendy_precip_mean_color, p_spring_slope_precip_igbp, p_trendy_spring_slope_precip_igbp,
          p_flx_trendy_par_mean_color, p_spring_slope_par_igbp, p_trendy_spring_slope_par_igbp,
          p_flx_trendy_ta_mean_color, p_spring_slope_ta_igbp, p_trendy_spring_slope_ta_igbp,
          nrow = 3, ncol = 3, labels = "auto", widths = c(1, 2, 2), align = "hv")
ggsave("figures/compare_mlm_ec_trendy_spring.png", p_compare_mlm, width = 11, height = 11, units = "in")
