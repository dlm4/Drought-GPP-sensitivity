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


# Load output slopes and plot

#setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
# slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr
# 
# flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
# flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_mlm_slopes.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_mlm.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

# relabel columns and met vars !!!
flx_slope_merged$Slope <- flx_slope_merged$MLM_Slope
flx_slope_merged$Met_Var[which(flx_slope_merged$Met_Var == "Precip")] <- "P_F_sum"
flx_slope_merged$Met_Var[which(flx_slope_merged$Met_Var == "PAR")] <- "PAR_F_sum"
flx_slope_merged$Met_Var[which(flx_slope_merged$Met_Var == "Ta")] <- "TA_F_mean"

# Just keep spring and precip
flx_slope_merged_spring_precip <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum")

#table(flx_slope_merged_spring_precip$Npts)

# get numbers for certain ranges of aridity = 0.65 threshold and Npts >= 10
sub1 <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET >= 0.65 & Npts >= 10)
sub1_negslope <- subset(sub1, Slope < 0)
sub2  <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET < 0.65 & Npts >= 10)
sub2_posslope <- subset(sub2, Slope > 0)

#####
# Just keep sites with at least 10 years of data, include error bars

# PRECIP

npts_min <- 10
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)

title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")

# Climate version
p_spring_slope_precip_climate <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + 
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C " ~ m^-2*" / mm]"), fill = "Climate",
       title = title_phrase) +
  #scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_precip_climate

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist_precip <- ggplot(data = flx_slope_merged_spring_precip_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-2, 6)) + # c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_precip

p_spring_slope_precip_hist_climate <- ggarrange(p_spring_slope_precip_climate, p_spring_hist_precip, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_precip_hist_climate, width = 10, height = 5, units = "in")

# IGBP version
p_spring_slope_precip_igbp <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
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
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C " ~ m^-2*" / mm]"), fill = "IGBP",
       title = title_phrase) +
  #scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_precip_igbp

p_spring_slope_hist_igbp <- ggarrange(p_spring_slope_precip_igbp, p_spring_hist_precip, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist_igbp, width = 10, height = 5, units = "in")

# PAR
# Just keep spring and PAR
flx_slope_merged_spring_par <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "PAR_F_sum")

npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_par_sub <- subset(flx_slope_merged_spring_par, subset = Npts >= npts_min)

title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_par_sub), " flux sites", sep = "")

p_spring_slope_par_climate <- ggplot(data = flx_slope_merged_spring_par_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + 
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "Climate",
       title = title_phrase) +
  scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_par_climate

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_par_sub$AI_threshold <- "wet"
flx_slope_merged_spring_par_sub$AI_threshold[which(flx_slope_merged_spring_par_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist_par <- ggplot(data = flx_slope_merged_spring_par_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_hist_par

p_spring_slope_hist_par_climate <- ggarrange(p_spring_slope_par_climate, p_spring_hist_par, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_par_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist_par_climate, width = 10, height = 5, units = "in")

# version with IGBP
p_spring_slope_par_igbp <- ggplot(data = flx_slope_merged_spring_par_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
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
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "IGBP",
       title = title_phrase) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_par_igbp

p_spring_slope_hist_par_igbp <- ggarrange(p_spring_slope_par_igbp, p_spring_hist_par, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_par_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist_par_igbp, width = 10, height = 5, units = "in")


# Just keep spring and Ta
flx_slope_merged_spring_ta <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "TA_F_mean")

# # get numbers for certain ranges of aridity = 0.65 threshold and Npts >= 10
# sub1 <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET >= 0.65 & Npts >= 10)
# sub1_posslope <- subset(sub1, Slope > 0)
# sub2  <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET < 0.65 & Npts >= 10)
# sub2_negslope <- subset(sub2, Slope < 0)

#table(flx_slope_merged_spring_precip$Npts)
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_ta_sub <- subset(flx_slope_merged_spring_ta, subset = Npts >= npts_min)

title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_ta_sub), " flux sites", sep = "")

p_spring_slope_ta_climate <- ggplot(data = flx_slope_merged_spring_ta_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + 
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Ta slope [g C " ~m^-2*" / \u00b0C]"), fill = "Climate",
       title = title_phrase) +
  scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_ta_climate

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_ta_sub$AI_threshold <- "wet"
flx_slope_merged_spring_ta_sub$AI_threshold[which(flx_slope_merged_spring_ta_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist_ta <- ggplot(data = flx_slope_merged_spring_ta_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), 
                 binwidth = 5, 
                 color = "black") +
  coord_cartesian(ylim = c(-110, 200)) + #ylim = c(-110, 110)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_hist_ta

p_spring_slope_hist_ta_climate <- ggarrange(p_spring_slope_ta_climate, p_spring_hist_ta, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist_ta_climate, width = 10, height = 5, units = "in")

# version with IGBP
p_spring_slope_ta_igbp <- ggplot(data = flx_slope_merged_spring_ta_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
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
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Ta slope [g C " ~m^-2*" / \u00b0C]"), fill = "IGBP",
       title = title_phrase) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_ta_igbp

p_spring_slope_hist_ta_igbp <- ggarrange(p_spring_slope_ta_igbp, p_spring_hist_ta, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist_ta_igbp, width = 10, height = 5, units = "in")

# Now just merge the slope plots
p_spring_slope_all_climate <- ggarrange(p_spring_slope_precip_climate, p_spring_slope_par_climate, p_spring_slope_ta_climate, nrow = 3, align = "h", labels = "auto", common.legend = TRUE, legend = "right")
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_errorbar_all_AIthreshold_climate_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_all_climate, width = 7, height = 15, units = "in")

p_spring_slope_all_igbp <- ggarrange(p_spring_slope_precip_igbp, p_spring_slope_par_igbp, p_spring_slope_ta_igbp, nrow = 3, align = "h", labels = "auto", common.legend = TRUE, legend = "right")
ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_errorbar_all_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_all_igbp, width = 7, height = 15, units = "in")

# could do a super plot merge with histograms, but would need to figure out what the histograms would be again....

#####
# Looping parts down here
for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)
  
  title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")
  
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
    coord_cartesian(xlim = c(0, 4), ylim = c(-3, 4)) + # ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_slope
  
  # vertical histogram of slopes as Trevor suggested, combine with ggpubr
  flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
  flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
  p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
    geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
    coord_cartesian(ylim = c(-3, 4)) + # c(-1.75, 2.75)) +
    labs(y = "", fill = "Aridity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_hist
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
  
  # version with IGBP
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
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
    coord_cartesian(xlim = c(0, 4), ylim = c(-3, 4)) + # ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "IGBP",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
}

#####
# Just keep spring and PAR
flx_slope_merged_spring_par <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "PAR_F_sum")

#table(flx_slope_merged_spring_precip$Npts)

for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_spring_par_sub <- subset(flx_slope_merged_spring_par, subset = Npts >= npts_min)
  
  title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_par_sub), " flux sites", sep = "")
  
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_par_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "Climate",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_slope
  
  # vertical histogram of slopes as Trevor suggested, combine with ggpubr
  flx_slope_merged_spring_par_sub$AI_threshold <- "wet"
  flx_slope_merged_spring_par_sub$AI_threshold[which(flx_slope_merged_spring_par_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
  p_spring_hist <- ggplot(data = flx_slope_merged_spring_par_sub) +
    geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
    coord_cartesian(ylim = c(-2.5, 2.5)) +
    labs(y = "", fill = "Aridity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_hist
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_par_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
  
  # version with IGBP
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_par_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
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
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "IGBP",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_par_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
}

#####
# Just keep spring and Ta
flx_slope_merged_spring_ta <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "TA_F_mean")

# get numbers for certain ranges of aridity = 0.65 threshold and Npts >= 10
sub1 <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET >= 0.65 & Npts >= 10)
sub1_posslope <- subset(sub1, Slope > 0)
sub2  <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET < 0.65 & Npts >= 10)
sub2_negslope <- subset(sub2, Slope < 0)

#table(flx_slope_merged_spring_precip$Npts)

for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_spring_ta_sub <- subset(flx_slope_merged_spring_ta, subset = Npts >= npts_min)
  
  title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_ta_sub), " flux sites", sep = "")
  
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_ta_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
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
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Ta slope [g C / \u00b0C]", fill = "Climate",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_slope
  
  # vertical histogram of slopes as Trevor suggested, combine with ggpubr
  flx_slope_merged_spring_ta_sub$AI_threshold <- "wet"
  flx_slope_merged_spring_ta_sub$AI_threshold[which(flx_slope_merged_spring_ta_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
  p_spring_hist <- ggplot(data = flx_slope_merged_spring_ta_sub) +
    geom_histogram(aes(y = Slope, fill = AI_threshold), 
                   binwidth = 5, 
                   color = "black") +
    coord_cartesian(ylim = c(-110, 200)) + #ylim = c(-110, 110)) +
    labs(y = "", fill = "Aridity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_hist
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
  
  # version with IGBP
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_ta_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
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
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Ta slope [g C / \u00b0C]", fill = "IGBP",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_ta_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
}

#####
# Just do initial plot looped for all vars, seasons multiplot

# Just keep spring and precip for initial test
flx_slope_merged_sub <- subset(flx_slope_merged, Met_Var %in% c("P_F_sum", "PAR_F_sum", "TA_F_mean"))

# set levels
flx_slope_merged_sub$Season <- factor(flx_slope_merged_sub$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_sub$Met_Var <- factor(flx_slope_merged_sub$Met_Var, levels = c("P_F_sum", "TA_F_mean", "PAR_F_sum"))

#flx_slope_merged_sub <- flx_slope_merged_sub[which(flx_slope_merged_sub$SITE_ID != "IL-Yat"),]

# site_season <- data.frame(table(flx_slope_merged_sub_n$SITE_ID, flx_slope_merged_sub_n$Season))
# site_npts_sum <- aggregate(site_season$Freq, by = list(site_season$Var1), FUN = "sum")
# site_npts_complete <- site_npts_sum$Group.1[which(site_npts_sum$x == 9)]
# 
# # spring precip is able to have 61 sites by itself, but only 59 sites that exist for all seasons
# 
# flx_slope_merged_sub <- flx_slope_merged_sub[which(flx_slope_merged_sub$SITE_ID %in% site_npts_complete),]

for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_sub_n <- subset(flx_slope_merged_sub, subset = Npts >= npts_min)
  
  #title_phrase <- paste("MLM ", npts_min, "+ years of data; ", nrow(flx_slope_merged_sub_n)/3/3, " flux sites", sep = "")
  
  p_slope <- ggplot(data = flx_slope_merged_sub_n, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
    #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
    facet_grid(vars(Met_Var), vars(Season), scales = "free_y") +
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
    coord_cartesian(xlim = c(0, 4)) + #, ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "Annual Aridity (P/PET)", 
         #y = "GPP ~ Precip slope [g C / mm]", 
         fill = "Climate") +
         #title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_slope
  ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_all_test_", npts_min_text, ".png", sep = ""),
         p_slope, width = 10, height = 7, units = "in")
  # ggsave(paste("figures/min_npts_looping/mlm/mlm_added_sites_gpp_all_test_noIL-Yat_", npts_min_text, ".png", sep = ""), 
  #        p_slope, width = 10, height = 7, units = "in")
}
