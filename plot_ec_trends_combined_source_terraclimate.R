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
library(moments) # for skewness

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

# Just keep spring and precip
flx_slope_merged_spring_precip <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum")

nrow(subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum" & Npts >= 10)) # 61 sites in spring
nrow(subset(flx_slope_merged, Season == "Summer" & Met_Var == "P_F_sum" & Npts >= 10)) # 62 sites in summer
nrow(subset(flx_slope_merged, Season == "Fall" & Met_Var == "P_F_sum" & Npts >= 10)) # 63 sites in fall

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

# terraclimate vars, set for near output directory
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")

# 10 year version with error bars and labels
npts_min <- 10 # can change this
#npts_min <- 5
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)

title_phrase <- paste(npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")

# get numbers for certain ranges of aridity = 0.65 threshold
sub1 <- subset(flx_slope_merged_spring_precip_sub, TC_aridity >= 0.65)
sub1_negslope <- subset(sub1, Slope < 0) # 38/46, 83%
sub2  <- subset(flx_slope_merged_spring_precip_sub, TC_aridity < 0.65)
sub2_posslope <- subset(sub2, Slope > 0) # 12/15, 80%

# Get information on the distribution for talking about Figure 1b
summary(flx_slope_merged_spring_precip_sub$Slope)
mean(flx_slope_merged_spring_precip_sub$Slope)
median(flx_slope_merged_spring_precip_sub$Slope)
sd(flx_slope_merged_spring_precip_sub$Slope)
skewness(flx_slope_merged_spring_precip_sub$Slope)

# ALTERNATE OLD VERSIONS
#####
# p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = 0.65, linetype = "dotted") +
#   geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
#   #geom_smooth(method = "lm", color = "black", formula = log.formula) +
#   geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + # error bar on slope?
#   geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
#   geom_text_repel(aes(label = SITE_ID), size = 1.25, color = "gray20", max.overlaps = 30, bg.color = "white", bg.r = 0.05,
#                   segment.color = "gray20", segment.size = 0.2, fontface = "bold") +
#   stat_poly_eq(formula = my.formula, 
#                aes(x = log10(TC_aridity),
#                    y = Slope,
#                    label = paste(..eq.label.., sep = "")),
#                eq.x.rhs = "log(x)",
#                output.type = "text",
#                parse = FALSE, size = 3.5, na.rm = TRUE,
#                label.y = 0.93, label.x = 0.9) +
#   stat_poly_eq(formula = log.formula, 
#                aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
#                parse = TRUE, size = 3.5, na.rm = TRUE,
#                label.y = 0.89, label.x = 0.9) +
#   coord_cartesian(xlim = c(0, 3), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
#   #scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
#   scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3), labels = c("0", "0.65", "1", "2", "3")) + 
#   labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "Climate",
#        title = title_phrase) +
#   #scale_color_discrete(guide = "none") +
#   scale_fill_manual(breaks = climate_names, values = climate_colors) +
#   scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
#   theme(#legend.position = c(0.85, 0.8), 
#     legend.key = element_blank(), 
#     panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
# p_spring_slope
# 
# # vertical histogram of slopes as Trevor suggested, combine with ggpubr
# flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
# flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$TC_aridity < 0.65)] <- "dry"
# p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
#   geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
#   coord_cartesian(ylim = c(-1.75, 2.75)) +
#   labs(y = "", fill = "Aridity") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme(legend.key = element_blank(), 
#         panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
# p_spring_hist
# 
# # p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
# # ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_labels_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
# #        p_spring_slope_hist, width = 10, height = 5, units = "in")
# 
# p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = 0.65, linetype = "dotted") +
#   geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
#   geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
#   geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
#   #geom_text_repel(aes(label = SITE_ID), size = 1.25, color = "gray20", max.overlaps = 30, bg.color = "white", bg.r = 0.05,
#   #                segment.color = "gray20", segment.size = 0.2, fontface = "bold") +
#   stat_poly_eq(formula = my.formula, 
#                aes(x = log10(TC_aridity),
#                    y = Slope,
#                    label = paste(..eq.label.., sep = "")),
#                eq.x.rhs = "log(x)",
#                output.type = "text",
#                parse = FALSE, size = 3.5, na.rm = TRUE,
#                label.y = 0.93, label.x = 0.9) +
#   stat_poly_eq(formula = log.formula, 
#                aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
#                parse = TRUE, size = 3.5, na.rm = TRUE,
#                label.y = 0.89, label.x = 0.9) +
#   coord_cartesian(xlim = c(0, 3), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
#   #scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
#   scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3), labels = c("0", "0.65", "1", "2", "3")) + 
#   labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "Climate",
#        title = title_phrase) +
#   scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
#   scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
#   theme(#legend.position = c(0.85, 0.8), 
#     legend.key = element_blank(), 
#     panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
# p_spring_slope
# 
# #p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
# #ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
# #       p_spring_slope_hist, width = 10, height = 5, units = "in")
# 
# # log scaled x
# 
# p_spring_slope_log10x <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = 0.65, linetype = "dotted") +
#   geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
#   geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + # error bar on slope?
#   geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
#   #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
#   stat_poly_eq(formula = my.formula, 
#                aes(x = TC_aridity,
#                    y = Slope,
#                    label = paste(..eq.label.., sep = "")),
#                eq.x.rhs = "log(x)",
#                output.type = "text",
#                parse = FALSE, size = 3.5, na.rm = TRUE,
#                label.y = 0.83, label.x = 0.9) +
#   stat_poly_eq(formula = my.formula, #log.formula, 
#                aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
#                parse = TRUE, size = 3.5, na.rm = TRUE,
#                label.y = 0.79, label.x = 0.9) +
#   #coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
#   coord_cartesian(ylim = c(-1.75, 2.75)) +
#   labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "Climate",
#        title = title_phrase) +
#   scale_fill_manual(breaks = climate_names, values = climate_colors) +
#   scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
#   scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
#                      labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
#   theme(#legend.position = c(0.85, 0.8), 
#     legend.key = element_blank(), 
#     panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
# p_spring_slope_log10x
# 
# #p_spring_slope_log10x_hist <- ggarrange(p_spring_slope_log10x, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
# #ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_log10x_errorbar_labels_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
# #       p_spring_slope_log10x_hist, width = 10, height = 5, units = "in")
#####

# PRECIP, figure 1

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$TC_aridity < 0.65)] <- "dry"
p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist


p_spring_slope_log10x <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Slope,
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
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "IGBP",
       title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope_log10x

p_spring_slope_log10x_hist <- ggarrange(p_spring_slope_log10x, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
# ggsave(paste("figures/tc_versions/gpp_precip_spring_igbp_logx_", npts_min_text, ".png", sep = ""), 
#        p_spring_slope_log10x_hist, width = 10, height = 5, units = "in")
# ggsave(paste("figures/tc_versions/gpp_precip_spring_igbp_logx_labs_", npts_min_text, ".png", sep = ""), 
#        p_spring_slope_log10x_hist, width = 10, height = 5, units = "in")
ggsave(paste("figures/tc_versions/gpp_precip_spring_igbp_logx_error_", npts_min_text, ".png", sep = ""), 
        p_spring_slope_log10x_hist, width = 10, height = 5, units = "in")


# Wrap version by IGBP
p_spring_slope_log10x_wrap <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  facet_wrap(~IGBP, ncol = 3) +
  geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Slope,
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
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "IGBP",
       title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4,  0.65, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_log10x_wrap
ggsave(paste("figures/tc_versions/gpp_precip_spring_igbp_logx_error_", npts_min_text, "_IGBPwrap.png", sep = ""), 
       p_spring_slope_log10x_wrap, width = 10, height = 10, units = "in")


#####
# PAR and Ta

# Just keep spring and par
flx_slope_merged_spring_par <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "PAR_F_sum")

# 10 year version with error bars and labels
#npts_min <- 10 # can change this
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_par_sub <- subset(flx_slope_merged_spring_par, subset = Npts >= npts_min)

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_par_sub$AI_threshold <- "wet"
flx_slope_merged_spring_par_sub$AI_threshold[which(flx_slope_merged_spring_par_sub$TC_aridity < 0.65)] <- "dry"
p_spring_hist_par <- ggplot(data = flx_slope_merged_spring_par_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.5, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_par

p_spring_slope_log10x_par <- ggplot(data = flx_slope_merged_spring_par_sub, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  #coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  coord_cartesian(ylim = c(-1.5, 2.75)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ PAR slope [g C / MJ]"), fill = "IGBP",
       title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope_log10x_par

p_spring_slope_log10x_hist_par <- ggarrange(p_spring_slope_log10x_par, p_spring_hist_par, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
#ggsave(paste("figures/tc_versions/gpp_par_spring_igbp_logx_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_par, width = 10, height = 5, units = "in")
#ggsave(paste("figures/tc_versions/gpp_par_spring_igbp_logx_labs_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_par, width = 10, height = 5, units = "in")
#ggsave(paste("figures/tc_versions/gpp_par_spring_igbp_logx_error_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_par, width = 10, height = 5, units = "in")



# Just keep spring and ta
flx_slope_merged_spring_ta <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "TA_F_mean")

# 10 year version with error bars and labels
#npts_min <- 10 # can change this
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_ta_sub <- subset(flx_slope_merged_spring_ta, subset = Npts >= npts_min)

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_ta_sub$AI_threshold <- "wet"
flx_slope_merged_spring_ta_sub$AI_threshold[which(flx_slope_merged_spring_ta_sub$TC_aridity < 0.65)] <- "dry"
p_spring_hist_ta <- ggplot(data = flx_slope_merged_spring_ta_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 10, color = "black") +
  coord_cartesian(ylim = c(-110, 120)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_ta

p_spring_slope_log10x_ta <- ggplot(data = flx_slope_merged_spring_ta_sub, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  #coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  coord_cartesian(ylim = c(-110, 120)) +
  labs(x = "Wetness Index (P/PET)", y = bquote("GPP ~ Ta slope [g C " ~m^-2* " / \u00B0C]"), fill = "IGBP",
       title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope_log10x_ta

p_spring_slope_log10x_hist_ta <- ggarrange(p_spring_slope_log10x_ta, p_spring_hist_ta, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
#ggsave(paste("figures/tc_versions/gpp_ta_spring_igbp_logx_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_ta, width = 10, height = 5, units = "in")
#ggsave(paste("figures/tc_versions/gpp_ta_spring_igbp_logx_labs_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_ta, width = 10, height = 5, units = "in")
#ggsave(paste("figures/tc_versions/gpp_ta_spring_igbp_logx_error_", npts_min_text, ".png", sep = ""),
#       p_spring_slope_log10x_hist_ta, width = 10, height = 5, units = "in")


#####

# Need to put regressions in

# everything! npts >= 10
flx_slope_merged_subnpts <- subset(flx_slope_merged, subset = Npts >= npts_min)
flx_slope_merged_subnpts$Season <- factor(flx_slope_merged_subnpts$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_subnpts$Met_Var <- factor(flx_slope_merged_subnpts$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"))
#flx_slope_merged_subnpts <- subset(flx_slope_merged_subnpts, SITE_ID != "IL-Yat") # extreme precip sensitivity in summer, so test excluding

p_all_slope_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  labs(x = "Wetness Index (P/PET)", y = bquote("Slope"), fill = "IGBP",
       title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_all_slope_log10x

ggsave(paste("figures/tc_versions/gpp_all_igbp_logx_", npts_min_text, ".png", sep = ""),
      p_all_slope_log10x, width = 10, height = 8, units = "in")
#ggsave(paste("figures/tc_versions/gpp_all_igbp_logx_noIL-Yat_", npts_min_text, ".png", sep = ""),
#      p_all_slope_log10x, width = 10, height = 8, units = "in")


# Version with correlations
# Correlation version
p_all_corr_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = PearsonR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = PearsonR,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  labs(x = "Wetness Index (P/PET)", y = bquote("Pearson Correlation (R)"), fill = "IGBP") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_all_corr_log10x
ggsave(paste("figures/tc_versions/gpp_all_igbp_logx_pearsonR_", npts_min_text, ".png", sep = ""),
       p_all_corr_log10x, width = 10, height = 8, units = "in")

p_all_corr_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = SpearmanR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  stat_poly_eq(formula = my.formula, 
               aes(x = TC_aridity,
                   y = SpearmanR,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  labs(x = "Wetness Index (P/PET)", y = bquote("Spearman Correlation (R)"), fill = "IGBP") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.65, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_all_corr_log10x
ggsave(paste("figures/tc_versions/gpp_all_igbp_logx_spearmanR_", npts_min_text, ".png", sep = ""),
       p_all_corr_log10x, width = 10, height = 8, units = "in")


#####
# comparing sensitivities across seasons
precip_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "P_F_sum", select = c(SITE_ID, Slope))
par_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "PAR_F_sum", select = c(SITE_ID, Slope))
ta_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "TA_F_mean", select = c(SITE_ID, Slope))
spring_merge <- merge(precip_spring, par_spring, by = "SITE_ID") %>% merge(ta_spring, by = "SITE_ID")
colnames(spring_merge) = c("SITE_ID", "Precip", "PAR", "Ta")

ggplot(spring_merge, aes(x = Ta, y = PAR)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  geom_point(aes(color = Precip)) +
  geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  scale_color_continuous(type = "viridis") +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., sep = "")),
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  labs(x = bquote("GPP ~ Ta slope [g C" ~m^-2*" / \u00B0C]"), 
       y = bquote("GPP ~ PAR slope [g C / MJ]"),
       color = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
ggsave(paste("figures/tc_versions/gpp_compare_spring_sens", npts_min_text, ".png", sep = ""),  width = 189, height = 150, units = "mm")


# Spearman
precip_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "P_F_sum", select = c(SITE_ID, SpearmanR))
par_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "PAR_F_sum", select = c(SITE_ID, SpearmanR))
ta_spring <- subset(flx_slope_merged_subnpts, subset = Season == "Spring" & Met_Var == "TA_F_mean", select = c(SITE_ID, SpearmanR))
spring_merge <- merge(precip_spring, par_spring, by = "SITE_ID") %>% merge(ta_spring, by = "SITE_ID")
colnames(spring_merge) = c("SITE_ID", "Precip", "PAR", "Ta")

ggplot(spring_merge, aes(x = Ta, y = PAR)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  geom_point(aes(color = Precip)) +
  geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  scale_color_continuous(type = "viridis") +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., sep = "")),
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.83, label.x = 0.1) +
  stat_poly_eq(formula = my.formula, #log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.79, label.x = 0.1) +
  coord_equal() +
  scale_x_continuous(breaks = seq(-0.5, 0.75, 0.25)) +
  scale_y_continuous(breaks = seq(-0.5, 0.75, 0.25)) +
  labs(x = bquote("GPP ~ Ta Spearman R"), 
       y = bquote("GPP ~ PAR Spearman R"),
       color = bquote("GPP ~ Precip Spearman R")) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
ggsave(paste("figures/tc_versions/gpp_compare_spring_spearmanR", npts_min_text, ".png", sep = ""),  width = 189, height = 150, units = "mm")
