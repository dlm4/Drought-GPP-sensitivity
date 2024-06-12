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


log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/
pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")

# igbp color scheme | will have to update this based on main_fig_test
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


# igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
#                  "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
#                  "#2a6495")


setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/tests")


# Supplemental S3 slope 10+ years
# everything! npts >= 10
npts_min <- 10

#flx_slope_merged_subnpts <- subset(flx_slope_merged, subset = Npts >= npts_min)
exclude_inds <- which(flx_slope_merged$SITE_ID == "IL-Yat" & flx_slope_merged$Season == "Summer" & flx_slope_merged$Met_Var == "P_F_sum")
flx_slope_merged_sub1 <- flx_slope_merged[which(1:nrow(flx_slope_merged) != exclude_inds),]

flx_slope_merged_subnpts <- subset(flx_slope_merged_sub1, subset = Npts >= npts_min)
flx_slope_merged_subnpts$Season <- factor(flx_slope_merged_subnpts$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_subnpts$Met_Var <- factor(flx_slope_merged_subnpts$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"))
#flx_slope_merged_subnpts <- subset(flx_slope_merged_subnpts, SITE_ID != "IL-Yat") # extreme precip sensitivity in summer, so test excluding

p_all_slope_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE) + #, formula = log.formula) +
  #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color ="gray20", size = 2, shape = 21, stroke = 0.25) +
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
  labs(x = "Wetness Index (P/PET)", y = bquote("Slope"), fill = "IGBP") + #,title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  theme(#legend.position = c(0.85, 0.8),
    legend.key = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA), strip.background = element_blank())
p_all_slope_log10x

# ggsave(paste("gpp_all_igbp_logx_", npts_min_text, "noIL-Yatsummerprecip_eq.png", sep = ""),
#        p_all_slope_log10x, width = 10, height = 8, units = "in")

#--

# Supplemental S4 slope 5+ years
# everything! npts >= 10
npts_min <- 5

#flx_slope_merged_subnpts <- subset(flx_slope_merged, subset = Npts >= npts_min)
exclude_inds <- which(flx_slope_merged$SITE_ID == "IL-Yat" & flx_slope_merged$Season == "Summer" & flx_slope_merged$Met_Var == "P_F_sum")
flx_slope_merged_sub1 <- flx_slope_merged[which(1:nrow(flx_slope_merged) != exclude_inds),]

flx_slope_merged_subnpts <- subset(flx_slope_merged_sub1, subset = Npts >= npts_min)
flx_slope_merged_subnpts$Season <- factor(flx_slope_merged_subnpts$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_subnpts$Met_Var <- factor(flx_slope_merged_subnpts$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"))
#flx_slope_merged_subnpts <- subset(flx_slope_merged_subnpts, SITE_ID != "IL-Yat") # extreme precip sensitivity in summer, so test excluding

p_all_slope_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE) + #, formula = log.formula) +
  #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color ="gray20", size = 2, shape = 21, stroke = 0.25) +
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
  labs(x = "Wetness Index (P/PET)", y = bquote("Slope"), fill = "IGBP") + #,title = title_phrase) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  theme(#legend.position = c(0.85, 0.8),
    legend.key = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA), strip.background = element_blank())
p_all_slope_log10x

# ggsave(paste("gpp_all_igbp_logx_", npts_min_text, "noIL-Yatsummerprecip_eq.png", sep = ""),
#        p_all_slope_log10x, width = 10, height = 8, units = "in")

#--
# Supplemental S5 pearson 10+ years

npts_min <- 10

#flx_slope_merged_subnpts <- subset(flx_slope_merged, subset = Npts >= npts_min)
exclude_inds <- which(flx_slope_merged$SITE_ID == "IL-Yat" & flx_slope_merged$Season == "Summer" & flx_slope_merged$Met_Var == "P_F_sum")
flx_slope_merged_sub1 <- flx_slope_merged[which(1:nrow(flx_slope_merged) != exclude_inds),]

flx_slope_merged_subnpts <- subset(flx_slope_merged_sub1, subset = Npts >= npts_min)
flx_slope_merged_subnpts$Season <- factor(flx_slope_merged_subnpts$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_subnpts$Met_Var <- factor(flx_slope_merged_subnpts$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"))
#flx_slope_merged_subnpts <- subset(flx_slope_merged_subnpts, SITE_ID != "IL-Yat") # extreme precip sensitivity in summer, so test excluding

p_all_corr_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = PearsonR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE) +
  geom_point(aes(fill = IGBP), color ="gray20", size = 2, shape = 21, stroke = 0.25) +
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
  labs(x = "Wetness Index (P/PET)", y = bquote("Pearson correlation (R)"), fill = "IGBP") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA), strip.background = element_blank())
p_all_corr_log10x

npts_min_text <- as.character(npts_min)
ggsave(paste("gpp_all_igbp_logx_", npts_min_text, "noIL-Yatsummerprecip_eq_pcorr.png", sep = ""),
       p_all_corr_log10x, width = 10, height = 8, units = "in")

#--

# Supplemental S6 spearman 10+ years

npts_min <- 10

#flx_slope_merged_subnpts <- subset(flx_slope_merged, subset = Npts >= npts_min)
exclude_inds <- which(flx_slope_merged$SITE_ID == "IL-Yat" & flx_slope_merged$Season == "Summer" & flx_slope_merged$Met_Var == "P_F_sum")
flx_slope_merged_sub1 <- flx_slope_merged[which(1:nrow(flx_slope_merged) != exclude_inds),]

flx_slope_merged_subnpts <- subset(flx_slope_merged_sub1, subset = Npts >= npts_min)
flx_slope_merged_subnpts$Season <- factor(flx_slope_merged_subnpts$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged_subnpts$Met_Var <- factor(flx_slope_merged_subnpts$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"))
#flx_slope_merged_subnpts <- subset(flx_slope_merged_subnpts, SITE_ID != "IL-Yat") # extreme precip sensitivity in summer, so test excluding

p_all_corr_log10x <- ggplot(data = flx_slope_merged_subnpts, aes(x = TC_aridity, y = SpearmanR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = FALSE) +
  geom_point(aes(fill = IGBP), color ="gray20", size = 2, shape = 21, stroke = 0.25) +
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
  labs(x = "Wetness Index (P/PET)", y = bquote("Spearman correlation (R)"), fill = "IGBP") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA), strip.background = element_blank())
p_all_corr_log10x

npts_min_text <- as.character(npts_min)
ggsave(paste("gpp_all_igbp_logx_", npts_min_text, "noIL-Yatsummerprecip_eq_scorr.png", sep = ""),
       p_all_corr_log10x, width = 10, height = 8, units = "in")

#--


