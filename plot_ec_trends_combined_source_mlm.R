
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

# Load output slopes and plot

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_mlm_slopes.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_mlm.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

num_bins <- 30 # test number of bins for all categories

# Just keep spring and precip for initial test
flx_slope_merged_spring_precip <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "Precip")

# get numbers for certain ranges of aridity = 0.65 threshold
sub1 <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET >= 0.65)
sub1_negslope <- subset(sub1, MLM_Slope < 0)
sub2  <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET < 0.65)
sub2_posslope <- subset(sub2, MLM_Slope > 0)

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# Spring only
p_spring_slope_precip <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = MLM_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = MLM_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2.25, 4.25)) + #ylim = c(-1.5, 2.5)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_precip
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_mlm.png", p_spring_slope_precip, width = 6, height = 5, units = "in")

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
p_spring_hist_precip <- ggplot(data = flx_slope_merged_spring_precip) +
  geom_histogram(aes(y = MLM_Slope), bins = num_bins, #binwidth = 0.2, 
                 color = "black", fill = "gray80") +
  coord_cartesian(ylim = c(-2.25, 4.25), xlim = c(0, 35)) +
  labs(y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_precip

p_spring_slope_hist_precip <- ggarrange(p_spring_slope_precip, p_spring_hist_precip, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_mlm_slope_hist.png", p_spring_slope_hist_precip, width = 9, height = 5, units = "in")

# Just keep spring and PAR
flx_slope_merged_spring_par <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "PAR")

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# Spring only
p_spring_slope_par <- ggplot(data = flx_slope_merged_spring_par, aes(x = CRU_Aridity_P_PET, y = MLM_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = MLM_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-3.5, 5)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ PAR slope [g C / MJ]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_par
#ggsave("figures/added_sites_gpp_par_ec_spring_cru_aridity_log10_mlm.png", p_spring_slope_par, width = 6, height = 5, units = "in")

p_spring_hist_par <- ggplot(data = flx_slope_merged_spring_par) +
  geom_histogram(aes(y = MLM_Slope), bins = num_bins, #binwidth = 0.2, 
                 color = "black", fill = "gray80") +
  coord_cartesian(ylim = c(-3.5, 5), xlim = c(0, 35)) +
  labs(y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_par

# Just keep spring and Ta
flx_slope_merged_spring_ta <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "Ta")

# get numbers for certain ranges of aridity = 0.65 threshold
sub1 <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET >= 0.65)
sub1_posslope <- subset(sub1, MLM_Slope > 0)
sub2  <- subset(flx_slope_merged_spring_ta, CRU_Aridity_P_PET < 0.65)
sub2_negslope <- subset(sub2, MLM_Slope < 0)

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# Spring only
p_spring_slope_ta <- ggplot(data = flx_slope_merged_spring_ta, aes(x = CRU_Aridity_P_PET, y = MLM_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = MLM_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-100, 200)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Ta slope [g C / \u00b0C]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_ta
#ggsave("figures/added_sites_gpp_ta_ec_spring_cru_aridity_log10_mlm.png", p_spring_slope, width = 6, height = 5, units = "in")

p_spring_hist_ta <- ggplot(data = flx_slope_merged_spring_ta) +
  geom_histogram(aes(y = MLM_Slope), bins = num_bins,
                 color = "black", fill = "gray80") +
  coord_cartesian(ylim = c(-100, 200), xlim = c(0, 35)) +
  labs(y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist_ta


# arrange all
p_spring_slope_hist_all <- ggarrange(p_spring_slope_precip, p_spring_hist_precip, 
                                     p_spring_slope_par, p_spring_hist_par,
                                     p_spring_slope_ta, p_spring_hist_ta,
                                     nrow = 3, ncol = 2, align = "h", labels = "auto", widths = c(2,1),
                                     common.legend = TRUE, legend = "right")
p_spring_slope_hist_all
ggsave("figures/added_sites_gpp_all_ec_spring_cru_aridity_log10_mlm_slope_hist.png", p_spring_slope_hist_all, width = 6, height = 10, units = "in")


#####

# multiplot
flx_slope_merged_plot <- flx_slope_merged

# Multiplot
flx_slope_merged_plot$Season <- factor(flx_slope_merged_plot$Season, levels = c("Spring", "Summer", "Fall"), labels = c("Spring", "Summer", "Fall"))
flx_slope_merged_plot$Met_Var <- factor(flx_slope_merged_plot$Met_Var, levels = c("Precip", "PAR", "Ta"), #levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"), 
                                        labels = c("GPP ~ Precip [g C / mm]", "GPP ~ PAR [g C / MJ]", "GPP ~ Ta [g C / \u00b0C]")) # label with degree symbol!

# log10
p_slopes_all <- ggplot(data = flx_slope_merged_plot, aes(x = CRU_Aridity_P_PET, y = MLM_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = MLM_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.88, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_mlm.png", p_slopes_all, width = 10, height = 8, units = "in")

# Reset labels for correlation setup
flx_slope_merged_plot <- flx_slope_merged
flx_slope_merged_plot$Season <- factor(flx_slope_merged_plot$Season, levels = c("Spring", "Summer", "Fall"), labels = c("Spring", "Summer", "Fall"))
flx_slope_merged_plot$Met_Var <- factor(flx_slope_merged_plot$Met_Var, levels = c("Precip", "PAR", "Ta"), #levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"),
                                        labels = c("GPP ~ Precip", "GPP ~ PAR", "GPP ~ Ta")) # label with degree symbol!
# Pearson
p_corrp_all <- ggplot(data = flx_slope_merged_plot, aes(x = CRU_Aridity_P_PET, y = Partial_PearsonR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Partial_PearsonR,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.09, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.03, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1, 1)) +
  labs(x = "Annual Aridity (P/PET)", y = "Pearson Correlation (R)", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_corrp_all
ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_mlm_pearson.png", p_corrp_all, width = 10, height = 8, units = "in")

#####
# plot VIF as boxplot? or something else?
flx_slope_merged2 <- flx_slope_merged
flx_slope_merged2$Season <- factor(flx_slope_merged2$Season, levels = c("Spring", "Summer", "Fall"))
flx_slope_merged2$Met_Var <- factor(flx_slope_merged2$Met_Var, levels = c("Precip", "PAR", "Ta"))
p_vif <- ggplot(flx_slope_merged2, aes(x = Season, y = log10(VIF), fill = Met_Var)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "yellow", "red")) + 
  theme_bw()
p_vif
ggsave("figures/added_sites_gpp_ta_ec_spring_cru_aridity_mlm_vif_boxplot.png", p_vif, width = 6, height = 5, units = "in")

