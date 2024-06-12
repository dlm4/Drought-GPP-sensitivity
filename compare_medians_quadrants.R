library(tidyverse)
library(reshape2)
library(trend)
library(zyp)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
`%notin%` <- Negate(`%in%`) # %notin% function
library(ggsignif)
library(ggpubr)

# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons_site_id_intersect_pdsi.csv")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars_aridity <- read.csv("flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

flx_site_sens <- merge(flx_site_sens, flx_tc_vars_aridity, by = "SITE_ID")

 #----

spring_precip <- subset(flx_site_sens, subset = Met_Var == "P_F_sum" & Season == "Spring")
spring_par <- subset(flx_site_sens, subset = Met_Var == "PAR_F_sum" & Season == "Spring")
spring_temp <- subset(flx_site_sens, subset = Met_Var == "TA_F_mean" & Season == "Spring")

spring_precip$Ta_Median <- spring_temp$Met_Var_Median
spring_precip$PAR_Median <- spring_par$Met_Var_Median

a_level <- 1
point_size <- 2
line_size <- 0.3
yrange <- c(-1.7, 2.9)

ec_precip <- ggplot(data = spring_precip,
                                        aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  geom_point(aes(fill = Ta_Median), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  #scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  #scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_gradientn(colors = rev(c('#e66101','#fdb863','#f7f7f7','#b2abd2','#5e3c99'))) +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_precip

ec_precip <- ggplot(data = spring_precip,
                    aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  #geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  geom_point(aes(fill = PAR_Median), color = "gray20", size = point_size, shape = 21, stroke = 0.25) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  #scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  #scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_gradientn(colors = rev(c('#e66101','#fdb863','#f7f7f7','#b2abd2','#5e3c99'))) +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ec_precip

#####
# Maybe do 4 density plots for different quadrants of the figure?

spring_precip$quadrants <- NA
spring_precip$quadrants[which(spring_precip$TC_aridity > 0.65 & spring_precip$Slope > 0)] <- "Q1"
spring_precip$quadrants[which(spring_precip$TC_aridity < 0.65 & spring_precip$Slope > 0)] <- "Q2"
spring_precip$quadrants[which(spring_precip$TC_aridity < 0.65 & spring_precip$Slope < 0)] <- "Q3"
spring_precip$quadrants[which(spring_precip$TC_aridity > 0.65 & spring_precip$Slope < 0)] <- "Q4"

#spring_precip2 <- spring_precip[which(abs(spring_precip$Slope) > 0.2),]
spring_precip2 <- spring_precip[which(spring_precip$quadrants %in% c("Q1", "Q4")),]
spring_precip2$quadrants[which(spring_precip2$quadrants == "Q1")] <- ">0"
spring_precip2$quadrants[which(spring_precip2$quadrants == "Q4")] <- "<0"

quad_par <- ggplot(spring_precip2, aes(x = quadrants, y = PAR_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  geom_signif(comparisons = list(c(">0", "<0")),
                                # list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
                                # c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
              map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) +
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Spring PAR (MJ "~m^-2*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

quad_ta <- ggplot(spring_precip2, aes(x = quadrants, y = Ta_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  geom_signif(comparisons = list(c(">0", "<0")),
                            #list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
                            #     c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
              map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) + 
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Spring Ta ("~degree*C*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

arrange_par_ta <- ggarrange(quad_par, quad_ta,
                             widths = c(1, 1), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("figures/updated_figs/spring_par_ta_quadrants.eps", arrange_par_ta, width = 180, height = 89, units = "mm", dpi = 300)

quad_par <- ggplot(spring_precip2, aes(x = quadrants, y = PAR_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  #geom_signif(comparisons = list(c(">0", "<0")),
              # list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              # c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
  #            map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) +
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Spring PAR (MJ "~m^-2*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

quad_ta <- ggplot(spring_precip2, aes(x = quadrants, y = Ta_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  #geom_signif(comparisons = list(c(">0", "<0")),
              #list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              #     c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
  #            map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) + 
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Spring Ta ("~degree*C*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

arrange_par_ta <- ggarrange(quad_par, quad_ta,
                            widths = c(1, 1), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("figures/updated_figs/spring_par_ta_quadrants_no_p.eps", arrange_par_ta, width = 180, height = 89, units = "mm", dpi = 300)

#####
# Repeat analysis for summer


summer_precip <- subset(flx_site_sens, subset = Met_Var == "P_F_sum" & Season == "Summer")
summer_par <- subset(flx_site_sens, subset = Met_Var == "PAR_F_sum" & Season == "Summer")
summer_temp <- subset(flx_site_sens, subset = Met_Var == "TA_F_mean" & Season == "Summer")

summer_precip$Ta_Median <- summer_temp$Met_Var_Median
summer_precip$PAR_Median <- summer_par$Met_Var_Median

# Maybe do 4 density plots for different quadrants of the figure?

summer_precip$quadrants <- NA
summer_precip$quadrants[which(summer_precip$TC_aridity > 0.65 & summer_precip$Slope > 0)] <- "Q1"
summer_precip$quadrants[which(summer_precip$TC_aridity < 0.65 & summer_precip$Slope > 0)] <- "Q2"
summer_precip$quadrants[which(summer_precip$TC_aridity < 0.65 & summer_precip$Slope < 0)] <- "Q3"
summer_precip$quadrants[which(summer_precip$TC_aridity > 0.65 & summer_precip$Slope < 0)] <- "Q4"

#summer_precip2 <- summer_precip[which(abs(summer_precip$Slope) > 0.2),]
summer_precip2 <- summer_precip[which(summer_precip$quadrants %in% c("Q1", "Q4")),]
summer_precip2$quadrants[which(summer_precip2$quadrants == "Q1")] <- ">0"
summer_precip2$quadrants[which(summer_precip2$quadrants == "Q4")] <- "<0"

quad_par <- ggplot(summer_precip2, aes(x = quadrants, y = PAR_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  geom_signif(comparisons = list(c(">0", "<0")),
              # list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              # c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
              map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) +
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Summer PAR (MJ "~m^-2*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

quad_ta <- ggplot(summer_precip2, aes(x = quadrants, y = Ta_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  geom_signif(comparisons = list(c(">0", "<0")),
              #list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              #     c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
              map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) + 
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Summer Ta ("~degree*C*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

arrange_par_ta <- ggarrange(quad_par, quad_ta,
                            widths = c(1, 1), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("figures/updated_figs/summer_par_ta_quadrants.eps", arrange_par_ta, width = 180, height = 89, units = "mm", dpi = 300)

quad_par <- ggplot(summer_precip2, aes(x = quadrants, y = PAR_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  #geom_signif(comparisons = list(c(">0", "<0")),
              # list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              # c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
  #            map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) +
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Summer PAR (MJ "~m^-2*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

quad_ta <- ggplot(summer_precip2, aes(x = quadrants, y = Ta_Median)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size=1) + 
  #geom_signif(comparisons = list(c(">0", "<0")),
              #list(c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"), 
              #     c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")), 
  #            map_signif_level=FALSE, step_increase = 0.1, textsize = 7*0.35) + 
  labs(x = "GPP~precip sensitivity for energy-limited sites", y = expression("Median Summer Ta ("~degree*C*")")) + 
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 9),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 9),
        legend.key = element_blank(), legend.title = element_text(size = 9), legend.text = element_text(size = 9),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())

arrange_par_ta <- ggarrange(quad_par, quad_ta,
                            widths = c(1, 1), nrow = 1, labels = "auto", font.label = list(size = 8))
ggsave("figures/updated_figs/summer_par_ta_quadrants_no_p.eps", arrange_par_ta, width = 180, height = 89, units = "mm", dpi = 300)