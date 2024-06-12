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

# Load in data
# setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source/")
# trendy_slope_merged <- read_csv("allmodels_gpp_precip_ec_sites_combined_source_data.csv") # trendy output, univariate

# Same thing but now with slope error for more representative plotting
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations_combined_source_error/")
trendy_slope_merged <- read_csv("allmodels_gpp_precip_ec_sites_combined_source_data_error.csv") # trendy output, univariate

trendy_slope_merged <- subset(trendy_slope_merged, ModelName != "SDGVM") # exclude SDGVM

# Get list of sites that are >10 years of data
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

flx_slope_merged_sub <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10)

#
spring_sites_10 <- unique(flx_slope_merged_sub$SITE_ID)

trendy_slope_merged_spring <- subset(trendy_slope_merged, Season == "spring" & SITE_ID %in% spring_sites_10)


ggplot(trendy_slope_merged_spring, aes(x = CRU_Aridity_P_PET, y = Precip_Slope)) +
  geom_point() +
  facet_wrap(~ModelName, ncol = 3)

trendy_plot_models <- trendy_slope_merged_spring
trendy_plot_models$aridity_bins <- cut(trendy_plot_models$CRU_Aridity_P_PET, breaks = c(0, 0.65, 10))
trendy_plot_models_wetsites <- subset(trendy_plot_models, subset = CRU_Aridity_P_PET >= 0.65)

p_trendy_plot_models <- ggplot(trendy_plot_models, aes(x = ModelName, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() + #aes(color = aridity_bins)) +
  facet_wrap(~aridity_bins, nrow = 2, scales = "free_y") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylim(-2, 3) +
  theme(legend.position = c(0.85, 0.7), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_trendy_plot_models

# man, LPX is way high and is pulling the distribution compared to all the other models...

# get model mean for precip
trendy_sub <- subset(trendy_slope_merged_spring, select = c(CRU_Aridity_P_PET, Precip_Slope))
trendy_sub_mean <- aggregate(trendy_sub, by = list(trendy_slope_merged_spring$SITE_ID), FUN = mean)
colnames(trendy_sub_mean)[1] <- "SITE_ID"

ggplot(trendy_sub_mean, aes(x = CRU_Aridity_P_PET, y = Precip_Slope)) +
  geom_point()

# subset, no LPX
trendy_sub_noLPX <- subset(trendy_slope_merged_spring, subset = ModelName != "LPX", select = c(CRU_Aridity_P_PET, Precip_Slope))
trendy_sub_mean_noLPX <- aggregate(trendy_sub_noLPX, by = list(trendy_slope_merged_spring$SITE_ID[which(trendy_slope_merged_spring$ModelName != "LPX")]), FUN = mean)
colnames(trendy_sub_mean_noLPX)[1] <- "SITE_ID"

ggplot(trendy_sub_mean_noLPX, aes(x = CRU_Aridity_P_PET, y = Precip_Slope)) +
  geom_point()


# Load in EC site observations and compare with boxplot
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")
flx_slope_merged_precip_spring <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum" & Npts >= 10)

# merge in trendy data with different factor level
flx_slope_merged_precip_spring$Source <- "EC" # label as eddy covariance
flx_slope_merged_precip_spring2 <- flx_slope_merged_precip_spring # make copy
flx_slope_merged_precip_spring2$Source <- "TRENDY" # substitute trendy model mean slopes
#trendy_sub_mean$SITE_ID == flx_slope_merged_precip_spring$SITE_ID # check site_id order is the same in both data sets
# trendy_sub_mean$CRU_Aridity_P_PET == flx_slope_merged_precip_spring$CRU_Aridity_P_PET # aridity is the same in both data sets

# can swap between versions with or without LPX, anomalously high model
flx_slope_merged_precip_spring2$Slope <- trendy_sub_mean$Precip_Slope # sub in slope
#flx_slope_merged_precip_spring2$Slope <- trendy_sub_mean_noLPX$Precip_Slope # sub in slope (no LPX)

flx_trendy <- rbind.data.frame(flx_slope_merged_precip_spring, flx_slope_merged_precip_spring2) # combine

# set up aridity bins
# new bins, simple
flx_trendy$aridity_bins <- cut(flx_trendy$CRU_Aridity_P_PET, breaks = c(0, 0.65, 10))

# output a version of this figure, leading difference boxplot?
# need version with comparing FLUXNET2015 too?
p_flx_trendy_mean <- ggplot(flx_trendy) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = aridity_bins, y = Slope, fill = Source)) +
  #geom_boxplot(aes(x = Source, y = Slope, color = aridity_bins)) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_flx_trendy_mean

p_flx_trendy_mean2 <- ggplot(flx_trendy) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = Source, y = Slope)) +
  facet_wrap(~aridity_bins, nrow = 2, scales = "free_y") +
  #geom_boxplot(aes(x = Source, y = Slope, color = aridity_bins)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_flx_trendy_mean2

ggarrange(p_flx_trendy_mean2, p_trendy_plot_models, nrow = 1, align = "h", widths = c(1, 4))

# need to retry this, but do each plot separately to get it all to line up on y

y_dry <- c(-0.75, 3)
y_wet <- c(-1, 1.5)

p_flx_trendy_mean_dry <- ggplot(flx_trendy[which(flx_trendy$aridity_bins == "(0,0.65]"),]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = Source, y = Slope), outlier.shape = NA) +
  coord_cartesian(ylim = y_dry) +
  theme(legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())
p_flx_trendy_mean_dry

p_flx_trendy_mean_wet <- ggplot(flx_trendy[which(flx_trendy$aridity_bins == "(0.65,10]"),]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = Source, y = Slope), outlier.shape = NA) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(-1, 0, 1)) + 
  coord_cartesian(ylim = y_wet) +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank())
p_flx_trendy_mean_wet

p_trendy_plot_models_dry <- ggplot(trendy_plot_models[which(trendy_plot_models$aridity_bins == "(0,0.65]"),], aes(x = ModelName, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  coord_cartesian(ylim = y_dry) +
  theme(legend.position = c(0.85, 0.7), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#p_trendy_plot_models_dry

p_trendy_plot_models_wet <- ggplot(trendy_plot_models[which(trendy_plot_models$aridity_bins == "(0.65,10]"),], aes(x = ModelName, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  coord_cartesian(ylim = y_wet) +
  theme(legend.position = c(0.85, 0.7), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#p_trendy_plot_models_wet

p_dry <- ggarrange(p_flx_trendy_mean_dry, p_trendy_plot_models_dry, align = "h", widths = c(1,4), labels = c("a", ""))
p_wet <- ggarrange(p_flx_trendy_mean_wet, p_trendy_plot_models_wet, align = "h", widths = c(1,4), labels = c("b", ""))
ggarrange(p_dry, p_wet, nrow = 2)
ggsave("figures/ec_trendy_boxplot_compare_10yrs.png", height = 140, width = 189, units = "mm")

# try combining into single set of plots?

# get mean of dry and wet sites for ec to draw lines across box plots as a reference...
ec_dry_slope_mean <- mean(flx_trendy$Slope[which(flx_trendy$Source == "EC" & flx_trendy$aridity_bins == '(0,0.65]')])
ec_wet_slope_mean <- mean(flx_trendy$Slope[which(flx_trendy$Source == "EC" & flx_trendy$aridity_bins == '(0.65,10]')])

dry_hex <- "#f35e5a"
wet_hex <- "#17b3b7"

flx_trendy$aridity_labels <- "dry"
flx_trendy$aridity_labels[which(flx_trendy$aridity_bins == "(0.65,10]")] <- "wet"

p_flx_trendy_mean_color <- ggplot(flx_trendy) +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x = Source, y = Slope, fill = aridity_labels), outlier.shape = NA) +
  geom_hline(yintercept = c(ec_dry_slope_mean), linetype = "dashed", color = dry_hex) +
  geom_hline(yintercept = c(ec_wet_slope_mean), linetype = "dashed", color = wet_hex) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
  guides(fill="none") +
  #scale_fill_brewer(palette="Set1") +
  coord_cartesian(ylim = c(-1, 3)) +
  labs(y = "GPP ~ Precip slope [g C / mm]") +
  theme(legend.key = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank())
p_flx_trendy_mean_color

trendy_plot_models$aridity_labels <- "dry"
trendy_plot_models$aridity_labels[which(trendy_plot_models$aridity_bins == "(0.65,10]")] <- "wet"

p_trendy_plot_models_color <- ggplot(trendy_plot_models, aes(x = ModelName, y = Precip_Slope, fill = aridity_labels)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_hline(yintercept = c(ec_dry_slope_mean), linetype = "dashed", color = dry_hex) +
  geom_hline(yintercept = c(ec_wet_slope_mean), linetype = "dashed", color = wet_hex) + 
  coord_cartesian(ylim = c(-1, 3)) +
  labs(fill = "Aridity") +
  #scale_fill_brewer(palette="Set1") +
  theme(legend.position = c(0.9, 0.8), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#p_trendy_plot_models_wet
ggarrange(p_flx_trendy_mean_color, p_trendy_plot_models_color, nrow = 1, align = "h", widths = c(1,3), labels = c("c", ""))
ggsave("figures/ec_trendy_boxplot_compare_10yrs_v2.png", height = 90, width = 189, units = "mm")

#####


# check if significantly overlaps zero?
t.test(subset(flx_trendy, subset = CRU_Aridity_P_PET >= 0.65 & Source == "EC", select = Slope))
t.test(subset(flx_trendy, subset = CRU_Aridity_P_PET >= 0.65 & Source == "TRENDY", select = Slope))

# get differences in slopes
test1 <- flx_slope_merged_precip_spring
test1$trendy_slope <- trendy_sub_mean$Precip_Slope
test1$slope_dif <- test1$trendy_slope - test1$Slope

ggplot(test1, aes(x = CRU_Aridity_P_PET, y = slope_dif)) + geom_point() + geom_smooth(method = "lm")

t.test(test1$slope_dif) # trendy has significantly more positive slopes than observations overall
# stays signficantly positive even if LPX is removed?

t.test(subset(test1, subset = CRU_Aridity_P_PET >= 0.65, select = slope_dif)) # trendy slopes are greater for >= 0.65 AI
# still true even if LPX is removed?

t.test(subset(test1, subset = CRU_Aridity_P_PET < 0.65, select = slope_dif)) # but not for < 0.65 AI!
# still true even if LPX is removed?


#####
trendy_wet_slopes <- subset(flx_trendy, subset = CRU_Aridity_P_PET >= 0.65 & Source == "TRENDY", select = Slope)
length(which(trendy_wet_slopes < 0)) # number of sites with negative slopes

trendy_dry_slopes <- subset(flx_trendy, subset = CRU_Aridity_P_PET < 0.65 & Source == "TRENDY", select = Slope)
length(which(trendy_dry_slopes > 0)) 

#####
# TRENDY version of Figure 1

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

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

trendy_plot_data <- flx_slope_merged_precip_spring2

p_spring_slope <- ggplot(data = trendy_plot_data, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  #geom_smooth(data = trendy_plot_models, aes(x = CRU_Aridity_P_PET, y = Precip_Slope, color = "ModelName"), method = "lm", se = FALSE, formula = log.formula, size = 0.5) + # add this for individual models
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) +
  scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
trendy_plot_data$AI_threshold <- "wet"
trendy_plot_data$AI_threshold[which(trendy_plot_data$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist <- ggplot(data = trendy_plot_data) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist

p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave("figures/added_sites_gpp_precip_TRENDY_spring_cru_aridity_log10_slope_hist_AIthreshold_climatecolor.png", p_spring_slope_hist, width = 10, height = 5, units = "in")

# version with all model fits?
p_trmod_spring_slope <- ggplot(data = trendy_plot_models, aes(x = CRU_Aridity_P_PET, y = Precip_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  #geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_point(aes(fill = ModelName), color = "black", size = 1, shape = 21) +
  #geom_point(aes(color = ModelName), size = 1, alpha = 0.5) +
  geom_smooth(aes(color = ModelName), method = "lm", se = FALSE, formula = log.formula, size = 0.75) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]") + #, color = "Model") + #fill = "Climate") +
  #scale_color_discrete(guide = "none") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  theme(legend.position = c(0.85, 0.7), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_trmod_spring_slope

# version with all model fits in the background!
p_spring_slope_allmodels <- ggplot(data = trendy_plot_data, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(data = trendy_plot_models, aes(x = CRU_Aridity_P_PET, y = Precip_Slope, group = ModelName), 
              method = "lm", se = FALSE, formula = log.formula, size = 0.5, color = "gray80") + # add this for individual models
  geom_smooth(data = trendy_plot_data, aes(x = CRU_Aridity_P_PET, y = Slope),
              method = "lm", color = "black", se = FALSE, formula = log.formula) +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", 
       y = bquote('GPP ~ Precip slope [g C' ~m^-2 *' / mm]'), 
       fill = "Climate") +
  scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_allmodels

p_spring_slope_allmodels_hist <- ggarrange(p_spring_slope_allmodels, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave("figures/added_sites_gpp_precip_TRENDY_spring_cru_aridity_log10_slope_allmodels_hist_AIthreshold_climatecolor.png", p_spring_slope_allmodels_hist, width = 10, height = 5, units = "in")

#####
# comparing individual models to trendy data
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "CABLE")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "CLASS-CTEM")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "CLM")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "DLEM")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "ISAM")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "JSBACH")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "JULES")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "LPJ")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "LPJ-GUESS")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "LPX")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "ORCHIDEE")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "ORCHIDEE-MICT")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "VEGAS")])
t.test(trendy_plot_models_wetsites$Precip_Slope[which(trendy_plot_models_wetsites$ModelName == "VISIT")])


# test to see if they are signficantly higher than the EC mean
flx_slope_merged_precip_spring_wet <- subset(flx_slope_merged_precip_spring, CRU_Aridity_P_PET >= 0.65)
flx_trendy_all_models_wet <- merge(flx_slope_merged_precip_spring_wet, subset(trendy_plot_models_wetsites, select = c(SITE_ID, Precip_Slope, ModelName)), by = "SITE_ID")
flx_trendy_all_models_wet$slope_dif <- flx_trendy_all_models_wet$Precip_Slope - flx_trendy_all_models_wet$Slope # TRENDY - EC, positive means TRENDY is greater

t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "CABLE")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "CLASS-CTEM")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "CLM")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "DLEM")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "ISAM")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "JSBACH")]) # not significantly different, p = 0.15
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "JULES")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "LPJ")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "LPJ-GUESS")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "LPX")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "ORCHIDEE")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "ORCHIDEE-MICT")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "VEGAS")])
t.test(flx_trendy_all_models_wet$slope_dif[which(flx_trendy_all_models_wet$ModelName == "VISIT")])
# all other models are significantly greater

#####
#
p_spring_slope2_hist <- ggarrange(p_spring_slope, p_trmod_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2, 2, 1))
p_spring_slope2_hist

p_spring_slope2_hist_boxplots <- ggarrange(p_spring_slope_allmodels, p_spring_hist, p_flx_trendy_mean, p_trendy_plot_models, nrow = 2, ncol = 2, labels = "auto", widths = c(2, 1, 2, 2))
p_spring_slope2_hist_boxplots
# maybe too much going on in this multiplot? need to think about options

#####
# Plotting TRENDY model data relationships, need to output these later...

trendy_sub_all <- subset(trendy_slope_merged_spring, select = c(CRU_Aridity_P_PET, Precip_Slope, Ta_Slope, PAR_Slope))
trendy_sub_all_mean <- aggregate(trendy_sub_all, by = list(trendy_slope_merged_spring$SITE_ID), FUN = mean)
colnames(trendy_sub_all_mean)[1] <- "SITE_ID"

# precip relationship
fit_trendy_spring_precip <- lm(trendy_sub_all_mean$Precip_Slope ~ log10(trendy_sub_all_mean$CRU_Aridity_P_PET))

# ta relationship
fit_trendy_spring_ta <- lm(trendy_sub_all_mean$Ta_Slope ~ log10(trendy_sub_all_mean$CRU_Aridity_P_PET))

# par relationship
fit_trendy_spring_par <- lm(trendy_sub_all_mean$PAR_Slope ~ log10(trendy_sub_all_mean$CRU_Aridity_P_PET))

ggplot(trendy_sub_all_mean, aes(x = CRU_Aridity_P_PET, y = Precip_Slope)) +
  geom_point()

ggplot(trendy_sub_all_mean, aes(x = CRU_Aridity_P_PET, y = Ta_Slope)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = log.formula)

ggplot(trendy_sub_all_mean, aes(x = CRU_Aridity_P_PET, y = PAR_Slope)) +
  geom_point() +
  geom_smooth(method = "lm", formula = log.formula)
