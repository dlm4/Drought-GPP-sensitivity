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
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")

# compare precip and par sensitivities
flx_slope_merged_sub <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10)

#flx_site_info_10 <- flx_site_info[which(flx_site_info$SITE_ID %in% flx_slope_merged_sub$SITE_ID),]
#write.csv(flx_site_info_10, "../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_10springyrs.csv", row.names = F)

flx_p_f <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10 & Met_Var == "P_F_sum")
flx_par_f <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10 & Met_Var == "PAR_F_sum")
flx_ta_f <- subset(flx_slope_merged, subset = Season == "Spring" & Npts >= 10 & Met_Var == "TA_F_mean")

plot(flx_p_f$Slope, flx_par_f$Slope)
plot(flx_p_f$Slope, flx_ta_f$Slope)
plot(flx_ta_f$Slope, flx_par_f$Slope)

cor(flx_ta_f$Slope, flx_par_f$Slope)
cor(flx_p_f$Slope, flx_ta_f$Slope)
cor(flx_p_f$Slope, flx_par_f$Slope)

neg_p_inds <- which(flx_p_f$Slope < 0)
cor(flx_p_f$Slope[neg_p_inds], flx_ta_f$Slope[neg_p_inds])
cor(flx_p_f$Slope[neg_p_inds], flx_par_f$Slope[neg_p_inds])

# PAR strongly correlated to air temperature, which is why it has no additional explanatory power...!

# boxplots, igbp and climate
ggplot(flx_slope_merged_sub, aes(x = IGBP, y = Slope)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(fill = Climate), color = "black", shape = 21, size = 1.5) +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  facet_wrap(~Met_Var, nrow = 3, scales = "free") +
  theme_bw()

ggplot(flx_slope_merged_sub, aes(x = Climate, y = Slope)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(fill = IGBP), color = "black", shape = 21, size = 1.5) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  facet_wrap(~Met_Var, nrow = 3, scales = "free") +
  theme_bw()

# MAT
ggplot(flx_slope_merged_sub, aes(x = MAT, y = Slope)) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(fill = Climate), color = "black", shape = 21, size = 1.5) +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  facet_wrap(~Met_Var, nrow = 3, scales = "free") +
  theme_bw()

# MAP
ggplot(flx_slope_merged_sub, aes(x = MAP, y = Slope)) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(fill = Climate), color = "black", shape = 21, size = 1.5) +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  facet_wrap(~Met_Var, nrow = 3, scales = "free") +
  theme_bw()

# ordered slope barplot
flx_p_f$SITE_ID_order <- reorder(flx_p_f$SITE_ID, -flx_p_f$Slope)
ggplot(flx_p_f) +
  geom_col(aes(x = SITE_ID_order, y = Slope, fill = IGBP), color = "gray20") +
  geom_errorbar(aes(x = SITE_ID_order, ymin = Slope - Slope_SE, ymax = Slope + Slope_SE), width = 0.3, color = "gray20") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))

# Seasonal median?
warm_sites <- flx_slope_merged_sub$SITE_ID[which(flx_slope_merged_sub$Met_Var_Median > 5 & flx_slope_merged_sub$Met_Var == "TA_F_mean")]
ggplot(flx_slope_merged_sub[which(flx_slope_merged_sub$SITE_ID %in% warm_sites),], aes(x = Met_Var_Median, y = Slope)) + 
  geom_point() + 
  facet_wrap(~Met_Var, nrow = 3, scales = "free") +
  #scale_color_manual(breaks = climate_names, values = climate_colors) +
  theme_bw()
