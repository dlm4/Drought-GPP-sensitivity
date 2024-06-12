
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

#setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data")
slope_data <- read_csv("combined_source_ec_univariate_slopes2.csv") # version with corr

flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")

flx_slope_merged <- merge(slope_data, flx_site_info, by.y = "SITE_ID")


# Just keep spring and precip for initial test
flx_slope_merged_spring_precip <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum")

# get numbers for certain ranges of aridity = 0.65 threshold
sub1 <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET >= 0.65)
sub1_negslope <- subset(sub1, Slope < 0)
sub2  <- subset(flx_slope_merged_spring_precip, CRU_Aridity_P_PET < 0.65)
sub2_posslope <- subset(sub2, Slope > 0)

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x

# mean annual temp and mean annual precip
ggplot(flx_slope_merged_spring_precip, aes(x = MAP, y = MAT)) + 
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) + 
  labs(x = "Mean Annual Precipitation (mm)", y = "Mean Annual Temperature (\u00b0C)") +
  theme_bw()
ggsave("figures/added_sites_mat_map.png", width = 6, height = 5, units = "in")

# map of flux sites
# coastline map from rnaturalearthdata, simpler than country boundaries
coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')

map_sites <- ggplot(flx_slope_merged_spring_precip) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
  geom_point(aes(x = LOCATION_LONG, y = LOCATION_LAT, fill = Climate), color = "black", shape = 21, size = 1.5) +
  #geom_text_repel(aes(x = LOCATION_LONG, y = LOCATION_LAT, label = SITE_ID, color = Climate), size = 1.5, max.overlaps = 40, bg.color = "white", bg.r = 0.05) +
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  scale_color_discrete(guide = "none") +
  theme(legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
ggsave("figures/map_sites.png", map_sites, width = 10, height = 4, units = "in")

# export shapefile for mapping
allsites_sub <- select(flx_slope_merged_spring_precip, c("SITE_ID", "LOCATION_LAT", "LOCATION_LONG"))

allsites_sub_sf <- st_as_sf(allsites_sub, agr = "SITE_ID", coords = c("LOCATION_LONG", "LOCATION_LAT")) # LONG is x, LAT is y
allsites_sub_sf_ordered <- allsites_sub_sf[order(allsites_sub_sf$SITE_ID),]
write_sf(allsites_sub_sf_ordered, "figures/combined_source_site_locations.shp")



#####

# Spring only
p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels.png", p_spring_slope, width = 6, height = 5, units = "in")

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip) +
  geom_histogram(aes(y = Slope), binwidth = 0.2, color = "black", fill = "gray80") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist

p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist.png", p_spring_slope_hist, width = 9, height = 5, units = "in")

#####

# New version with boundary line of AI = 0.65 for wet vs dry
# Spring only
p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels.png", p_spring_slope, width = 6, height = 5, units = "in")

#lm(data = flx_slope_merged_spring_precip, Slope ~ log10(CRU_Aridity_P_PET))

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_precip$AI_threshold <- "wet"
flx_slope_merged_spring_precip$AI_threshold[which(flx_slope_merged_spring_precip$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist

p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold.png", p_spring_slope_hist, width = 10, height = 5, units = "in")

#####
# try only for sites with at least x years of data, loop and plot

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

table(flx_slope_merged_spring_precip$Npts)

for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)
  
  title_phrase <- paste(npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")
  
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    #geom_smooth(method = "lm", color = "black", formula = log.formula) +
    #geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + # error bar on slope?
    geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
    #geom_text_repel(aes(label = SITE_ID), size = 2.5, max.overlaps = 50, color = "gray20", bg.color = "white", bg.r = 0.05) +
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
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate",
         title = title_phrase) +
    #scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  # vertical histogram of slopes as Trevor suggested, combine with ggpubr
  flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
  flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
  p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
    geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
    coord_cartesian(ylim = c(-1.75, 2.75)) +
    labs(y = "", fill = "Aridity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_hist
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
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
    coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
}


# 10 year version with error bars and labels
npts_min <- 10
npts_min_text <- as.character(npts_min)
if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}

flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)

title_phrase <- paste(npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")

p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  #geom_smooth(method = "lm", color = "black", formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + # error bar on slope?
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID), size = 1.25, color = "gray20", max.overlaps = 30, bg.color = "white", bg.r = 0.05,
                  segment.color = "gray20", segment.size = 0.2, fontface = "bold") +
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
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "Climate",
       title = title_phrase) +
  #scale_color_discrete(guide = "none") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_hist

p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_labels_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist, width = 10, height = 5, units = "in")

p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = IGBP)) + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID), size = 1.25, color = "gray20", max.overlaps = 30, bg.color = "white", bg.r = 0.05,
  #                segment.color = "gray20", segment.size = 0.2, fontface = "bold") +
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
  scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
  labs(x = "Annual Aridity (P/PET)", y = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]"), fill = "IGBP",
       title = title_phrase) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope

p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_errorbar_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_hist, width = 10, height = 5, units = "in")

#####
# log 10 scaled x

p_spring_slope_log10x <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_errorbar(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE, color = Climate)) + # error bar on slope?
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity_P_PET,
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
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_fill_manual(breaks = climate_names, values = climate_colors) +
  scale_color_manual(breaks = climate_names, values = climate_colors, guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope_log10x

p_spring_slope_log10x_hist <- ggarrange(p_spring_slope_log10x, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave(paste("figures/min_npts_looping/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_log10x_errorbar_labels_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
       p_spring_slope_log10x_hist, width = 10, height = 5, units = "in")


#####
# Version with log10 scaled x and "linear" trend

p_spring_slope_log10x <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity_P_PET,
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
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  scale_x_continuous(trans = "log10", breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, 4)) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope_log10x
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels.png", p_spring_slope, width = 6, height = 5, units = "in")

summary(lm(data = flx_slope_merged_spring_precip, Slope ~ log(CRU_Aridity_P_PET)))

# vertical histogram of slopes as Trevor suggested, combine with ggpubr
flx_slope_merged_spring_precip$AI_threshold <- "wet"
flx_slope_merged_spring_precip$AI_threshold[which(flx_slope_merged_spring_precip$CRU_Aridity_P_PET < 0.65)] <- "dry"
p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip) +
  geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
  coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(y = "", fill = "Aridity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_hist

p_spring_slope_hist <- ggarrange(p_spring_slope_log10x, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_log10x_hist_AIthreshold.png", p_spring_slope_hist, width = 10, height = 5, units = "in")


#####

# Need to figure out a way to show site names and limited trends

table(flx_slope_merged_spring_precip$Climate, flx_slope_merged_spring_precip$IGBP)

# show different versions with site names parsed out to be able to show all of them
flx_slope_merged_spring_precip$Climate_sub <- substr(flx_slope_merged_spring_precip$Climate, 1, 1)
ClimateB <- subset(flx_slope_merged_spring_precip, Climate_sub == "B")
ClimateC <- subset(flx_slope_merged_spring_precip, Climate_sub == "C")
ClimateD <- subset(flx_slope_merged_spring_precip, Climate_sub == "D")

p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  geom_text_repel(data = ClimateB, aes(label = SITE_ID), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.5, 2.5)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels_climateB.png", p_spring_slope, width = 6, height = 5, units = "in")

p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  geom_text_repel(data = ClimateC, aes(label = SITE_ID), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.5, 2.5)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels_climateC.png", p_spring_slope, width = 6, height = 5, units = "in")

p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  geom_text_repel(data = ClimateD, aes(label = SITE_ID), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
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
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.5, 2.5)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_slope
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_labels_climateD.png", p_spring_slope, width = 6, height = 5, units = "in")

#####
# pearson
p_spring_corrp <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = PearsonR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = PearsonR,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1, 1)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip Pearson Correlation (R)", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_corrp
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_pearson.png", p_spring_corrp, width = 6, height = 5, units = "in")
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_pearson_labels.png", p_spring_corrp, width = 6, height = 5, units = "in")

# spearman
p_spring_corrs <- ggplot(data = flx_slope_merged_spring_precip, aes(x = CRU_Aridity_P_PET, y = SpearmanR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = SpearmanR,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1, 1)) + # ylim = c(-1.3, 1.3)
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip Spearman Correlation (R)", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(#legend.position = c(0.85, 0.8), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_spring_corrs
ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_spearman.png", p_spring_corrs, width = 6, height = 5, units = "in")
#ggsave("figures/added_sites_gpp_precip_ec_spring_cru_aridity_log10_spearman_labels.png", p_spring_corrs, width = 6, height = 5, units = "in")



# multiplot
flx_slope_merged_plot <- flx_slope_merged

# Multiplot
flx_slope_merged_plot$Season <- factor(flx_slope_merged_plot$Season, levels = c("Spring", "Summer", "Fall"), labels = c("Spring", "Summer", "Fall"))
flx_slope_merged_plot$Met_Var <- factor(flx_slope_merged_plot$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"), 
                           labels = c("GPP ~ Precip [g C / mm]", "GPP ~ PAR [g C / MJ]", "GPP ~ Ta [g C / \u00b0C]")) # label with degree symbol!

# log10
p_slopes_all <- ggplot(data = flx_slope_merged_plot, aes(x = CRU_Aridity_P_PET, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = Slope,
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
ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10.png", p_slopes_all, width = 10, height = 8, units = "in")
#ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_labels.png", p_slopes_all, width = 10, height = 8, units = "in")

# Reset labels for correlation setup
flx_slope_merged_plot <- flx_slope_merged
flx_slope_merged_plot$Season <- factor(flx_slope_merged_plot$Season, levels = c("Spring", "Summer", "Fall"), labels = c("Spring", "Summer", "Fall"))
flx_slope_merged_plot$Met_Var <- factor(flx_slope_merged_plot$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"), 
                                        labels = c("GPP ~ Precip", "GPP ~ PAR", "GPP ~ Ta")) # label with degree symbol!
# Pearson
p_corrp_all <- ggplot(data = flx_slope_merged_plot, aes(x = CRU_Aridity_P_PET, y = PearsonR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = PearsonR,
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
ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_pearson.png", p_corrp_all, width = 10, height = 8, units = "in")
#ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_pearson_labels.png", p_corrp_all, width = 10, height = 8, units = "in")

# Spearman
p_corrs_all <- ggplot(data = flx_slope_merged_plot, aes(x = CRU_Aridity_P_PET, y = SpearmanR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity_P_PET),
                   y = SpearmanR,
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
  labs(x = "Annual Aridity (P/PET)", y = "Spearman Correlation (R)", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_corrs_all
ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_spearman.png", p_corrs_all, width = 10, height = 8, units = "in")
#ggsave("figures/added_sites_gpp_slopes_grid_ec_cru_aridity_log10_spearman_labels.png", p_corrs_all, width = 10, height = 8, units = "in")
