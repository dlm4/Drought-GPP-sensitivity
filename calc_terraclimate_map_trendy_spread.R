# Estimating model spread in maps, map for each model and SD values

# Comment from Reviewer #1

# offshoot of calc_terraclimate_aridity_map2.R

# this doesn't actually have any terraclimate data in it, but it's in the terraclimate directory because that's where the mapping was happening

library(ncdf4)
library(tidyverse)
library(R.matlab)
library(mapproj)
library(ggpubr)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_allmodel_slope_error_1992_2016_detrend")
# static consistent color scale
myColors <- rev(brewer.pal(11, "RdBu"))
names(myColors) <- c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                     "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3")
fillScale <- scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = myColors) 

#output_path <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4_maps_30N/" # need last /
file_list <- list.files()

# only doing spring, no need for loop over season
season <- "spring"
season_searchstring <- paste("*", season, "*", sep = "")
file_inds <- grep(pattern = season_searchstring, file_list)

# loop and load all files and append together with cbind
for (i in 1:length(file_inds)){
  file_name <- file_list[file_inds[i]]
  print(file_name)
  model_sens <- read_csv(file_name, show_col_types = FALSE)
  model_sens_precip <- model_sens %>% dplyr::select(Lon, Lat, Precip_Slope)
  file_name_split <- unlist(strsplit(file_name, "_"))
  model_name <- paste(file_name_split[1])
  colnames(model_sens_precip)[3] <- paste(model_name, "_Precip_Slope", sep = "")
  
  if (i == 1){
    model_sens_precip_merge <- model_sens_precip
  } else {
    model_sens_precip_merge <- cbind.data.frame(model_sens_precip_merge, model_sens_precip[,3])
  }
}

# Remove SDGVM! from average
model_sens_precip_merge$SDGVM_Precip_Slope <- NA

# This is the mean of the slopes, whereas the other comparison is the slope of the mean GPP
# mean(GPPs)/precip vs. mean(GPPs/precip)

# Get mean and SD across models
Mean_Precip_Slope <- apply(model_sens_precip_merge[, 3:ncol(model_sens_precip_merge)], MARGIN = 1, FUN = mean, na.rm = TRUE)
SD_Precip_Slope <- apply(model_sens_precip_merge[, 3:ncol(model_sens_precip_merge)], MARGIN = 1, FUN = sd, na.rm = TRUE)
model_sens_precip_mean <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope, SD_Precip_Slope) %>% na.omit() # bind and omit NAs

# coastline map from rnaturalearthdata, simpler than country boundaries
coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')
countriesmap <- ne_countries(scale = 'medium', returnclass = 'sf')

latlongrid <- model_sens_precip[,1:2]
latlongrid <- subset(latlongrid, Lat > 30)

model_sens_precip_mean_all <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope, SD_Precip_Slope) # across grid cells
model_sens_precip_mean_30N <- subset(model_sens_precip_mean_all, Lat > 30)
latlongrid$trendy_gpp_sens <- model_sens_precip_mean_30N$Mean_Precip_Slope # get trendy predicted slopes
latlongrid$trendy_gpp_sd <- model_sens_precip_mean_30N$SD_Precip_Slope # get trendy predicted slopes SD

latlongrid$trendy_gpp_sens_cut <- cut(latlongrid$trendy_gpp_sens,
                                      breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                      labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                 "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                      include.lowest = TRUE)

latlongrid$trendy_gpp_sd_cut <- cut(latlongrid$trendy_gpp_sd,
                                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1e32),
                                      labels = c("(0, 0.1]", "(0.1, 0.2]", "(0.2, 0.3]", "(0.3, 0.4]", "(0.4, 0.5]", "(0.5, 0.6]", "(0.6, 0.7]", "(0.7, 0.8]", ">0.8"),
                                      include.lowest = TRUE)

# Need to include MODIS land cover as a vegetated land filter
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/")
modis_pft <- R.matlab::readMat("MODIS_PFTS_Gridded.mat")
# 360 rows, 720 columns, need to swap?
#modis_pft$landCoverPadded
modis_pft <- aperm(modis_pft$landCoverPadded, c(2,1)) # swap rows and columns
modis_pft <- modis_pft[, rev(seq_len(ncol(modis_pft)))] # flip columns

modis_pft_sub <- modis_pft[, 241:360]

latlongrid$modis <- c(modis_pft_sub)

latlongrid_sub <- subset(latlongrid, modis %in% 1:14) # keep everything that isn't water or barren

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/maps")

# Robinson projection
crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"
#crs_robin2 <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(1,2), crs = crs(coastlinemap)) # make sf object with points with lat lon crs from coastline map default
latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf() # make 0.5 degree square grid object for lat long extent, offset corner to line up centers
grid_intersect <- st_intersects(latlongrid_sub_sf_grid2, latlongrid_sub_sf) # which grid cells intersect sf points
grid_new_inds <- lengths(grid_intersect) > 0 # subset grid indices that intersect
grid_new <- latlongrid_sub_sf_grid2[grid_new_inds,] # get new grid object polygon sf
latlongrid_sub_poly <- st_set_geometry(latlongrid_sub, st_as_sfc(grid_new)) # swap in grid polygons for geometry in sf

theme_robin_map_2 <- theme(legend.position = c(0.98, 0.6), # shift legend over a little more
                         panel.background = element_blank(), panel.border = element_rect(color = NA, fill = NA),
                         panel.grid = element_line(color = "gray80", size = 0.1), panel.ontop = TRUE,
                         legend.key = element_blank(), legend.title = element_text(size = 5), legend.text = element_text(size = 5),# reduced legend title size
                         legend.background = element_blank(),
                         axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7), axis.ticks = element_blank(), 
                         legend.key.width = unit(0.5, "mm"), legend.key.height = unit(1, "mm"),
                         title = element_text(size = 6, color = "black"))

map_trendy <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = trendy_gpp_sens_cut, color = trendy_gpp_sens_cut), size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  theme_robin_map

ggsave("map_test.eps", map_trendy, width = 120, height = 40, units = "mm")

# SD
map_trendy_sd <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = trendy_gpp_sd_cut, color = trendy_gpp_sd_cut), size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sd_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sd_cut)), values = rev(brewer.pal(9, "PuRd"))) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sd_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sd_cut)), values = rev(brewer.pal(9, "PuRd"))) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = "Standard Deviation") + 
  theme_robin_map_2

ggsave("map_test_sd.eps", map_trendy_sd, width = 120, height = 30, units = "mm")

# Set up original map as function for all models
#####

mapTrendyModel <- function(selected_col, model_name){
  

latlongrid <- model_sens_precip[,1:2]
latlongrid <- subset(latlongrid, Lat > 30)

model_sens_precip_merge_30N <- subset(model_sens_precip_merge, Lat > 30)
latlongrid$trendy_gpp_sens <- selected_col # model_sens_precip_merge_30N$CABLE_Precip_Slope # get trendy predicted slopes (CABLE)

latlongrid$trendy_gpp_sens_cut <- cut(latlongrid$trendy_gpp_sens,
                                      breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                      labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                 "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                      include.lowest = TRUE)

latlongrid$modis <- c(modis_pft_sub)
latlongrid_sub <- subset(latlongrid, modis %in% 1:14) # keep everything that isn't water or barren

latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(1,2), crs = crs(coastlinemap)) # make sf object with points with lat lon crs from coastline map default
latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf() # make 0.5 degree square grid object for lat long extent, offset corner to line up centers
grid_intersect <- st_intersects(latlongrid_sub_sf_grid2, latlongrid_sub_sf) # which grid cells intersect sf points
grid_new_inds <- lengths(grid_intersect) > 0 # subset grid indices that intersect
grid_new <- latlongrid_sub_sf_grid2[grid_new_inds,] # get new grid object polygon sf
latlongrid_sub_poly <- st_set_geometry(latlongrid_sub, st_as_sfc(grid_new)) # swap in grid polygons for geometry in sf

crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"
theme_robin_map_2 <- theme(legend.position = c(0.98, 0.6), # shift legend over a little more
                           panel.background = element_blank(), panel.border = element_rect(color = NA, fill = NA),
                           panel.grid = element_line(color = "gray80", size = 0.1), panel.ontop = TRUE,
                           legend.key = element_blank(), legend.title = element_text(size = 5), legend.text = element_text(size = 5),# reduced legend title size
                           legend.background = element_blank(),
                           axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7), axis.ticks = element_blank(), 
                           legend.key.width = unit(0.5, "mm"), legend.key.height = unit(1, "mm"),
                           title = element_text(size = 6, color = "black"))

map_trendy <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = trendy_gpp_sens_cut, color = trendy_gpp_sens_cut), size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = model_name) + #labs(title = "CABLE") + 
  theme_robin_map_2

ggsave(paste("map_", model_name,".eps", sep=""), map_trendy, width = 120, height = 30, units = "mm")
}

mapTrendyModel(model_sens_precip_merge_30N$CABLE_Precip_Slope, "CABLE")
mapTrendyModel(model_sens_precip_merge_30N$`CLASS-CTEM_Precip_Slope`, "CLASS-CTEM")
mapTrendyModel(model_sens_precip_merge_30N$CLM_Precip_Slope, "CLM")
mapTrendyModel(model_sens_precip_merge_30N$DLEM_Precip_Slope, "DLEM")
mapTrendyModel(model_sens_precip_merge_30N$ISAM_Precip_Slope, "ISAM")
mapTrendyModel(model_sens_precip_merge_30N$JSBACH_Precip_Slope, "JSBACH")
mapTrendyModel(model_sens_precip_merge_30N$JULES_Precip_Slope, "JULES")
mapTrendyModel(model_sens_precip_merge_30N$LPJ_Precip_Slope, "LPJ")
mapTrendyModel(model_sens_precip_merge_30N$`LPJ-GUESS_Precip_Slope`, "LPJ-GUESS")
mapTrendyModel(model_sens_precip_merge_30N$LPX_Precip_Slope, "LPX")
mapTrendyModel(model_sens_precip_merge_30N$ORCHIDEE_Precip_Slope, "ORCHIDEE")
mapTrendyModel(model_sens_precip_merge_30N$`ORCHIDEE-MICT_Precip_Slope`, "ORCHIDEE-MICT")
mapTrendyModel(model_sens_precip_merge_30N$VEGAS_Precip_Slope, "VEGAS")
mapTrendyModel(model_sens_precip_merge_30N$VISIT_Precip_Slope, "VISIT")


#####

# SD analysis

igbp_names_full <- c("ENF", "EBF", "DNF", "DBF", "MF", 
                     "CSH", "OSH", "WSA", "SAV", "GRA", 
                     "WET", "CRO", "URB", "CNV")
# igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
#                  "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
#                  "#2a6495")

library(plyr)
library(dplyr) # need to always load dplyr after plyr
library(Hmisc) # for weighted variance (base R already has weighted mean)
#ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = weighted.mean(x$est_gpp_sens, as.numeric(x$area_m))))

# do mid 95% clip to remove extreme values that will otherwise mess with the regression
latlongrid_sub_poly2 <- latlongrid_sub_poly
latlongrid_sub_poly2$area_m <- st_area(latlongrid_sub_poly2) # calculate area
latlongrid_sub_poly2$trendy_gpp_sd_mid95 <- latlongrid_sub_poly2$trendy_gpp_sd

for(i in 1:length(unique(latlongrid_sub_poly2$modis))){
  pft_inds <- which(latlongrid_sub_poly2$modis == i)
  
  trendy_quantiles <- quantile(latlongrid_sub_poly2$trendy_gpp_sd_mid95[pft_inds], probs = c(0, 0.025, 0.975, 1), na.rm = T)
  extreme_pft_trendy <- which(latlongrid_sub_poly2$trendy_gpp_sd_mid95[pft_inds] < trendy_quantiles[2] | 
                                latlongrid_sub_poly2$trendy_gpp_sd_mid95[pft_inds] > trendy_quantiles[3])
  latlongrid_sub_poly2$trendy_gpp_sd_mid95[pft_inds[extreme_pft_trendy]] <- NA
}

# trendy_gpp_sens
modis_wmean <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = wtd.mean(x$trendy_gpp_sd_mid95, as.numeric(x$area_m), na.rm = T)))
modis_wvar <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wvar = wtd.var(x$trendy_gpp_sd_mid95, as.numeric(x$area_m), na.rm = T)))
modis_weighted <- merge(modis_wmean, modis_wvar)
modis_weighted$wsd <- modis_weighted$wvar^(0.5) # take square root to get SD from variance
modis_weighted$IGBP <- factor(igbp_names_full, levels = igbp_names_full)
modis_weighted$npts <- aggregate(latlongrid_sub_poly2$modis, by = list(latlongrid_sub_poly2$modis), FUN = length)[,2]
modis_weighted$pct_area <- aggregate(as.numeric(latlongrid_sub_poly2$area_m), by = list(latlongrid_sub_poly2$modis), FUN = sum, na.rm=T)[,2]/as.numeric(sum(latlongrid_sub_poly2$area_m))*100 # to get pct
modis_weighted$input <- "SD"
modis_weighted_trendy <- modis_weighted

modis_weighted <- modis_weighted_trendy
modis_weighted_sub <- subset(modis_weighted, pct_area > 1 & IGBP != "CRO" & IGBP != "CNV")
modis_weighted_sub <- droplevels(modis_weighted_sub)
p_modis_weighted_alt <- ggplot(modis_weighted_sub, aes(x = wmean, y = IGBP, color = input)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = wmean - wsd, xmax = wmean + wsd), position = position_dodge(width = 0.5)) + 
  coord_cartesian(xlim = c(0, 0.5)) +
  scale_color_manual(values = c("gray80", "gray50")) +
  scale_y_discrete(limits = rev(levels(modis_weighted_sub$IGBP))) +
  labs(x  = bquote("g C" ~m^-2*" / mm")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
        legend.position = c(0.7, 0.97), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
p_modis_weighted_alt
ggsave("modis_pft_gpp_sens_from_map_alt_sub_SD.eps", p_modis_weighted_alt, width = 50, height = 80, units = "mm")


# mean of the SD and the SD of the SDs....