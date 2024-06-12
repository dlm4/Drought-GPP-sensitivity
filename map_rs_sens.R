
library(mapproj)
library(ggpubr)
library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/outputs")
model_sens_precip_modis <- read.csv("MODIS_2001_2016gppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_detrend.csv")
model_sens_precip_gosif <- read.csv("GOSIF_GPP_2000_2016gppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_detrend.csv")
model_sens_precip_fluxcom_rs <- read.csv("FLUXCOM_RS_2001_2016gppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_detrend.csv")
model_sens_precip_fluxcom_rs_meteo <- read.csv("FLUXCOM_RS_METEO_1992_2016gppproc_met_CABLE_S2_precip_ta_par_df_spring_slope_se_detrend.csv")

model_sens_precip_modis$Index <- 1:nrow(model_sens_precip_modis)
model_sens_precip_gosif$Index <- 1:nrow(model_sens_precip_gosif)
model_sens_precip_fluxcom_rs$Index <- 1:nrow(model_sens_precip_fluxcom_rs)
model_sens_precip_fluxcom_rs_meteo$Index <- 1:nrow(model_sens_precip_fluxcom_rs_meteo)

model_sens_precip_modis_selected <- subset(model_sens_precip_modis, select = c("Lon", "Lat", "Aridity", "MODIS_LC", "Precip_Slope", "Index"))
colnames(model_sens_precip_modis_selected)[colnames(model_sens_precip_modis_selected) == "Precip_Slope"] <- "Precip_Slope_MODIS"

model_sens_precip_gosif_selected <- subset(model_sens_precip_gosif, select = c("Precip_Slope", "Index"))
colnames(model_sens_precip_gosif_selected)[colnames(model_sens_precip_gosif_selected) == "Precip_Slope"] <- "Precip_Slope_GOSIF"

model_sens_precip_fluxcom_rs_selected <- subset(model_sens_precip_fluxcom_rs, select = c("Precip_Slope", "Index"))
colnames(model_sens_precip_fluxcom_rs_selected)[colnames(model_sens_precip_fluxcom_rs_selected) == "Precip_Slope"] <- "Precip_Slope_FLUXCOM_RS"

model_sens_precip_fluxcom_rs_meteo_selected <- subset(model_sens_precip_fluxcom_rs_meteo, select = c("Precip_Slope", "Index"))
colnames(model_sens_precip_fluxcom_rs_meteo_selected)[colnames(model_sens_precip_fluxcom_rs_meteo_selected) == "Precip_Slope"] <- "Precip_Slope_FLUXCOM_RS_METEO"

#put all data frames into list
df_list <- list(model_sens_precip_modis_selected, model_sens_precip_gosif_selected, model_sens_precip_fluxcom_rs_selected, model_sens_precip_fluxcom_rs_meteo_selected)      

#merge all data frames together, something is up with this indexing...
model_sens_precip <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')
countriesmap <- ne_countries(scale = 'medium', returnclass = 'sf')

latlongrid <- model_sens_precip
latlongrid <- subset(latlongrid, Lat > 30)

# land cover filtering from MODIS
latlongrid_sub <- subset(latlongrid, MODIS_LC %in% 1:14) # keep everything that isn't water or barren

latlongrid_sub$precip_cut_modis <- cut(latlongrid_sub$Precip_Slope_MODIS,
                                       breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                       labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]",
                                                    "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                       include.lowest = TRUE)

latlongrid_sub$precip_cut_gosif <- cut(latlongrid_sub$Precip_Slope_GOSIF,
                                       breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                       labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]",
                                                  "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                       include.lowest = TRUE)

latlongrid_sub$precip_cut_fluxcom_rs <- cut(latlongrid_sub$Precip_Slope_FLUXCOM_RS,
                                       breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                       labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]",
                                                  "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                       include.lowest = TRUE)

latlongrid_sub$precip_cut_fluxcom_rs_meteo <- cut(latlongrid_sub$Precip_Slope_FLUXCOM_RS_METEO,
                                       breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                       labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]",
                                                  "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                       include.lowest = TRUE)

crs_robin2 <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(1,2), crs = crs(coastlinemap)) # make sf object with points with lat lon crs from coastline map default
latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(2,3), crs = crs(coastlinemap)) # make sf object with points with lat lon crs from coastline map default
latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf() # make 0.5 degree square grid object for lat long extent, offset corner to line up centers
grid_intersect <- st_intersects(latlongrid_sub_sf_grid2, latlongrid_sub_sf) # which grid cells intersect sf points
grid_new_inds <- lengths(grid_intersect) > 0 # subset grid indices that intersect
grid_new <- latlongrid_sub_sf_grid2[grid_new_inds,] # get new grid object polygon sf
latlongrid_sub_poly <- st_set_geometry(latlongrid_sub, st_as_sfc(grid_new)) # swap in grid polygons for geometry in sf

theme_robin_map <- theme(legend.position = c(0.98, 0.6),
                         panel.background = element_blank(), panel.border = element_rect(color = NA, fill = NA),
                         panel.grid = element_line(color = "gray80", size = 0.1), panel.ontop = TRUE,
                         legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
                         legend.background = element_blank(),
                         axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7), axis.ticks = element_blank(),
                         legend.key.width = unit(0.5, "mm"), legend.key.height = unit(1, "mm"),
                         title = element_text(size=7))

map_modis <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(aes(fill = precip_cut_modis, color = precip_cut_modis), size = 0.01) + #, 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_modis)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_modis)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_modis)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_modis)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin2, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = "MODIS MOD17") +
  theme_robin_map

map_gosif <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(aes(fill = precip_cut_gosif, color = precip_cut_gosif), size = 0.01) + #, 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_gosif)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_gosif)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_gosif)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_gosif)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin2, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = "GOSIF GPP") +
  theme_robin_map

map_fluxcom_rs <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(aes(fill = precip_cut_fluxcom_rs, color = precip_cut_fluxcom_rs), size = 0.01) + #, 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin2, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = "FLUXCOM RS") +
  theme_robin_map

map_fluxcom_rs_meteo <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(aes(fill = precip_cut_fluxcom_rs_meteo, color = precip_cut_fluxcom_rs_meteo), size = 0.01) + #, 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs_meteo)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs_meteo)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs_meteo)), breaks = rev(levels(latlongrid_sub_poly$precip_cut_fluxcom_rs_meteo)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin2, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  labs(title = "FLUXCOM RS METEO ERA5") +
  theme_robin_map

#ggsave("map_modis.eps", map_modis, width = 120, height = 30, units = "mm")

# MODIS, FLUXCOM RS, FLUXCOM RS+METEO, GOSIF GPP

combined_maps_rs <- ggarrange(map_modis, map_gosif, map_fluxcom_rs, map_fluxcom_rs_meteo, nrow = 4, labels = "auto", font.label = list(size = 8))
ggsave("combined_maps_rs.eps", combined_maps_rs, width = 120, height = 133, units = "mm")
