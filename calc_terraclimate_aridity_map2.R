# Use this to calculate the aridity from terraclimate at a half degree grid
# then generate plots to map the difference for EC estimated precip_slope from the TRENDY model mean for each grid cell

library(ncdf4)
library(tidyverse)
library(R.matlab)
library(mapproj)
library(ggpubr)
library(sf)

# aggregate terraclimate from 1/24 degree

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/summaries/")
tc_var <- nc_open("TerraClimate19812010_ppt.nc")
ppt <- ncvar_get(tc_var, "ppt")
nc_close(tc_var)

tc_var <- nc_open("TerraClimate19812010_pet.nc")
pet <- ncvar_get(tc_var, "pet")
nc_close(tc_var)
# this is if we were going to a 0.5 degree grid

ppt_sum <- rowSums(ppt, dims = 2)
pet_sum <- rowSums(pet, dims = 2)

#test_mat <- matrix(data = 1:8640, nrow = 8640, ncol =  4320)

coarsen24toHalf <- function(input){
  out_mat <- matrix(data = NA, nrow = 720, ncol = 360)
  # this is lazy but it's still fast enough, so it's not a bad way of doing it?
  for (i in 1:720){
    print(i)
    for (j in 1:360){
      row_start <- (i - 1)*12 + 1
      row_end <- i*12
      col_start <- (j - 1)*12 + 1
      col_end <- j*12
      
      # this only needs a single pixel to output something for now...
      datasub <- input[row_start:row_end, col_start:col_end]
      out_mat[i,j] <- mean(datasub, na.rm = T)
    }
  }
  return(out_mat)
}

ppt_sum_coarse <- coarsen24toHalf(ppt_sum)
pet_sum_coarse <- coarsen24toHalf(pet_sum)

aridity_index <- ppt_sum_coarse / pet_sum_coarse

#image(t(aridity_index), zlim = c(0, 4))

#####
# get the trendy gpp loaded

library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4")
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_allmodel_slope_error_1992_2016_detrend")
# static consistent color scale
myColors <- rev(brewer.pal(11, "RdBu"))
names(myColors) <- c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                     "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3")
#fillScale <- scale_fill_manual(name = "g C / mm", values = myColors) 
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

# Get mean across models
Mean_Precip_Slope <- apply(model_sens_precip_merge[, 3:ncol(model_sens_precip_merge)], MARGIN = 1, FUN = mean, na.rm = TRUE)
model_sens_precip_mean <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope) %>% na.omit() # bind and omit NAs

# Set ranges and labels
model_sens_precip_mean$precip_cut <- cut(model_sens_precip_mean$Mean_Precip_Slope,
                                         breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                         labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                    "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                         include.lowest = TRUE)

plot_title <- paste("GPP~Precip Slope, Model Mean - ", season, sep = "")

# coastline map from rnaturalearthdata, simpler than country boundaries
coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')
countriesmap <- ne_countries(scale = 'medium', returnclass = 'sf')
  
latlongrid <- model_sens_precip[,1:2]
latlongrid <- subset(latlongrid, Lat > 30)

aridity_index_30N <- aridity_index[, 1:120] # just get nhem 30 N
aridity_index_30N <- aridity_index_30N[,ncol(aridity_index_30N):1] # flip over colomns
latlongrid$ai <- c(aridity_index_30N) # append
latlongrid$ai_cut <- cut(latlongrid$ai, breaks = c(0, 0.3, 0.5, 0.65, 1, 2, 3, 1e32))

# test plot for alignment
# ggplot(latlongrid) +
#   geom_tile(aes(x = Lon, y = Lat, fill = ai_cut)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme_bw()

# estimated gpp sensitivity from flux tower regression, keep it to two decimal points?
latlongrid$est_gpp_sens <- -0.08 - 1.26*log10(latlongrid$ai) 

model_sens_precip_mean_all <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope) # across grid cells
model_sens_precip_mean_30N <- subset(model_sens_precip_mean_all, Lat > 30)

latlongrid$trendy_gpp_sens <- model_sens_precip_mean_30N$Mean_Precip_Slope # get trendy predicted slopes

latlongrid$trendy_gpp_sens_cut <- cut(latlongrid$trendy_gpp_sens,
                                         breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                         labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                    "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                         include.lowest = TRUE)

latlongrid$est_gpp_sens_cut <- cut(latlongrid$est_gpp_sens,
                                      breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                      labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                 "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                      include.lowest = TRUE)

# # regular mercator style plots, everywhere
# ggplot(latlongrid) +
#   geom_tile(aes(x = Lon, y = Lat, fill = trendy_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   fillScale + 
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()
# 
# ggplot(latlongrid) +
#   geom_tile(aes(x = Lon, y = Lat, fill = est_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   fillScale + 
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()

# get difference
latlongrid$dif_gpp_sens <- latlongrid$trendy_gpp_sens - latlongrid$est_gpp_sens

# set bins, may change this
latlongrid$dif_gpp_sens_cut <- cut(latlongrid$dif_gpp_sens,
                                   #breaks = c(-1e32, -0.8, -0.6, -0.4, -0.2, -0.1, 0.1, 0.2, 0.4, 0.6, 0.8, 1e32),
                                   breaks = c(-1e32, -0.9, -0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1e32),
                                   #labels = c("<-0.8", "(-0.8,-0.6]", "(-0.6,-0.4]", "(-0.1,-0.1]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                  #            "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
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

# # check modis orientation
# ggplot(latlongrid) +
#   geom_tile(aes(x = Lon, y = Lat, fill = modis)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme_bw()

# land cover filtering from MODIS
latlongrid_sub <- subset(latlongrid, modis %in% 1:14) # keep everything that isn't water or barren

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/maps")

# # Mercator versions of plots
# # the maps are vertically stretched for some reason, pixels are not equal squares?
# ggplot(latlongrid_sub) +
#   geom_tile(aes(x = Lon, y = Lat, fill = trendy_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   fillScale + 
#   #scale_fill_manual(values = rev(brewer.pal(11, "RdBu"))) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", fill = "gray90", size = 0.25) + 
#   coord_sf(xlim = c(-180, 180), ylim = c(30, 80), expand = FALSE, lims_method = "orthogonal") +
#   #coord_equal() +
#   #scale_x_continuous(name = NULL, breaks = c(-120, -60, 0, 60, 120)) + 
#   scale_y_continuous(name = NULL) +
#   theme_bw()
# #ggsave("trendy_gpp_sens.png", width = 10, height = 4, units = "in")
# 
# ggplot(latlongrid_sub) +
#   geom_tile(aes(x = Lon, y = Lat, fill = est_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   fillScale + 
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()
# #ggsave("tc_gpp_sens.png", width = 10, height = 4, units = "in")
# 
# # need to revise the color scale here...
# ggplot(latlongrid_sub) +
#   geom_tile(aes(x = Lon, y = Lat, fill = dif_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   #fillScale + 
#   scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()
# ggsave("trendy_minus_tc_gpp_sens.png", width = 10, height = 4, units = "in")
# 
# # maybe only show areas with AI >= 0.65 because the real difference isn't significant on the dry side??
# 
# latlongrid_sub2 <- subset(latlongrid, modis %in% 1:14 & ai >= 0.65) # keep everything that isn't water or barren
# ggplot(latlongrid_sub2) +
#   geom_tile(aes(x = Lon, y = Lat, fill = dif_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   #fillScale + 
#   scale_fill_manual(values = rev(brewer.pal(11, "RdBu"))) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()
# ggsave("trendy_minus_tc_gpp_sens_wetonly.png", width = 10, height = 4, units = "in")
# # try as a pct dif? would case low sensitivity areas to potentially blow up...


#####
# not ggplot?
# plot(coastlinemap$geometry)
# plot(x = latlongrid_sub$Lon, latlongrid_sub$Lat, col = latlongrid_sub$dif_gpp_sens_cut)
# lines(coastlinemap$geometry)
# 
# plot(coastlinemap_robin$geometry)
# 
# coastlinemap_robin <- st_transform(coastlinemap, crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# library(raster)
# rast <- rasterFromXYZ(subset(latlongrid_sub, select = c(Lon, Lat, modis)), crs = crs(coastlinemap))
# plot(rast)
# rast_robin <- projectRaster(from = rast, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", method = 'ngb')
# plot(rast_robin)
# # for dimensions reference, if necessary


# plot(rast_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 6969593), ext = extent(rast_robin))
# plot(coastlinemap_robin$geometry, extent = extent(rast_robin), add = TRUE)  
# plot(rast_robin)
# plot(coastlinemap_robin$geometry, add = TRUE)

#### THIS WORKS!
# Robinson projection
crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"
crs_robin2 <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(1,2), crs = crs(coastlinemap)) # make sf object with points with lat lon crs from coastline map default
latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf() # make 0.5 degree square grid object for lat long extent, offset corner to line up centers
grid_intersect <- st_intersects(latlongrid_sub_sf_grid2, latlongrid_sub_sf) # which grid cells intersect sf points
grid_new_inds <- lengths(grid_intersect) > 0 # subset grid indices that intersect
grid_new <- latlongrid_sub_sf_grid2[grid_new_inds,] # get new grid object polygon sf
latlongrid_sub_poly <- st_set_geometry(latlongrid_sub, st_as_sfc(grid_new)) # swap in grid polygons for geometry in sf

# NEW VERSION OF PLOT
ggplot(data = latlongrid_sub_poly) + 
  geom_sf(aes(fill = dif_gpp_sens_cut, color = dif_gpp_sens_cut), size = 0.1) +
  geom_sf(data = coastlinemap, color = "gray20", size = 0.2) +
  #geom_sf(data = grid_new, fill = "blue", color = NA, alpha = 0.5) + 
  #coord_sf(ylim = c(30, 80), expand = FALSE) + 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  #coord_sf(crs = crs_robin, xlim = c(-10000000, 10000000), ylim = c(2969593, 8491593), expand = FALSE) + # test smaller limits, latitude displays here
  #xlab("Longitude") + ylab("Latitude") +
  #scale_fill_continuous(guide = guide_legend()) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 30)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 10)) +
  #annotate("text", x = -16560200, y = 2969593, label = "30 \u00B0N") + 
  theme(legend.position = c(0.95, 0.6),
        #legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = NA, fill = NA),
        panel.grid = element_line(color = "gray80", size = 0.2), panel.ontop = TRUE)
#   theme(legend.position="bottom") +
#   guides(fill = guide_legend(label.position = "bottom", nrow = 1, byrow = TRUE, label.theme = element_text(angle = 90))) +
# #guides(color = guide_legend(nrow = 2, byrow = TRUE))
#   theme(legend.position = "bottom",
#     #legend.key = element_blank(), 
#     panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/maps")
#ggsave("trendy_minus_tc_gpp_sens_robin.png", width = 16, height = 5, units = "in")

#####

# maybe try getting a continents basemap that is filled
#coastlinemap_crop <- st_crop(coastlinemap, c(xmin = -180, xmax = 180, ymin = 30, ymax = 80)) # this didn't work
# new combined plots
theme_robin_map <- theme(legend.position = c(0.98, 0.6),
      panel.background = element_blank(), panel.border = element_rect(color = NA, fill = NA),
      panel.grid = element_line(color = "gray80", size = 0.1), panel.ontop = TRUE,
      legend.key = element_blank(), legend.title = element_text(size = 6), legend.text = element_text(size = 5),
      legend.background = element_blank(),
      axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7), axis.ticks = element_blank(),
      legend.key.width = unit(0.5, "mm"), legend.key.height = unit(1, "mm"))

map_est <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = est_gpp_sens_cut, color = est_gpp_sens_cut), size = 0.01) + #, 
  #geom_sf(data = coastlinemap, color = "gray50", size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$est_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$est_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +#,
                    # guide = guide_legend(direction = "horizontal", title.position = "top",
                    #                      label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                    #                      label.theme = element_text(angle = 90))) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$est_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$est_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  #coord_sf(crs = crs_robin2, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  #coord_sf(crs = crs_robin2, xlim = c(-180, 180)*111111, ylim = c(30, 80)*111111, expand = TRUE) + # full extent, try an approx version for meters to latlon, OK at equator for lon
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  # scale_x_continuous(breaks = seq(from = -180, to = 180, by = 360)) +
  # scale_y_continuous(breaks = seq(from = 28, to = 86, by = 58)) +
  theme_robin_map
#map_est
# make sure map_est has the same color scale as map_trendy for direction comparisons

map_trendy <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = trendy_gpp_sens_cut, color = trendy_gpp_sens_cut), size = 0.01) +
  #geom_sf(data = coastlinemap, color = "gray50", size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$trendy_gpp_sens_cut)), values = brewer.pal(11, "RdYlBu")) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  theme_robin_map
#map_trendy

map_dif_trendy_ec <- ggplot(data = latlongrid_sub_poly) + 
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
  geom_sf(aes(fill = dif_gpp_sens_cut, color = dif_gpp_sens_cut), size = 0.01) +
  #geom_sf(data = coastlinemap, color = "gray50", size = 0.01) +
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$dif_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$dif_gpp_sens_cut)), values = brewer.pal(11, "PuOr")) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), limits = rev(levels(latlongrid_sub_poly$dif_gpp_sens_cut)), breaks = rev(levels(latlongrid_sub_poly$dif_gpp_sens_cut)), values = brewer.pal(11, "PuOr")) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  theme_robin_map

combined_maps <- ggarrange(map_est, map_trendy, map_dif_trendy_ec, nrow = 3, labels = "auto", font.label = list(size = 8))
#ggsave("combined_maps.eps", combined_maps, width = 120, height = 100, units = "mm")
# still have a lot of arranging to do to get this to look good


#####
# Answering Trevor's question:
# How much of the vegetated land surface >30N has negative relationship between spring GPP and precipitation?
# What's the breakdown on this like?

# Will need to take each pixel that's at 0.5 degree spatial resolution and weight it by it's actual on the ground area (km2), will vary by latitude.
# As pixels get farther north, they get smaller in actual area

# # using geosphere package, areamap function
# library(geosphere)
# 
# # long/lat
# p <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
# p <- rbind(c(0,30), c(0,30.5), c(0.5, 30.5), c(0.5,30), c(0,30))
# areaPolygon(p)/10^6

latlongrid_sub_poly2 <- latlongrid_sub_poly
#sum(head(latlongrid_sub_poly2$area_m))

latlongrid_sub_poly2$area_m <- st_area(latlongrid_sub_poly2) # calculate area

# # map area, test
# map_area <- ggplot(data = latlongrid_sub_poly2) + 
#   geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) + 
#   geom_sf(aes(fill = as.numeric(area_m), color = as.numeric(area_m)), size = 0.01) +
#   coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
#   scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
#   scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
#   theme_robin_map
# map_area

map_modis <- ggplot(data = latlongrid_sub_poly2) +
  geom_sf(data = countriesmap, fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(aes(fill = as.factor(modis), color = as.factor(modis)), size = 0.01) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) + # full extent
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  scale_y_continuous(breaks = seq(from = 30, to = 80, by = 15)) +
  theme_classic()
map_modis

neg_area_ec <- sum(latlongrid_sub_poly2$area_m[which(latlongrid_sub_poly2$est_gpp_sens < 0)]) # 2.921112e+13 [m^2]
neg_area_trendy <- sum(latlongrid_sub_poly2$area_m[which(latlongrid_sub_poly2$trendy_gpp_sens < 0)]) # 1.886292e+13 [m^2]
total_area <- sum(latlongrid_sub_poly2$area_m) # 5.290185e+13 [m^2]

neg_area_ec/total_area*100 # 55.22%
neg_area_trendy/total_area*100 # 35.66%
# this is all areas that are not water, barren, or permanent snow (includes urban areas, which are only < 0.1% of the total anyway in this classification)

area_urban <- sum(latlongrid_sub_poly2$area_m[which(latlongrid_sub_poly2$modis == 13)])
area_urban/total_area*100 # 0.098%

modis_areas <- aggregate(latlongrid_sub_poly2$area_m, by = list(latlongrid_sub_poly2$modis), FUN = sum)
modis_areas$pct <- modis_areas$x/(sum(modis_areas$x))*100

#####
# What the sensitivity distribution like for the different modis land cover classes?
# *THESE NEED TO BE AREA WEIGHTED MEANS AND/OR SE or SD
# boxplots are no good because we would need to reweight by area

igbp_names_full <- c("ENF", "EBF", "DNF", "DBF", "MF", 
                "CSH", "OSH", "WSA", "SAV", "GRA", 
                "WET", "CRO", "URB", "CNV")
# igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
#                  "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
#                  "#2a6495")

weighted.mean(latlongrid_sub_poly2$est_gpp_sens, as.numeric(latlongrid_sub_poly2$area_m))

library(plyr)
library(dplyr) # need to always load dplyr after plyr
library(Hmisc) # for weighted variance (base R already has weighted mean)
#ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = weighted.mean(x$est_gpp_sens, as.numeric(x$area_m))))

# do mid 95% clip to remove extreme values that will otherwise mess with the regression
latlongrid_sub_poly2$est_gpp_sens_mid95 <- latlongrid_sub_poly2$est_gpp_sens
latlongrid_sub_poly2$trendy_gpp_sens_mid95 <- latlongrid_sub_poly2$trendy_gpp_sens
latlongrid_sub_poly2$dif_gpp_sens_mid95 <- latlongrid_sub_poly2$dif_gpp_sens
for(i in 1:length(unique(latlongrid_sub_poly2$modis))){
  pft_inds <- which(latlongrid_sub_poly2$modis == i)
  
  ec_quantiles <- quantile(latlongrid_sub_poly2$est_gpp_sens_mid95[pft_inds], probs = c(0, 0.025, 0.975, 1), na.rm = T)
  extreme_pft_ec <- which(latlongrid_sub_poly2$est_gpp_sens_mid95[pft_inds] < ec_quantiles[2] | 
                                latlongrid_sub_poly2$est_gpp_sens_mid95[pft_inds] > ec_quantiles[3])
  latlongrid_sub_poly2$est_gpp_sens_mid95[pft_inds[extreme_pft_ec]] <- NA
  
  trendy_quantiles <- quantile(latlongrid_sub_poly2$trendy_gpp_sens_mid95[pft_inds], probs = c(0, 0.025, 0.975, 1), na.rm = T)
  extreme_pft_trendy <- which(latlongrid_sub_poly2$trendy_gpp_sens_mid95[pft_inds] < trendy_quantiles[2] | 
                              latlongrid_sub_poly2$trendy_gpp_sens_mid95[pft_inds] > trendy_quantiles[3])
  latlongrid_sub_poly2$trendy_gpp_sens_mid95[pft_inds[extreme_pft_trendy]] <- NA
  
  dif_quantiles <- quantile(latlongrid_sub_poly2$dif_gpp_sens_mid95[pft_inds], probs = c(0, 0.025, 0.975, 1), na.rm = T)
  extreme_pft_dif <- which(latlongrid_sub_poly2$dif_gpp_sens_mid95[pft_inds] < dif_quantiles[2] | 
                                latlongrid_sub_poly2$dif_gpp_sens_mid95[pft_inds] > dif_quantiles[3])
  latlongrid_sub_poly2$dif_gpp_sens_mid95[pft_inds[extreme_pft_trendy]] <- NA
}

# est_gpp_sens
modis_wmean <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = wtd.mean(x$est_gpp_sens_mid95, as.numeric(x$area_m))))
modis_wvar <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wvar = wtd.var(x$est_gpp_sens_mid95, as.numeric(x$area_m))))
modis_weighted <- merge(modis_wmean, modis_wvar)
modis_weighted$wsd <- modis_weighted$wvar^(0.5) # take square root to get SD from variance
modis_weighted$IGBP <- factor(igbp_names_full, levels = igbp_names_full)
modis_weighted$npts <- aggregate(latlongrid_sub_poly2$modis, by = list(latlongrid_sub_poly2$modis), FUN = length)[,2]
modis_weighted$pct_area <- aggregate(as.numeric(latlongrid_sub_poly2$area_m), by = list(latlongrid_sub_poly2$modis), FUN = sum, na.rm=T)[,2]/as.numeric(sum(latlongrid_sub_poly2$area_m))*100 # to get pct
modis_weighted$input <- "EC"
modis_weighted_ec <- modis_weighted

# trendy_gpp_sens
modis_wmean <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = wtd.mean(x$trendy_gpp_sens_mid95, as.numeric(x$area_m))))
modis_wvar <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wvar = wtd.var(x$trendy_gpp_sens_mid95, as.numeric(x$area_m))))
modis_weighted <- merge(modis_wmean, modis_wvar)
modis_weighted$wsd <- modis_weighted$wvar^(0.5) # take square root to get SD from variance
modis_weighted$IGBP <- factor(igbp_names_full, levels = igbp_names_full)
modis_weighted$npts <- aggregate(latlongrid_sub_poly2$modis, by = list(latlongrid_sub_poly2$modis), FUN = length)[,2]
modis_weighted$pct_area <- aggregate(as.numeric(latlongrid_sub_poly2$area_m), by = list(latlongrid_sub_poly2$modis), FUN = sum, na.rm=T)[,2]/as.numeric(sum(latlongrid_sub_poly2$area_m))*100 # to get pct
modis_weighted$input <- "TRENDY mean"
modis_weighted_trendy <- modis_weighted

# dif_gpp_sens
modis_wmean <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wmean = wtd.mean(x$dif_gpp_sens_mid95, as.numeric(x$area_m))))
modis_wvar <- ddply(latlongrid_sub_poly2, .(modis), function(x) data.frame(wvar = wtd.var(x$dif_gpp_sens_mid95, as.numeric(x$area_m))))
modis_weighted <- merge(modis_wmean, modis_wvar)
modis_weighted$wsd <- modis_weighted$wvar^(0.5) # take square root to get SD from variance
modis_weighted$IGBP <- factor(igbp_names_full, levels = igbp_names_full)
modis_weighted$npts <- aggregate(latlongrid_sub_poly2$modis, by = list(latlongrid_sub_poly2$modis), FUN = length)[,2]
modis_weighted$pct_area <- aggregate(as.numeric(latlongrid_sub_poly2$area_m), by = list(latlongrid_sub_poly2$modis), FUN = sum, na.rm=T)[,2]/as.numeric(sum(latlongrid_sub_poly2$area_m))*100 # to get pct
modis_weighted$input <- "Difference"
modis_weighted_dif <- modis_weighted

# combine
modis_weighted <- rbind.data.frame(modis_weighted_ec, modis_weighted_trendy)
  
# p_modis_weighted <- ggplot(modis_weighted, aes(x = IGBP, y = wmean, color = input)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_linerange(aes(ymin = wmean - wsd, ymax = wmean + wsd), position = position_dodge(width = 0.5)) + 
#   coord_cartesian(ylim = c(-0.7, 1.2)) +
#   scale_color_manual(values = c("gray80", "gray50")) +
#   labs(y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
#   theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
#         strip.background = element_blank(), strip.text = element_text(size = 7),
#         axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
#         legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
#         legend.position = c(0.875, 0.9), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
# ggsave("modis_pft_gpp_sens_from_map.eps", p_modis_weighted, width = 89, height = 69, units = "mm")

modis_weighted_sub <- subset(modis_weighted, pct_area > 1 & IGBP != "CRO" & IGBP != "CNV")
modis_weighted_sub <- droplevels(modis_weighted_sub)
p_modis_weighted_alt <- ggplot(modis_weighted_sub, aes(x = wmean, y = IGBP, color = input)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = wmean - wsd, xmax = wmean + wsd), position = position_dodge(width = 0.5)) + 
  coord_cartesian(xlim = c(-0.7, 1.2)) +
  scale_color_manual(values = c("gray80", "gray50")) +
  scale_y_discrete(limits = rev(levels(modis_weighted_sub$IGBP))) +
  labs(x  = bquote("g C" ~m^-2*" / mm")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
        legend.position = c(0.8, 0.97), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ggsave("modis_pft_gpp_sens_from_map_alt_sub.eps", p_modis_weighted_alt, width = 50, height = 80, units = "mm")

# Do a version that is just based on difference
modis_weighted_dif_sub <- subset(modis_weighted_dif, pct_area > 1 & IGBP != "CRO" & IGBP != "CNV")
modis_weighted_dif_sub <- droplevels(modis_weighted_dif_sub)
p_modis_weighted_dif <- ggplot(modis_weighted_dif, aes(x = wmean, y = IGBP)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = wmean - wsd, xmax = wmean + wsd), position = position_dodge(width = 0.5)) + 
  coord_cartesian(xlim = c(-0.7, 1.2)) +
  scale_y_discrete(limits = rev(levels(modis_weighted_dif_sub$IGBP))) +
  labs(x  = bquote("g C" ~m^-2*" / mm")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
        legend.position = c(0.8, 0.97), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ggsave("modis_pft_gpp_sens_from_map_dif.eps", p_modis_weighted_dif, width = 50, height = 80, units = "mm")

# version with all
modis_weighted_all <- rbind.data.frame(modis_weighted_ec, modis_weighted_trendy, modis_weighted_dif)
modis_weighted_all$input <- factor(modis_weighted_all$input, levels = c("Difference", "TRENDY mean", "EC"))
modis_weighted_all_sub <- subset(modis_weighted_all, pct_area > 1 & IGBP != "CRO" & IGBP != "CNV")
modis_weighted_all_sub <- droplevels(modis_weighted_all_sub)
p_modis_weighted_all <- ggplot(modis_weighted_all_sub, aes(x = wmean, y = IGBP, color = input)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 1) +
  geom_linerange(aes(xmin = wmean - wsd, xmax = wmean + wsd), position = position_dodge(width = 0.5), size = 0.5) + 
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  scale_y_discrete(limits = rev(levels(modis_weighted_all_sub$IGBP))) +
  scale_color_manual(values = c("gray30", "gray50", "gray70")) +
  labs(x  = bquote("g C" ~m^-2*" / mm")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
        legend.box.background = element_blank(),
        legend.position = c(0.8, 0.8), 
        legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ggsave("modis_pft_gpp_sens_from_map_all.eps", p_modis_weighted_all, width = 70, height = 75, units = "mm")

# version with all area pct%
modis_weighted_all_sub_ec <- subset(modis_weighted_all_sub, input == "EC")
p_modis_weighted_areas <- ggplot(modis_weighted_all_sub_ec, aes(x = pct_area, y = IGBP)) +
  geom_col(color = "black", fill = "white", size = 0.3, width = 0.75) +
  geom_text(x = modis_weighted_all_sub_ec$pct_area, label = paste(round(modis_weighted_all_sub_ec$pct_area, 0),"%", sep=""), size = 5/.pt) +
  scale_y_discrete(limits = rev(levels(modis_weighted_all_sub$IGBP))) +
  lims(x = c(0, 30)) + 
  labs(x  = "Area %") +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 5, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 5), # legend.title = element_text(size = 6),
        #legend.position = c(0.8, 0.97), 
        legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ggsave("modis_pft_gpp_sens_from_map_all_pct_area.eps", p_modis_weighted_areas, width = 25, height = 75, units = "mm")

#p_modis_weighted_all_areas <- ggarrange(p_modis_weighted_areas, p_modis_weighted_all, widths = c(1, 2), align = "hv")
#ggsave("modis_pft_gpp_sens_from_map_all_pct_area_merge.eps", p_modis_weighted_all_areas, width = 70, height = 75, units = "mm")

##
# Reorder by area

# version with all
p_modis_weighted_all <- ggplot(modis_weighted_all_sub, aes(x = wmean, y = IGBP, color = input)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  geom_linerange(aes(xmin = wmean - wsd, xmax = wmean + wsd), position = position_dodge(width = 0.5), size = 0.5) + 
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  #scale_y_discrete(limits = rev(levels(modis_weighted_all_sub$IGBP))) +
  scale_y_discrete(limits = levels(modis_weighted_all_sub$IGBP)[order(unique(modis_weighted_all_sub$pct_area))]) + # order by area
  scale_color_manual(values = c("gray30", "gray50", "gray70")) +
  labs(x  = bquote("g C" ~m^-2*" / mm")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 6, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 6), # legend.title = element_text(size = 6),
        legend.box.background = element_blank(),
        legend.position = c(0.8, 0.8), 
        legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ggsave("modis_pft_gpp_sens_from_map_all_areaorder.eps", p_modis_weighted_all, width = 70, height = 75, units = "mm")

# version with all area pct%
modis_weighted_all_sub_ec <- subset(modis_weighted_all_sub, input == "EC")
p_modis_weighted_areas <- ggplot(modis_weighted_all_sub_ec, aes(x = pct_area, y = IGBP)) +
  geom_col(color = "black", fill = "white", size = 0.3, width = 0.75) +
  geom_text(x = modis_weighted_all_sub_ec$pct_area, label = paste(round(modis_weighted_all_sub_ec$pct_area, 0),"%", sep=""), size = 6/.pt) +
  #scale_y_discrete(limits = rev(levels(modis_weighted_all_sub$IGBP))) +
  scale_y_discrete(limits = levels(modis_weighted_all_sub_ec$IGBP)[order(modis_weighted_all_sub_ec$pct_area)]) +
  lims(x = c(0, 30)) + 
  labs(x  = "Area %") +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 6, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 6), # legend.title = element_text(size = 6),
        #legend.position = c(0.8, 0.97), 
        legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
ggsave("modis_pft_gpp_sens_from_map_all_pct_area_areaorder.eps", p_modis_weighted_areas, width = 25, height = 75, units = "mm")

#p_modis_weighted_all_areas <- ggarrange(p_modis_weighted_areas, p_modis_weighted_all, widths = c(1, 2), align = "hv")
#ggsave("modis_pft_gpp_sens_from_map_all_pct_area_merge.eps", p_modis_weighted_all_areas, width = 70, height = 75, units = "mm")

p_modis_weighted_all_areas <- ggarrange(p_modis_weighted_areas, p_modis_weighted_all, widths = c(1, 2), align = "hv")
ggsave("modis_pft_gpp_sens_from_map_all_pct_area_merge_areaorder.eps", p_modis_weighted_all_areas, width = 75, height = 80, units = "mm")


x <- modis_weighted_all_sub_ec
order(x$pct_area)
x$IGBP[order(x$pct_area)]
x$IGBP <- factor(x$IGBP, levels = x$IGBP[order(x$pct_area)])
