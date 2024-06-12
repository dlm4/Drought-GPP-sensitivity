library(ncdf4)
library(tidyverse)
library(R.matlab)
library(mapproj)

# aggregate terraclimate from 1/24 degree

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/summaries/")
tc_var <- nc_open("TerraClimate19812010_ppt.nc")
ppt <- ncvar_get(tc_var, "ppt")

tc_var <- nc_open("TerraClimate19812010_pet.nc")
pet <- ncvar_get(tc_var, "pet")
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

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4")
# static consistent color scale
myColors <- rev(brewer.pal(11, "RdBu"))
names(myColors) <- c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                     "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3")
#fillScale <- scale_fill_manual(name = "g C / mm", values = myColors) 
fillScale <- scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = myColors) 

#output_path <- "C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/gpp_precip_slope_v4_maps_30N/" # need last /
file_list <- list.files()

# Do average of all models map for each season
#season_vec <- c("spring", "summer", "fall")
# only doing spring, no need for loop over season
season <- "spring"
#for(season in season_vec){
  #season <- "spring"
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
  
  # Get mean across models
  Mean_Precip_Slope <- apply(model_sens_precip_merge[, 3:ncol(model_sens_precip_merge)], MARGIN = 1, FUN = mean, na.rm = TRUE)
  model_sens_precip_mean <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope)
  model_sens_precip_mean <- na.omit(model_sens_precip_mean) # remove NAs, doesn't work?
  
  # Set ranges and labels
  model_sens_precip_mean$precip_cut <- cut(model_sens_precip_mean$Mean_Precip_Slope,
                                           breaks = c(-1e32, -3, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 3, 1e32),
                                           labels = c("<-3", "(-3,-2]", "(-2,-1]", "(-1,-0.5]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                                      "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                           include.lowest = TRUE)
  
  plot_title <- paste("GPP~Precip Slope, Model Mean - ", season, sep = "")
  
  # coastline map from rnaturalearthdata, simpler than country boundaries
  coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')
  
  ggplot(model_sens_precip_mean) +
    geom_tile(aes(x = Lon, y = Lat, fill = precip_cut)) +
    #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
    fillScale + 
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
    coord_map() +
    #coord_sf(ylim = c(30, 85), expand = FALSE) +
    #coord_equal() +
    scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
    labs(title = plot_title) + theme_bw()
  
  #ggsave(paste(output_path, "modelmean_", season, "_gpp_precip_slope_map_30N.png", sep = ""),
         #width = 11, height = 3, units = "in")
#}
  
latlongrid <- model_sens_precip[,1:2]
latlongrid <- subset(latlongrid, Lat > 30)

aridity_index_30N <- aridity_index[, 1:120] # just get nhem 30 N
aridity_index_30N <- aridity_index_30N[,ncol(aridity_index_30N):1] # flip over colomns
latlongrid$ai <- c(aridity_index_30N) # append
latlongrid$ai_cut <- cut(latlongrid$ai, breaks = c(0, 0.3, 0.5, 0.65, 1, 2, 3, 1e32))

ggplot(latlongrid) +
  geom_tile(aes(x = Lon, y = Lat, fill = ai_cut)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()

latlongrid$est_gpp_sens <- -0.0839 - 1.26*log10(latlongrid$ai) # estimated gpp sensitivity from flux tower regression

model_sens_precip_mean_all <- cbind(model_sens_precip[,1:2], Mean_Precip_Slope)
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

ggplot(latlongrid) +
  geom_tile(aes(x = Lon, y = Lat, fill = trendy_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  fillScale + 
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  labs(title = plot_title) + theme_bw()

ggplot(latlongrid) +
  geom_tile(aes(x = Lon, y = Lat, fill = est_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  fillScale + 
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  labs(title = plot_title) + theme_bw()


latlongrid$dif_gpp_sens <- latlongrid$trendy_gpp_sens - latlongrid$est_gpp_sens

latlongrid$dif_gpp_sens_cut <- cut(latlongrid$dif_gpp_sens,
                                   #breaks = c(-1e32, -0.8, -0.6, -0.4, -0.2, -0.1, 0.1, 0.2, 0.4, 0.6, 0.8, 1e32),
                                   breaks = c(-1e32, -0.9, -0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1e32),
                                   #labels = c("<-0.8", "(-0.8,-0.6]", "(-0.6,-0.4]", "(-0.1,-0.1]", "(-0.5,-0.1]", "(-0.1,0.1]", 
                                  #            "(0.1,0.5]", "(0.5,1]", "(1,2]", "(2,3]", ">3"),
                                   include.lowest = TRUE)

# ggplot(latlongrid) +
#   geom_tile(aes(x = Lon, y = Lat, fill = dif_gpp_sens_cut)) +
#   #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
#   fillScale +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
#   coord_sf(ylim = c(30, 85), expand = FALSE) +
#   scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
#   labs(title = plot_title) + theme_bw()

# Need to include MODIS land cover as a vegetated land filter
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/")
modis_pft <- R.matlab::readMat("MODIS_PFTS_Gridded.mat")
# 360 rows, 720 columns, need to swap?
#modis_pft$landCoverPadded
modis_pft <- aperm(modis_pft$landCoverPadded, c(2,1)) # swap rows and columns
modis_pft <- modis_pft[, rev(seq_len(ncol(modis_pft)))] # flip columns

modis_pft_sub <- modis_pft[, 241:360]

latlongrid$modis <- c(modis_pft_sub)

ggplot(latlongrid) +
  geom_tile(aes(x = Lon, y = Lat, fill = modis)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()

latlongrid_sub <- subset(latlongrid, modis %in% 1:14) # keep everything that isn't water or barren

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/maps")

# the maps are vertically stretched for some reason, pixels are not equal squares?

ggplot(latlongrid_sub) +
  geom_tile(aes(x = Lon, y = Lat, fill = trendy_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  fillScale + 
  #scale_fill_manual(values = rev(brewer.pal(11, "RdBu"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", fill = "gray90", size = 0.25) + 
  coord_sf(xlim = c(-180, 180), ylim = c(30, 80), expand = FALSE, lims_method = "orthogonal") +
  #coord_equal() +
  #scale_x_continuous(name = NULL, breaks = c(-120, -60, 0, 60, 120)) + 
  scale_y_continuous(name = NULL) +
  theme_bw()
#ggsave("trendy_gpp_sens.png", width = 10, height = 4, units = "in")

ggplot(latlongrid_sub) +
  geom_tile(aes(x = Lon, y = Lat, fill = est_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  fillScale + 
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  labs(title = plot_title) + theme_bw()
#ggsave("tc_gpp_sens.png", width = 10, height = 4, units = "in")

# need to revise the color scale here...
ggplot(latlongrid_sub) +
  geom_tile(aes(x = Lon, y = Lat, fill = dif_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  #fillScale + 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  labs(title = plot_title) + theme_bw()
ggsave("trendy_minus_tc_gpp_sens.png", width = 10, height = 4, units = "in")

# maybe only show areas with AI >= 0.65 because the real difference isn't significant on the dry side??

latlongrid_sub2 <- subset(latlongrid, modis %in% 1:14 & ai >= 0.65) # keep everything that isn't water or barren
ggplot(latlongrid_sub2) +
  geom_tile(aes(x = Lon, y = Lat, fill = dif_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  #fillScale + 
  scale_fill_manual(values = rev(brewer.pal(11, "RdBu"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  coord_sf(ylim = c(30, 85), expand = FALSE) +
  scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) +
  labs(title = plot_title) + theme_bw()
ggsave("trendy_minus_tc_gpp_sens_wetonly.png", width = 10, height = 4, units = "in")
# try as a pct dif? would case low sensitivity areas to potentially blow up...


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
crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"

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
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/maps")
ggsave("trendy_minus_tc_gpp_sens_robin.png", width = 16, height = 5, units = "in")
