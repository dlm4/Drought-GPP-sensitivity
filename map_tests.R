states <- map_data("state")
usamap <- ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
usamap + coord_map("gilbert")


world1 <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world1) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world1) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

world2 <- map_data("world")
worldmap <- ggplot(world2, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)
worldmap

box <- c(xmin = -180, ymin = 30, xmax = 180, ymax = 85)

#coastline_crop <- st_crop(coastlinemap, box)

#ggplot(data = coastline_crop) +
#  geom_sf() #+
  #coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")


ggplot(data = world1) +
  geom_sf() +
  coord_sf(ylim = c(30, 85), crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_map("azequalarea")

ggplot(data = coastlinemap) + 
  geom_sf() +
  coord_sf(crs = st_crs(3035))

crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"

latlongrid_sub_sf <- st_as_sf(latlongrid_sub, coords = c(1,2), crs = crs(coastlinemap))
#latlongrid_sub_sf_grid <- st_make_grid(latlongrid_sub_sf, what = "polygons") %>% st_as_sf()

latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf()
# 
# xmin <- latlongrid_sub$Lon - 0.25
# xmax <- latlongrid_sub$Lon + 0.25
# ymin <- latlongrid_sub$Lat - 0.25
# ymax <- latlongrid_sub$Lat + 0.25
# 
# i <- 1
# for (i in 1:nrow(latlongrid_sub)){
#   
# b <- list(st_point(c(xmin[i], ymin[i])), 
#      st_point(c(xmin[i], ymax[i])), 
#      st_point(c(xmax[i], ymax[i])), 
#      st_point(c(xmax[i], ymin[i])))
# 
# b <- list(matrix(c(xmin[i], ymin[i]), ncol = 2), 
#           matrix(c(xmin[i], ymax[i]), ncol = 2), 
#           matrix(c(xmax[i], ymax[i]), ncol = 2), 
#           matrix(c(xmax[i], ymin[i]), ncol = 2))
# 
# b <- list(matrix(c(xmin[i], xmin[i], xmax[i], xmax[i], xmin[i], ymin[i], ymax[i], ymax[i], ymin[i], ymin[i]), ncol = 2))
# }
# 
# b_poly <- st_polygon(b)
# 
# latlongrid_sub_sf$geometry <- b
# 
# # this works but is incredibly slooooow
# for(i in 1:nrow(latlongrid_sub_sf)){
#   print(i)
#   box_bounds <- rbind.data.frame(c(ymin[i], xmin[i]), c(ymax[i], xmax[i]))
#   box_poly <- st_as_sf(box_bounds, coords = c(2, 1), crs = crs(coastlinemap)) %>% st_bbox() %>% st_as_sfc() %>% st_as_sf()
#   latlongrid_sub_sf$geometry[i] <- box_poly
# }

#### THIS WORKS!
latlongrid_sub_sf_grid2 <- st_make_grid(latlongrid_sub_sf, cellsize = c(0.5, 0.5), offset = c(-180, 30), what = "polygons") %>% st_as_sf()
grid_intersect <- st_intersects(latlongrid_sub_sf_grid2, latlongrid_sub_sf)
grid_new_inds <- lengths(grid_intersect) > 0
grid_new <- latlongrid_sub_sf_grid2[grid_new_inds,]
latlongrid_sub_poly <- st_set_geometry(latlongrid_sub, st_as_sfc(grid_new))

#test1 <- latlongrid_sub_sf[1:10,]
ggplot(data = latlongrid_sub_sf) + 
  #geom_sf(aes(color = dif_gpp_sens_cut)) +
  geom_sf(data = coastlinemap) +
  geom_sf(data = grid_new, fill = "blue", color = NA, alpha = 0.5) + 
  #coord_sf(ylim = c(30, 80), expand = FALSE) + 
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()

# NEW VERSION OF PLOT
ggplot(data = latlongrid_sub_poly) + 
  geom_sf(aes(fill = dif_gpp_sens_cut, color = dif_gpp_sens_cut), size = 0.1) +
  geom_sf(data = coastlinemap, color = "gray20", size = 0.2) +
  #geom_sf(data = grid_new, fill = "blue", color = NA, alpha = 0.5) + 
  #coord_sf(ylim = c(30, 80), expand = FALSE) + 
  scale_fill_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
  scale_color_manual(name = bquote("g C "~m^-2*" / mm"), values = rev(brewer.pal(11, "RdBu"))) +
  coord_sf(crs = crs_robin, xlim = c(-16560200, 16518400), ylim = c(2969593, 8491593), expand = FALSE, label_graticule = "SW") +
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


# Needed to convert the latlongrid_sub into an sf object with gridded polygons for grid cells
# Needed to get the crs_robin crs to have lat and lon for referencing



outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
pl1 = st_polygon(pts)
pts3 = lapply(pts, function(x) cbind(x, 0))



ggplot(data = coastlinemap) +
  geom_sf() +
  coord_sf(ylim = c(-80, 80), expand = FALSE, crs = st_crs(4326)) +
  theme_bw()


world3 <- map_data("world")
w3map <- ggplot(world3, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black")
w3map + coord_map("azequalarea")



ggplot(latlongrid_sub) +
  geom_polygon(data = world3, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray90") +
  geom_tile(aes(x = Lon, y = Lat, fill = trendy_gpp_sens_cut)) +
  #scale_fill_brewer("g C / mm",type = "seq", palette = "RdBu") +
  fillScale + 
  #scale_fill_manual(values = rev(brewer.pal(11, "RdBu"))) +
  guides(fill = guide_legend(reverse = TRUE)) + coord_map(projection = "mercator", xlim = c(-180, 180), ylim = c(30, 80)) + 
  theme_bw()


coastlinemap_robin <- st_transform(coastlinemap, crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
