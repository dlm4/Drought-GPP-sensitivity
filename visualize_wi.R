# Use this to calculate the aridity from terraclimate at a half degree grid
# then generate plots to map the difference for EC estimated precip_slope from the TRENDY model mean for each grid cell

library(ncdf4)
library(tidyverse)
library(R.matlab)
library(mapproj)
library(ggpubr)
library(sf)
library(raster)

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

aridity_index_native <- ppt_sum / pet_sum
aridity_index_native_transpose <- t(aridity_index_native)
aridity_index_native_transpose[aridity_index_native_transpose > 3] <- 3
aridity_raster <- raster(aridity_index_native_transpose)
#crs(aridity_raster) <- "+proj=latlon +lat_0=0 +lon_0=0 +x0=0 +y0=0 +datum=WGS84"
plot(aridity_raster, zlim = c(0,3), col = rev(topo.colors(256)))
