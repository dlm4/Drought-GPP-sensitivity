library(raster)
library(foreach)
library(doParallel)
library(ncdf4)

detectCores()

# MODIS data for crs
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/gridded_from_yanghui/MOD17A2H_monthly_agg_full_list/")
imgfilename <- list.files()[1]
img <- brick(imgfilename)
modis_crs <- crs(img)
modis_extent <- extent(img)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_v006")
filelist <- list.files()[grep("*.nc", list.files())]

cores <- detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)

foreach(i = 1:length(filelist)) %dopar% {
  library(raster)
  library(ncdf4)
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/FLUXCOM/rs_v006")
  nc <- nc_open(filelist[i])
  gpp <- ncvar_get(nc, varid = "GPP")
  for (layer_ind in 1:12){
    gpp_layer <- gpp[,,layer_ind]
    gpp_raster <- raster(t(gpp_layer), crs = modis_crs) # needed to transpose gpp_layer first due to switching of long and lat
    extent(gpp_raster) <- modis_extent
    gpp_raster_05 <- aggregate(gpp_raster, 6) # 0.083 to 0.5
    
    layer_ind_char <- as.character(layer_ind)
    if(nchar(layer_ind_char) < 2){ layer_ind_char <- paste("0", layer_ind_char, sep="")}
    
    setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/fluxcom_rs_v006_05deg")
    output_imgfilename <- paste(substring(filelist[i], 1, 60), layer_ind_char, "_05deg.tif", sep = "")
    writeRaster(gpp_raster_05, output_imgfilename, overwrite = T) 
  }
}

#stop cluster
stopCluster(cl)
