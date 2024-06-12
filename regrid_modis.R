library(raster)
library(foreach)
library(doParallel)

detectCores()

# get spring GPP ~ precip sensitivity maps for the remote sensing data products

# Regrid products to 0.5 degree (if necessary)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/gridded_from_yanghui/MOD17A2H_monthly_agg_full_list/")
filelist <- list.files()

cores <- detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)

foreach(i = 1:length(filelist)) %dopar% {
  library(raster)
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/gridded_from_yanghui/MOD17A2H_monthly_agg_full_list/")
  imgfilename <- filelist[i]
  img <- brick(imgfilename)
  img[img == -9999] <- NA
  img_05 <- aggregate(img, 10)
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/gridded_from_yanghui/MOD17A2H_monthly_agg_full_list_05")
  output_imgfilename <- paste(unlist(strsplit(imgfilename, "[.]"))[1], "_05deg.tif", sep = "")
  writeRaster(img_05, output_imgfilename, overwrite = T) 
}

#stop cluster
stopCluster(cl)
