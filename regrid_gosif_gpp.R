library(raster)
library(foreach)
library(doParallel)
`%notin%` <- Negate(`%in%`) # %notin% function

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/GOSIF-GPP_v2_Monthly/Mean")
filelist <- list.files()[grep("*.tif", list.files())]
filelist <- filelist[1:length(filelist) %notin% grep("gz", filelist)]

cores <- detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)


getndays <- function(imgfilename){
  month <- unlist(strsplit(imgfilename, "[.]|[_]|M"))[5] %>% as.integer()
  year <- unlist(strsplit(imgfilename, "[.]|[_]|M"))[3] %>% as.integer()
  if (month %in% c(1, 3, 5, 7, 8, 10, 12)){
    nday <- 31
  } else if (month == 2){
    if (year %% 4 == 0){
      nday <- 29
    } else {
      nday <- 28
    }
  } else {
    nday <- 30
  }
}

#foreach(i = 1:length(filelist)) %dopar% { # something not working with this for some reason
for(i in 1:length(filelist)){
  library(raster)
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/source/GOSIF-GPP_v2_Monthly/Mean")
  imgfilename <- filelist[i]
  img <- brick(imgfilename)
  img[img > 60000] <- NA # note that this GPP is g C m-2 mo-1 with 0.01 scale factor
  img_05 <- aggregate(img, 10)
  nday <- getndays(imgfilename)
  img_05_dailyavg <- img_05 * 0.01 / nday
  setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/regridded/gosif_gpp_05deg")
  output_imgfilename <- paste(substring(filelist[i], 1, 23), "_05deg.tif", sep = "")
  writeRaster(img_05_dailyavg, output_imgfilename, overwrite = T) 
}

#stop cluster
stopCluster(cl)
