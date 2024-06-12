# Aug 7, 2021
# 1. Resample to GRACE extent, resolution (1˚x1˚)
# 2. Average to GRACE time step (monthly)

# Data downloaded from:
# https://osf.io/8xqy6/ 

# Set up -----
library(pacman)
p_load(ncdf4, stringr, raster, tictoc, dplyr, rts)

# Create parallel processing unit -----
# Locate cores
p_load(foreach, doParallel)
parallel::detectCores()
n.cores <- parallel::detectCores() - 1

# Create cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

# Register cluster
doParallel::registerDoParallel(cl = my.cluster)


# Get monthly average and resample -----

wds <- '/Volumes/Dan_Raid_Disk/Data/CSIF/CSIF_v2'
setwd(wds)
years <- as.integer(list.files())


for (i in 21) { # For each year
  setwd(paste0(wds, '/', years[i]))
  files <- list.files()
  
  dates <- data.frame(date = as.Date(substr(files, 21, 27), format = '%Y%j'))  %>% 
    mutate(month = format(date, '%m'))
  
  month.mean <- foreach (j = 1:length(unique(dates$month)), # each month
                         .combine = 'c') %dopar% { 
                           
                           vec <- which(dates$month == unique(dates$month)[j])
                           
                           data <- list()
                           
                           for (k in vec) { # create brick of each day
                             
                             setwd(paste0(wds, '/', years[i]))
                             
                             nc_data <- nc_open(files[k])
                             dat <- ncvar_get(nc_data, 'clear_daily_SIF')
                             
                             # Create raster
                             dat <- raster(dat)
                             dat <- flip(t(dat),2)
                             
                             # Resample
                             extent(dat)<- c(-180, 180, -90, 90)
                             crs(dat) <- ' +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 '
                             dat <- aggregate(dat, fact=5) # from 0.05 to 0.25 resolution (matching ESA CCI data)
                             
                             index <- k - vec[1] + 1
                             data[[index]] <- dat
                             rm(dat)
                             nc_close(nc_data)
                           }
                           
                           data <- brick(data)
                           data <- mean(data, na.rm=T)
                           names(data) <- paste(years[i], unique(dates$month)[j], sep = '-')
                           
                           return(data)
                           rm(vec, dat, nc_data, index)
                         }
  
  month.mean <- brick(month.mean)
  
  if (i == 1){
    result.mean <- month.mean
  } else {
    result.mean <- stack(result.mean, month.mean)
  }
  
  print(years[i])
  toc()
}

result.mean <- brick(result.mean)
setwd('/Volumes/Dan_Raid_Disk/Sophie/SIF_TWS/data/remote_sesing/25x25res/monthly_means')
writeRaster(result.mean, 'CSIF_v2.grd', overwrite = T)
