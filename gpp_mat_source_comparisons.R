library(tidyverse)
library(ncdf4)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/")
ncin <- nc_open("LPJ-GUESS_S2_gpp_fEst.nc") # netcdf file

gppnc <- ncvar_get(ncin,"gpp.monthly")
# get lat and lon - need these for reference coordinates
lat <- ncvar_get(ncin,"lat")
lon <- ncvar_get(ncin,"lon")

yrs <- rep(1901:2016, each = 12) # get list of all years, 12 elements each
mos <- rep(1:12, 116) # repeat 1:12 116 times, for 1901 through 2016
band_inds <- which(yrs %in% 1991:2016) # will be same for gpp and precip

model_name <- "LPJ-GUESS"
setwd("../TRENDY_S2/GPP")
gppfile <- list.files(pattern = glob2rx(paste(model_name, "*S2*gpp*.mat", sep = "")))[1] # use 1 to distinguish LPJ from LPJ-GUESS and ORCHIDEE from OCHIDEE-MICT

gpp <- h5read(gppfile, name = "gpp_05")
gpp <- aperm(gpp, c(2,1,3)) # swap rows and columns
gpp <- gpp[, rev(seq_len(ncol(gpp))),] # flip columns
gpp_array <- gpp[,, band_inds]
rm(gpp)

# image(gppnc[,,1880])
# image(gppnc[which(lon > -15 & lon < 5), which(lat > 25 & lat < 45), 1880])
# image(gpp_array[which(lon > -15 & lon < 5), which(lat > 25 & lat < 45), 308])
# 
# image(gppnc[which(lon > -5 & lon < 0), which(lat > 25 & lat < 30), 1879])
# image(gpp_array[which(lon > -5 & lon < 0), which(lat > 25 & lat < 30), 307])
# 
# gppnc[which(lon > -5 & lon < 0), which(lat > 25 & lat < 30), 1879]
# gpp_array[which(lon > -5 & lon < 0), which(lat > 25 & lat < 30), 307]


na_inds <- which(is.na(gpp_array))
gppnc_sub <- gppnc[,,1573:1884]
gppnc_sub_na <- gppnc_sub[na_inds]

length(gppnc_sub_na)
gppnc_sub2 <- gppnc_sub_na[which(!is.na(gppnc_sub_na))]

hist(gppnc_sub2)
max(gppnc_sub2)
# all missing LPJ-GUESS missing values from 1991-2016 are 0 or negative

#####

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/")
ncin <- nc_open("CLM4.5_S2_gpp.nc") # netcdf file
gpp_clm <- ncvar_get(ncin,"gpp")
nc_close(ncin)
image(gpp_clm[,,1880])


model_name <- "CLM"
setwd("../TRENDY_S2/GPP")
gppfile <- list.files(pattern = glob2rx(paste(model_name, "*S2*gpp*.mat", sep = "")))[1] # use 1 to distinguish LPJ from LPJ-GUESS and ORCHIDEE from OCHIDEE-MICT

gpp <- h5read(gppfile, name = "gpp_05")
gpp <- aperm(gpp, c(2,1,3)) # swap rows and columns
gpp <- gpp[, rev(seq_len(ncol(gpp))),] # flip columns
gpp_array <- gpp[,, band_inds]
rm(gpp)

image(gpp_array[,,308]) # this is smoothed, also...
# regridded CLM has a vertical line of missing values at prime meridian, some sort of re-grid issues

# original CLM is centered at 180 E, so need to split and rejoin
image(gpp_clm[1:144,,308])
gpp_clm2 <- gpp_clm
gpp_clm2[1:144,,] <- gpp_clm[145:288,,]
gpp_clm2[145:288,,] <- gpp_clm[1:144,,]
image(gpp_clm2[,,308])
View(gpp_clm2[,,308])

gpp_clm2[gpp_clm2 <= 0] <- NA
image(gpp_clm2[,,308])

# Regridding for CLM knocks out edges and weird skinny lines on coarse gridding, may have applied a mask too


#####
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/trendy/test_data/")
ncin <- nc_open("VEGAS_S2_gpp.nc") # netcdf file
gpp_vegas <- ncvar_get(ncin,"gpp")
nc_close(ncin)

image(gpp_vegas[,,1880])
# upside down and split
gpp_vegas <- gpp_vegas[,rev(1:ncol(gpp_vegas)),]
gpp_vegas2 <- gpp_vegas
gpp_vegas2[1:360,,] <- gpp_vegas[361:720,,]
gpp_vegas2[361:720,,] <- gpp_vegas[1:360,,]
image(gpp_vegas2[,,1880])


model_name <- "VEGAS"
setwd("../TRENDY_S2/GPP")
gppfile <- list.files(pattern = glob2rx(paste(model_name, "*S2*gpp*.mat", sep = "")))[1] # use 1 to distinguish LPJ from LPJ-GUESS and ORCHIDEE from OCHIDEE-MICT

gpp <- h5read(gppfile, name = "gpp_05")
gpp <- aperm(gpp, c(2,1,3)) # swap rows and columns
gpp <- gpp[, rev(seq_len(ncol(gpp))),] # flip columns
gpp_array <- gpp[,, band_inds]
rm(gpp)

image(gpp_vegas2[,,1880])
image(gpp_array[,,308])
# these look identical, which is good

gpp_vegas2_sub <- gpp_vegas2[,,1573:1884]
unique(gpp_vegas2_sub == gpp_array)
image(gpp_vegas2_sub[which(lon > -10 & lon < 10), which(lat > 30 & lat < 50), 308])
image(gpp_array[which(lon > -10 & lon < 10), which(lat > 30 & lat < 50), 308])

gpp_vegas2_sub[which(lon > -10 & lon < 10), which(lat > 30 & lat < 50), 308] * 60*60*24*30 # check the rescaling, kg C m-2 s-1 --> kg C m-2 30d-1
gpp_array[which(lon > -10 & lon < 10), which(lat > 30 & lat < 50), 308]
# original has NA, new as NaN, but looks like they're the same values?

x1 <- gpp_vegas2_sub[which(!is.na(gpp_vegas2_sub))]* 60*60*24*30
x2 <- gpp_array[which(!is.na(gpp_array))]

unique(x1 == x2) # not exactly the same, they're different lengths, so some more NAs removed in x2

x3 <- gpp_vegas2_sub[which(!is.na(gpp_vegas2_sub) | gpp_vegas2_sub > 0)]* 60*60*24*30 # nope this isn't the same length either...

gpp_vegas2_sub_scaled <- gpp_vegas2_sub * 60*60*24*30
gpp_vegas2_sub_scaled[which(!is.na(gpp_vegas2_sub_scaled))]

w1 <- which(!is.na(gpp_vegas2_sub))
w2 <- which(!is.na(gpp_array))
w3 <- setdiff(w1, w2)

gpp_array[w3] # these are all NaNs
gpp_vegas2_sub[w3] # these are all very small negative numbers

w4 <- which(gpp_vegas2_sub > 0)
unique(w4 == w2) # this is all true
# So the regridding is excluding the small negatives and zeros, generally
