#flx_combinedsource_worldclim_rfranger.R

# predict site's GPP sensitivity to spring precip using extracted worldclim variables

# use set of 61 sites for 10 year set

library(tidyverse)
library(raster)
library(ranger)

`%notin%` <- Negate(`%in%`)

# bioclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/")
flx_wc2_vars <- read.csv("fluxsites_combinedsource_wc2_bio_all_vars.csv")

# Read in list of sites and sensitivities
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

# Precip trends, spring only, at least 10 years of observations
flx_site_sens_sub <- flx_site_sens[which(flx_site_sens$Met_Var == "P_F_sum" & flx_site_sens$Season == "Spring" & flx_site_sens$Npts >= 10),]

# merge together
flx_merged <- merge(flx_site_sens_sub, flx_wc2_vars, by = "SITE_ID")

 # Test rf ranger with OOB Rsq error?
# this is just the worldclim variables
select_columns <- c("Slope", "wc2_bio_1",  "wc2_bio_2",  "wc2_bio_3",  "wc2_bio_4",
                    "wc2_bio_5",  "wc2_bio_6",  "wc2_bio_7",
                    "wc2_bio_8", "wc2_bio_9",
                    "wc2_bio_10",  "wc2_bio_11",  "wc2_bio_12",
                    "wc2_bio_13",  "wc2_bio_14",  "wc2_bio_15",  "wc2_bio_16",
                    "wc2_bio_17",  "wc2_bio_18",  "wc2_bio_19")

#select_columns <- c("Slope", colnames(flx_merged)[12:115]) # this is everything, but doesn't appear to do a much better job, only very marginal improvement in median R2 of OOB, 0.003
flx_input <- subset(flx_merged, select = select_columns)

# run ranger
seed_rsq <- rep(NA, 100)
for (i in 1:100){
  set.seed(i) 
  rfout <- ranger(Slope ~ ., data = flx_input, importance = "permutation", num.trees = 500)
  seed_rsq[i] <- rfout$r.squared
}
summary(seed_rsq)
plot(seed_rsq)

# going with seed = 100 just for repeatability
set.seed(100)

plot(flx_input$Slope, rfout$predictions)
summary(lm(rfout$predictions ~ flx_input$Slope)) 

# relationships really aren't that good... hard to predict!
# R2  = 0.34... and this is on the training data, which is as good as we can expect to do

#####
# predict on stacked data
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/wc2.1_30s_bio/")

# img <- brick("wc2.1_30s_bio_stacked.tif")
# names(img) <- c("wc2_bio_1",  "wc2_bio_2",  "wc2_bio_3",  "wc2_bio_4",
#                 "wc2_bio_5",  "wc2_bio_6",  "wc2_bio_7",
#                 "wc2_bio_8", "wc2_bio_9",
#                 "wc2_bio_10",  "wc2_bio_11",  "wc2_bio_12",
#                 "wc2_bio_13",  "wc2_bio_14",  "wc2_bio_15",  "wc2_bio_16",
#                 "wc2_bio_17",  "wc2_bio_18",  "wc2_bio_19")
# # crop to only be north of 30 N
# n30_extent <- extent(-180,180,30,90)
# img <- crop(img, n30_extent)
# 
# # will need to mask out areas that are nonvegetated, totally veg free deserts and ice sheets
# 
# # write out because this takes a while
# raster::writeRaster(img, "wc2.1_30s_bio_stacked_n30.tif", format = "GTiff", progress = "text")

img2 <- brick("wc2.1_30s_bio_stacked_n30.tif")
names(img2) <- c("wc2_bio_1",  "wc2_bio_2",  "wc2_bio_3",  "wc2_bio_4",
                "wc2_bio_5",  "wc2_bio_6",  "wc2_bio_7",
                "wc2_bio_8", "wc2_bio_9",
                "wc2_bio_10",  "wc2_bio_11",  "wc2_bio_12",
                "wc2_bio_13",  "wc2_bio_14",  "wc2_bio_15",  "wc2_bio_16",
                "wc2_bio_17",  "wc2_bio_18",  "wc2_bio_19")
img_predict <- predict(img, rfout, 
                       type = "response", progress = "text", 
                       fun = function(model, ...) predict(model, ...)$predictions)

save(rfout, file = "rfranger_for_wc2.1_seed100.Rdata") # save the (random) forest
raster::writeRaster(img_predict, "rfpredictions_wc2.1_30s_bio_stacked_n30.tif", format = "GTiff", progress = "text")

#test.colors <- colorRampPalette(c("blue3", "purple3", "gray90", "orange3", "red3"), space = "rgb")
#plot(img_predict, col = test.colors(255), zlim = c(-1.5, 1.5))

rfvarimpdf <- data.frame(rfout$variable.importance)
rfvarimpdf$vars <- rownames(rfvarimpdf)
colnames(rfvarimpdf)[1] <- "importance"
ggplot(data = rfvarimpdf) +
  geom_col(aes(x = vars, y = importance))
