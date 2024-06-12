# flx_combinedsource_terraclimate_rfranger.R

library(tidyverse)
library(raster)
library(ranger)

`%notin%` <- Negate(`%in%`)

# terraclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars <- read.csv("flx_site_combinedsource_terraclimate_data.csv")

# Read in list of sites and sensitivities
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

# Precip trends, spring only, at least 10 years of observations
flx_site_sens_sub <- flx_site_sens[which(flx_site_sens$Met_Var == "P_F_sum" & flx_site_sens$Season == "Spring" & flx_site_sens$Npts >= 10),]

# merge together
flx_merged <- merge(flx_site_sens_sub, flx_tc_vars, by = "SITE_ID")

# subset to just get slope and terraclimate variables
flx_input <- cbind.data.frame(flx_merged$Slope, flx_merged[, which(colnames(flx_merged) %in% colnames(flx_tc_vars[8:ncol(flx_tc_vars)]))])
colnames(flx_input)[1] <- "Slope"

# run ranger tests
seed_rsq <- rep(NA, 100)
for (i in 1:100){
  print(i)
  set.seed(i) 
  rfout <- ranger(Slope ~ ., data = flx_input, importance = "permutation", num.trees = 500)
  seed_rsq[i] <- rfout$r.squared
}
summary(seed_rsq)
plot(seed_rsq)

# describes just about the same amount of error compared to just CRU aridity even with all variables
# mean = 0.403 Rsq

rfvarimpdf <- data.frame(rfout$variable.importance)
rfvarimpdf$vars <- rownames(rfvarimpdf)
colnames(rfvarimpdf)[1] <- "importance"
ggplot(data = rfvarimpdf) +
  geom_col(aes(x = importance, y = vars))
