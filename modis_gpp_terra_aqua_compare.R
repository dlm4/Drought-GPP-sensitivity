# compare mod17 and myd17 gpp seasonal sums
library(tidyverse)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/gpp_sif_gridded/modis_gpp/")
gpp_terra <- read.csv("ec_sites_seasons_modis_gpp_MOD17A2HGF_006_Gpp_500m.csv")
gpp_aqua <- read.csv("ec_sites_seasons_modis_gpp_MYD17A2HGF_006_Gpp_500m.csv")

colnames(gpp_terra)[5] <- "GPP_terra"
colnames(gpp_aqua)[5] <- "GPP_aqua"

gpp_merge <- merge(gpp_terra, gpp_aqua)

gpp_merge2 <- subset(gpp_merge, GPP_aqua < 32000 & GPP_terra < 32000)
summary(lm(gpp_merge2$GPP_aqua ~ gpp_merge2$GPP_terra))

ggplot(gpp_merge2, aes(x = GPP_terra, y = GPP_aqua)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(x = bquote("GPP Terra MOD17 (g C " ~m^-2* " " ~season^-1*")"),
       y = bquote("GPP Aqua MYD17 (g C " ~m^-2* " " ~season^-1*")")) +
  coord_equal() +
  annotate(geom="text", x=800, y=100, label = bquote("y = 0.97*x + 3.84," ~R^2*" = 0.97"), color="blue") +
  theme_bw()

ggsave("modis_gpp_terra_aqua_compare.png", height = 90, width = 189, units = "mm")
  
#####
# It's just IT-Ro1 that's not available in MODIS for some reason...
# IT-Ro1 is only in the 5+ set, so not a big deal for our purposes for now
x <- subset(gpp_merge, GPP_aqua > 32000 | GPP_terra > 32000)
