# How to evaluate impact of spring precip deficits (met drought) on annual GPP (Josh's question)

# Get the spring precip for each site from each flux site,
# use the data for sites that are subset based on availability from the original dataset
# Get the yearly fluxnet format data for each site for the years available
# detrend with Mann-Kendall test and Sen's slope yearly data
# Do years low spring precip correspond with higher annual GPP (detrended)?
#   Correlation plot?
# plot this along per-site aridity gradient

#####

library(tidyverse)
library(ggpmisc)
library(trend)
library(zyp)
`%notin%` <- Negate(`%in%`) # %notin% function

# all site data
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_seasons <- read.csv("combined_source_spring_info_swc1.csv") # this is the full list of spring sites

# list of sites in the 10+ list
flx_slopes <- read.csv("site_info_lists/site_list_spring10.csv")

# subset
flx_seasons <- subset(flx_seasons, subset = SITE_ID %in% flx_slopes$SITE_ID)

# Get the yearly flx data

for (i in 1:nrow(flx_slopes)){
  #i <- which(flx_slopes$SITE_ID == "US-NR1")
  
  path_prefix <- "/Users/davidmiller/dlm_files/keenan_postdoc/"
  source_dataset <- flx_slopes$SOURCE[i]
  source_path <- NA # so it throws error and stops if it doesn't work
  if (source_dataset == "FLUXNET2015") {
    source_path <- "fluxnet2015/FLUXNET2015-latest/"
  } else if (source_dataset == "ICOS-2020"){
    source_path <- "icos_2020_warm_winter/icos_2020_data/"
  } else if (source_dataset == "OneFlux") {
    source_path <- "ameriflux/oneflux_beta/allsites_FULLSET/"
  } else if (source_dataset == "ICOS-2018"){
    source_path <- "icos_2018_drought_archive/icos_2018_data/"
  }
  
  setwd(paste(path_prefix, source_path, sep = "")) # set path to where the data is pointing
  flx_name <- flx_slopes$SITE_ID[i]
  site_yearly <- read.csv(list.files(pattern = glob2rx(paste("*", flx_name, "*FULLSET_YY*.csv", sep = "")), recursive = T)[1]) # get yearly data
  
  # subset seasonal data for particular site
  flx_seasons_site <- subset(flx_seasons, subset = SITE_ID %in% flx_name & keep_year == TRUE)
  flx_seasons_site$SITE_ID <- flx_name
  
  # Subset yearly data based on years included, AND where NEE_VUT_REF_QC is > 0.50
  site_yearly_sub <- subset(site_yearly, TIMESTAMP %in% flx_seasons_site$year & 
                              NEE_VUT_REF_QC > 0.5)
  
  # Need to subset again based on yearly data and gaps?
  
  # Detrend GPP based on yearly data
  # might want to create this as a function
  mktest_result <- trend::mk.test(site_yearly_sub$GPP_NT_VUT_REF) # do mann kendall test for trend over time
  # If trend is signficant at 0.05, then detrend GPP, otherwise do nothing
  if (mktest_result$p.value < 0.05){
    x <- site_yearly_sub$TIMESTAMP
    y <- site_yearly_sub$GPP_NT_VUT_REF
    zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
    # subtract zyp.sen trend from original GPP values to get detrended version of GPP
    site_yearly_sub$GPP_NT_VUT_REF_annual_detrend <- site_yearly_sub$GPP_NT_VUT_REF - (zypsen_form$coefficients[2]*site_yearly_sub$TIMESTAMP + zypsen_form$coefficients[1])
    site_yearly_sub$detrended <- TRUE # specify that this site-season combination is detrended
    
    # Fix nonzero center due to sen's slope if needed for anomalies, don't need this right now
    #df_spring$anom_GPP[site_inds] <- df_spring$anom_GPP[site_inds] - mean(df_spring$anom_GPP[site_inds])
    print(paste("Detrended", flx_name, "yearly GPP", sep = " "))
  } else {
    site_yearly_sub$GPP_NT_VUT_REF_annual_detrend <- site_yearly_sub$GPP_NT_VUT_REF # "detrended" GPP column is same as regular GPP column
    site_yearly_sub$detrended <- FALSE # specify that this site-season combination is NOT detrended
  }
  
  combine_site_info <- merge(subset(flx_seasons_site, select = c("SITE_ID", "year", "P_F_sum")), 
                             subset(site_yearly_sub, select = c("TIMESTAMP", "GPP_NT_VUT_REF_annual_detrend", "detrended")),
                             by.x = "year", by.y = "TIMESTAMP") # this *should* remove years missing based on the NEE QC too
  
  # Slope and correlation
  fit <- lm(combine_site_info$GPP_NT_VUT_REF_annual_detrend ~ combine_site_info$P_F_sum)
  ct <- cor.test(combine_site_info$P_F_sum, combine_site_info$GPP_NT_VUT_REF_annual_detrend) # correlation
  annual_slope <- data.frame(matrix(NA, nrow = 1, ncol = 3))
  colnames(annual_slope) <- c("SITE_ID", "Slope", "Cor_Pearson")
  annual_slope$SITE_ID <- flx_name
  annual_slope$Slope <- fit$coefficients[2] # slope
  annual_slope$Cor_Pearson <- ct$estimate # Pearson correlation
  
  if(i == 1){
    combine_site_info_all <- combine_site_info
    annual_slope_all <- annual_slope

  } else {
    combine_site_info_all <- rbind.data.frame(combine_site_info_all, combine_site_info)
    annual_slope_all <- rbind.data.frame(annual_slope_all, annual_slope)
  }
}

#####
# will need to add aridity column on to this

# Aridity values from the terraclimate data
tc_aridity <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

annual_slope_all_tc <- merge(annual_slope_all, tc_aridity) # merge in, merges on SITE_ID

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/annual_compare")

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x
annual_gpp_slope <- ggplot(annual_slope_all_tc, aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_point() + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  #labs(title = flx_name, x = "Spring Precip", y = "Annual GPP (detrended)") +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula,
               aes(x = TC_aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, #log.formula,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  #coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", 
       y = bquote('Annual GPP ~ Spring Precip slope [g C' ~m^-2 *' / mm]')) +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
# it looks like higher precip in Spring is associated with higher annual GPP for water-limited sites, but it's a toss-up for energy-limited, annual GPP can go either way
ggsave("annual_gpp_spring_precip_tc_aridity_compare_corpearson.png",
       annual_gpp_slope, width = 5, height = 4, units = "in")

annual_gpp_cor <- ggplot(annual_slope_all_tc, aes(x = TC_aridity, y = Cor_Pearson)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_point() + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + #, formula = log.formula) +
  #labs(title = flx_name, x = "Spring Precip", y = "Annual GPP (detrended)") +
  #geom_text_repel(aes(label = SITE_ID, color = Climate), size = 2.5, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula,
               aes(x = TC_aridity,
                   y = Cor_Pearson,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, #log.formula,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.9) +
  #coord_cartesian(ylim = c(-1.75, 2.75)) +
  labs(x = "Wetness Index (P/PET)", 
       y = bquote('Annual GPP ~ Spring Precip Pearson Correlation')) +
  scale_x_continuous(trans = "log10", breaks = c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4),
                     labels = as.character(c(c(0.125, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.5, 2, 3, 4)))) +
  theme(#legend.position = c(0.85, 0.8), 
    legend.key = element_blank(), 
    panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
ggsave("annual_gpp_spring_precip_tc_aridity_compare_slope.png",
       annual_gpp_cor, width = 5, height = 4, units = "in")

annual_slope_all_tc_dry <- subset(annual_slope_all_tc, subset = TC_aridity < 0.65)
length(which(annual_slope_all_tc_dry$Slope > 0)) # 12/15 (80%) are positive
annual_slope_all_tc_wet <- subset(annual_slope_all_tc, subset = TC_aridity >= 0.65)
length(which(annual_slope_all_tc_wet$Slope < 0)) # 27/46 (59%) are negative

# maybe need to talk more about the distribution?

# Need to setup Nature journal figure sizing for text, dimensions, other related things like that
# higher spring precip is associated with higher ANNUAL GPP at water-limited sites
# higher spring precip is NOT related to higher ANNUAL GPP at energy limited sites, instead it's mixed depending on sites