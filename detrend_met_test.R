# test detrend met data

library(tidyverse)
library(reshape2)
library(trend)
library(zyp)
library(ggpmisc) # for geom_smooth equations, masks annotate from ggplot2
`%notin%` <- Negate(`%in%`) # %notin% function

# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons.csv")

#####
########### Filtering, GPP detrending, and climate relabeling prep work  ##########

# Filter by keep_year column, keep only good years
df_all <- df_all[df_all$keep_year,]

# Filter by NEE QC > 0.5
df_all <- df_all[which(df_all$NEE_VUT_REF_QC_mean > 0.5),]

# Detrend GPP using Sen's slope (if needed) for each site, season
df_all$GPP_detrend <- df_all$GPP_NT_VUT_REF_sum # set GPP detrend column to original GPP column to start
df_all$detrended <- FALSE # is the GPP detrended? Set to FALSE by default then replace

# same for met variable columns
df_all$P_detrend <- df_all$P_F_sum
df_all$detrended_P <- FALSE

df_all$TA_detrend <- df_all$TA_F_mean
df_all$detrended_TA <- FALSE

df_all$SW_IN_detrend <- df_all$SW_IN_F_mean
df_all$detrended_SW_IN <- FALSE


# vec <- df_all$P_F_sum
# vec_new <- df_all$P_detrend
# detrend_logical <- df_all$detrended_P

detrendMK <- function(df_all, site_inds, vec, vec_new, detrend_logical){
  
  # need to remove NAs from site_inds if there are any
  not_na_inds <- !is.na(vec[site_inds])
  site_inds <- site_inds[not_na_inds]
  
  mktest_result <- trend::mk.test(vec[site_inds]) # do mann kendall test for trend over time
  
  # If trend is signficant at 0.05, then detrend GPP, otherwise do nothing
  if (mktest_result$p.value < 0.05){
    x <- df_all$year[site_inds]
    y <- vec[site_inds]
    
    zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
    # subtract zyp.sen trend from original GPP values to get detrended version of GPP
    vec_new[site_inds] <- vec[site_inds] - (zypsen_form$coefficients[2]*df_all$year[site_inds] + zypsen_form$coefficients[1])
    detrend_logical[site_inds] <- TRUE # specify that this site-season combination is detrended
    
    # Fix nonzero center due to sen's slope if needed for anomalies, don't need this right now
    #df_sub$anom_GPP[site_inds] <- df_sub$anom_GPP[site_inds] - mean(df_sub$anom_GPP[site_inds])
    #print(paste("Detrended", site, "for", seas, ":", as.character(vec)sep = " "))
  }
  return(list(vec_new = vec_new, detrend_logical = detrend_logical))
}

# list of sites that will need to be removed because not enough data in one of the seasons
remove_site_id <- c()
remove_site_id_season <- c()

# Loop over seasons: Spring, Summer, Fall
for (seas in unique(df_all$season)){
  # Loop over sites
  for (site in unique(df_all$SITE_ID)){
    print(site)
    # Which for site and season
    site_inds <- which(df_all$season == seas & df_all$SITE_ID == site)
    
    if (length(site_inds) < 5){
      df_all$GPP_NT_VUT_REF_sum[site_inds] <- NA
      remove_site_id <- c(remove_site_id, site)
      remove_site_id_season <- c(remove_site_id_season, seas)
    } else {
      
      mk_list_GPP <- detrendMK(df_all, site_inds, df_all$GPP_NT_VUT_REF_sum, df_all$GPP_detrend, df_all$detrended)
      df_all$GPP_detrend <- mk_list_GPP$vec_new
      df_all$detrended_GPP <- mk_list_GPP$detrend_logical
      
      mk_list_P <- detrendMK(df_all, site_inds, df_all$P_F_sum, df_all$P_detrend, df_all$detrended_P)
      df_all$P_detrend <- mk_list_P$vec_new
      df_all$detrended_P <- mk_list_P$detrend_logical
      
      mk_list_TA <- detrendMK(df_all, site_inds, df_all$TA_F_mean, df_all$TA_detrend, df_all$detrended_TA)
      df_all$TA_detrend <- mk_list_TA$vec_new
      df_all$detrended_TA <- mk_list_TA$detrend_logical
      
      mk_list_SW_IN <- detrendMK(df_all, site_inds, df_all$SW_IN_F_mean, df_all$SW_IN_detrend, df_all$detrended_SW_IN)
      df_all$SW_IN_detrend <- mk_list_SW_IN$vec_new
      df_all$detrended_SW_IN <- mk_list_SW_IN$detrend_logical
      
      
      # mktest_result <- trend::mk.test(df_all$GPP_NT_VUT_REF_sum[site_inds]) # do mann kendall test for trend over time
      # 
      # # If trend is signficant at 0.05, then detrend GPP, otherwise do nothing
      # if (mktest_result$p.value < 0.05){
      #   x <- df_all$year[site_inds]
      #   y <- df_all$GPP_NT_VUT_REF_sum[site_inds]
      #   zypsen_form <- zyp.sen(y~x) # use zyp.sen() to build sen's slope regression with years as x because years may be missing
      #   # subtract zyp.sen trend from original GPP values to get detrended version of GPP
      #   df_all$GPP_detrend[site_inds] <- df_all$GPP_NT_VUT_REF_sum[site_inds] - (zypsen_form$coefficients[2]*df_all$year[site_inds] + zypsen_form$coefficients[1])
      #   df_all$detrended[site_inds] <- TRUE # specify that this site-season combination is detrended
      #   
      #   # Fix nonzero center due to sen's slope if needed for anomalies, don't need this right now
      #   #df_sub$anom_GPP[site_inds] <- df_sub$anom_GPP[site_inds] - mean(df_sub$anom_GPP[site_inds])
      #   print(paste("Detrended", site, "for", seas, sep = " "))
      # }
    }
  }
}

#####

# Some of these would need to be detrended... how to explain why we did not do this.

df_sub <- subset(df_all, subset = season == "Spring")

length(unique(df_sub$SITE_ID[(df_sub$detrended_P == TRUE)])) # AT-Neu, BE-Vie, FI-Hyy, NL-Loo
length(unique(df_sub$SITE_ID[(df_sub$detrended_P == FALSE)])) # 116

length(unique(df_sub$SITE_ID[(df_sub$detrended_SW_IN == TRUE)]))
length(unique(df_sub$SITE_ID[(df_sub$detrended_SW_IN == FALSE)]))

length(unique(df_sub$SITE_ID[(df_sub$detrended_TA == TRUE)]))
length(unique(df_sub$SITE_ID[(df_sub$detrended_TA == FALSE)]))

length(unique(df_sub$SITE_ID[(df_sub$detrended_GPP == TRUE)]))
length(unique(df_sub$SITE_ID[(df_sub$detrended_GPP == FALSE)]))



#####


#df_sub <- df_all[which(df_all$SITE_ID == "GL-NuF"),]

# Column for two-letter Climate label for coloring plots
#df_all$Climate2 <- substring(df_all$Climate, 1, 2) # two letter version of climate for simpler summary
#df_all$Climate2[which(df_all$Climate == "Bwh")] <- "Bs" # moving SRC from Bw (where it would be by itself) to Bs in Climate2 column

# Convert SW_IN_F_mean into a PAR sum
# df_all$PAR_F_sum <- df_all$SW_IN_detrend*3600*24/1000000 * 0.5 # daily PAR estimate in MJ m-2 from SW W m-2 # this is detrended!
# df_all$PAR_F_sum[which(df_all$season == "Spring")] <- df_all$PAR_F_sum[which(df_all$season == "Spring")] * (31+30+31) # sum for number of days in spring
# df_all$PAR_F_sum[which(df_all$season == "Summer")] <- df_all$PAR_F_sum[which(df_all$season == "Summer")] * (30+31+31) # sum for number of days in summer
# df_all$PAR_F_sum[which(df_all$season == "Fall")] <- df_all$PAR_F_sum[which(df_all$season == "Fall")] * (30+31+30) # sum for number of days in fall

#####
# Univariate regressions for each met variable, season, and site
# Median of each met variable for each site and season too
met_vars <- c("P_detrend")
num_rows <- length(met_vars)*length(unique(df_all$season))*length(unique(df_all$SITE_ID)) # number of rows

# adding in climate (three letters) and elevation data

# originally had all these data appended but will need add afterwards and filter
df_sens_colnames <- c("SITE_ID", "Met_Var", "Season", "Slope", "pvalue", "Climate", "Climate2", "IGBP", "Elevation", "Met_Var_Median", "Npts",
                      "PearsonR", "SpearmanR", "RMSE", "Slope_SE")
df_sens <- data.frame(matrix(data = NA, nrow = num_rows, ncol = length(df_sens_colnames)))
colnames(df_sens) <- df_sens_colnames

i <- 1 # indexing adding rows

# Need to do new version of this loop but doing multilinear regression for met_vars: precip, par, and air temp
# Could just call it shortwave instead of par and multiply by 2?

for (met in met_vars){
  for (seas in unique(df_all$season)){
    for (site in unique(df_all$SITE_ID)){
      
      # Variable labels etc
      df_sens$SITE_ID[i] <- site
      df_sens$Met_Var[i] <- met
      df_sens$Season[i] <- seas
      
      # Which for site and season
      site_inds <- which(df_all$season == seas & df_all$SITE_ID == site)
      #climate2 <- df_all$Climate2[site_inds]
      #igbp <- df_all$IGBP[site_inds]
      #climate <- df_all$Climate[site_inds]
      #elev <- df_all$LOCATION_ELEV[site_inds]
      
      #df_sens$Climate2[i] <- climate2[1]
      #df_sens$IGBP[i] <- igbp[1]
      #df_sens$Climate[i] <- climate[1]
      #df_sens$Elevation[i] <- elev[1]
      
      # Which column has the met variable
      met_col <- which(colnames(df_all) == met)
      
      # univariate linear regression
      x <- df_all[site_inds, met_col] # x value, met variable column
      y <- df_all$GPP_detrend[site_inds] # GPP or detrended GPP (if needed)
      
      # if x is empty, median is NA; can happen for NETRAD - G_F
      if (length(x) == 0){
        df_sens$Met_Var_Median[i] <- NA
      } else {
        df_sens$Met_Var_Median[i] <- median(x, na.rm = T) # median of the met val, could have NA
      }
      
      # Need to test for length of site_inds, don't do regression if there are too few points?
      x_not_NA_inds <- which(!is.na(x))
      x <- x[x_not_NA_inds] # only keep x values that aren't NA
      y <- y[x_not_NA_inds] # same for y, GPP
      df_sens$Npts[i] <- length(x) # number of points in regression ie number of years
      
      if (length(x) < 3){
        df_sens$Slope[i] <- NA
        df_sens$pvalue[i] <- NA
      } else {
        # linear model
        lm_result <- lm(y~x) # linear model
        
        df_sens$Slope[i] <- lm_result$coefficients[2] # slope
        df_sens$pvalue[i] <- summary(lm_result)$coefficients[2,4] # p value 
        
        # Correlations
        df_sens$PearsonR[i] <- cor(x, y, method = "pearson")
        df_sens$SpearmanR[i] <- cor(x, y, method = "spearman")
        
        # (added Mar 29, 2022)
        # RMSE
        df_sens$RMSE[i] <- sqrt(mean(lm_result$residuals^2))
        
        # Slope standard error
        df_sens$Slope_SE[i] <- summary(lm_result)$coefficients[2,2]
        
      }
      
      i <- i + 1
    }
  }
}
# factors for different seasons
df_sens$Season <- factor(df_sens$Season, levels = c("Spring", "Summer", "Fall"))

# Remove sites that have less than 5 years of data for any season
site_list <- unique(df_sens$SITE_ID)

# get the only three met vars we're actually using for now
#df_sens_sub <- df_sens[which(df_sens$Met_Var %in% c("P_F_sum", "TA_F_mean", "PAR_F_sum")),]
df_sens_sub <- df_sens

# Remove site slopes with too short of time series (less than 5 years) in any season
for (s in site_list){
  inds <- which(df_sens_sub$SITE_ID == s)
  if(any(df_sens_sub$Npts[inds] < 5)){
    df_sens_sub$Slope[inds] <- NA 
  }
}

df_sens_sub <- df_sens_sub[which(!is.na(df_sens_sub$Slope)),]
# output
df_sens_sub_output <- df_sens_sub[, colnames(df_sens_sub) %notin% c("Climate", "Climate2", "IGBP", "Elevation")]
write.csv(df_sens_sub_output, "combined_source_ec_univariate_slopes2_detrend_P.csv", row.names = F)


#####

# Make plot similar to Figure 1

# Load data file with all seasons

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
df_all <- read.csv("combined_source_info_swc1_all_seasons.csv")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2_detrend_P.csv")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars_aridity <- read.csv("flx_site_combinedsource_terraclimate_data_aridity_sites.csv")

flx_site_info <- read.csv("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource_10springyrs.csv")

flx_site_info_tc <- merge(flx_site_info, flx_tc_vars_aridity, by = "SITE_ID")
flx_site_info_tc_sub <- subset(flx_site_info_tc, select = c(SITE_ID, IGBP, TC_aridity))

flx_site_sens <- merge(flx_site_sens, flx_site_info_tc_sub, by = "SITE_ID")

#----

spring_precip <- subset(flx_site_sens, subset = Met_Var == "P_detrend" & Season == "Spring")
# spring_par <- subset(flx_site_sens, subset = Met_Var == "PAR_F_sum" & Season == "Spring")
# spring_temp <- subset(flx_site_sens, subset = Met_Var == "TA_F_mean" & Season == "Spring")
# 
# spring_precip$Ta_Median <- spring_temp$Met_Var_Median
# spring_precip$PAR_Median <- spring_par$Met_Var_Median

a_level <- 1
point_size <- 2
line_size <- 0.3
yrange <- c(-1.7, 2.9)

# igbp color scheme # revise these colors
igbp_names <- c("ENF", "EBF", "DBF", "MF", 
                "CSH", "OSH", "WSA", "SAV", "GRA",  
                "WET")

pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")

igbp_colors <- c("#377e22", "#9acd32", 
                 "#b1fca3",
                 pal[3], 
                 pal[4], 
                 pal[5],  
                 pal[15], 
                 pal[12], 
                 "#f19e38",
                 "#006ddb")

ec_precip <- ggplot(data = spring_precip,
                    aes(x = TC_aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0.65, linetype = "dotted") +
  geom_smooth(method = "lm", color = "gray15", se = TRUE, size = line_size, fill = "gray90", alpha = 1) +
  geom_linerange(aes(ymin = Slope - Slope_SE, ymax = Slope + Slope_SE), size = line_size, color = "gray50") + # error bar on slope?
  geom_point(aes(fill = IGBP), color = "gray50", size = point_size, shape = 21) +
  geom_smooth(method = "lm", color = "gray15", se = FALSE, size = 0.5) +
  scale_x_continuous(trans = "log10", breaks = c(0.3, 0.65, 1, 3),
                     labels = as.character(c(0.3, 0.65, 1, 3))) +
  scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
  scale_color_manual(breaks = igbp_names, values = igbp_colors, guide = "none") +
  #scale_fill_gradientn(colors = rev(c('#e66101','#fdb863','#f7f7f7','#b2abd2','#5e3c99'))) +
  #guides(fill = guide_legend(nrow = 5)) +
  coord_cartesian(ylim = yrange) +
  labs(x = "Wetness Index (P/PET)", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3), #panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ec_precip

fit <- lm(spring_precip$Slope ~ log10(spring_precip$TC_aridity))
summary(fit) # y = -1.24 * log10(x) - 0.08, R2 = 0.48, p < 0.001; essentially the same as in Figure 1

ec_only_density <- ggplot(data = spring_precip, 
                          aes(y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = median(spring_precip$Slope), linetype = "dotted", color = "gray20") +
  geom_density(color = "gray20") + 
  coord_cartesian(ylim = yrange) + scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Density", y  = bquote("GPP ~ Precip slope [g C" ~m^-2*" / mm]")) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "black", size = 0.3),
        strip.background = element_blank(), strip.text = element_text(size = 7),
        axis.text = element_text(size = 7, color = "black"), axis.title = element_text(size = 7),
        legend.key = element_blank(), legend.title = element_text(size = 7), legend.text = element_text(size = 7),
        legend.position = c(0.9, 0.75), legend.key.size = unit(2.5, "mm"), legend.background = element_blank())
#ec_only_density

fig1_arrange_ec <- ggarrange(ec_precip, ec_only_density,
                             widths = c(3, 1.25), nrow = 1, labels = "auto", font.label = list(size = 8))
#ggsave("ec_only_precip_spring_slope_se_density_arranged_update_p_lt_05.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/updated_figs")
ggsave("ec_only_precip_spring_slope_se_density_arranged_update_detrend_P.eps", fig1_arrange_ec, width = 120, height = 75, units = "mm", dpi = 300)
