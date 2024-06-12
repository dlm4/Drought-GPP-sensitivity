library(ncdf4)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(ggpmisc)
library(ggrepel)
library(ggtext)

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/summaries/")
tc_var <- nc_open("TerraClimate19812010_ppt.nc")

tc_var$var$ppt$size
# lon, lat, months climatologies

ppt <- ncvar_get(tc_var, "ppt")


tc_var <- nc_open("TerraClimate19812010_pet.nc")
pet <- ncvar_get(tc_var, "pet")

# From best practices page for point data
# enter in longitude, latitude here
x<-c(-77.71, -1.59)

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
#var="aet"
var <- "ppt"

#install.packages("ncdf4")
#library(ncdf4)

#baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

#nc <- nc_open(baseurlagg)
nc <- nc_open("TerraClimate19812010_ppt.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
flat = match(abs(lat - x[2]) < 1/48, 1)
latindex = which(flat %in% 1)
flon = match(abs(lon - x[1]) < 1/48, 1)
lonindex = which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)


# read in the full period of record using aggregated files

data <- as.numeric(ncvar_get(nc, varid = var,start = start, count))


#####

# compare aridity
# terraclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars <- read.csv("flx_site_combinedsource_terraclimate_data.csv")

# Read in list of sites and sensitivities
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/icos_2020_warm_winter/processed_data/")
flx_site_sens <- read.csv("combined_source_ec_univariate_slopes2.csv")

# # Precip trends, spring only, at least 10 years of observations
# flx_site_sens_sub <- flx_site_sens[which(flx_site_sens$Met_Var == "P_F_sum" & flx_site_sens$Season == "Spring" & flx_site_sens$Npts >= 10),]
# 
# # merge together
# flx_merged <- merge(flx_site_sens_sub, flx_tc_vars, by = "SITE_ID")

# PET
flx_tc_vars_pet <- subset(flx_tc_vars, select = c(SITE_ID, pet_01, pet_02, pet_03, pet_04, pet_05, pet_06, pet_07, pet_08, pet_09, pet_10, pet_11, pet_12))
flx_tc_vars_pet_melt <- melt(flx_tc_vars_pet, id.vars = list("SITE_ID"))
flx_tc_vars_pet_sum <- aggregate(flx_tc_vars_pet_melt$value, by = list(flx_tc_vars_pet_melt$SITE_ID), FUN = sum)
colnames(flx_tc_vars_pet_sum) <- c("SITE_ID", "PET")

# PPT
flx_tc_vars_ppt <- subset(flx_tc_vars, select = c(SITE_ID, ppt_01, ppt_02, ppt_03, ppt_04, ppt_05, ppt_06, ppt_07, ppt_08, ppt_09, ppt_10, ppt_11, ppt_12))
flx_tc_vars_ppt_melt <- melt(flx_tc_vars_ppt, id.vars = list("SITE_ID"))
flx_tc_vars_ppt_sum <- aggregate(flx_tc_vars_ppt_melt$value, by = list(flx_tc_vars_ppt_melt$SITE_ID), FUN = sum)
colnames(flx_tc_vars_ppt_sum) <- c("SITE_ID", "PPT")

flx_tc_vars_aridity <- flx_tc_vars_pet_sum
flx_tc_vars_aridity$PPT <- flx_tc_vars_ppt_sum$PPT
flx_tc_vars_aridity$TC_aridity <- flx_tc_vars_aridity$PPT / flx_tc_vars_aridity$PET

# get flx site info
flx_site_info <- read_csv("../combined_source_site_list_info_gpp_ignore_years_cru_aridity_subset2_fixedsource.csv")
flx_info_merged <- merge(flx_site_info, flx_tc_vars_aridity, by = "SITE_ID")
flx_slope_merged <- merge(flx_info_merged, flx_site_sens, by = "SITE_ID")

# Just keep spring and precip
flx_slope_merged_spring_precip <- subset(flx_slope_merged, Season == "Spring" & Met_Var == "P_F_sum")

log.formula <- y ~ I(log10(x))
my.formula <- y ~ x


# climate color scheme
# color scheme from peel et al. 2007
# https://hess.copernicus.org/articles/11/1633/2007/
climate_names <- c("Bwh", "Bsh", "Bsk",
                   "Csa", "Csb", 
                   "Cfa", "Cfb",
                   "Dfa", "Dfb", "Dfc", "Dwc")
climate_colors <- c("#e13122", "#eba83a", "#f8e074",
                    "#f6f651", "#c7c941",
                    "#d1f56e", "#8ae756",
                    "#75f9fb", "#67c4f3", "#33757f", "#4f54aa")

# igbp color scheme
igbp_names <- c("ENF", "EBF", "DBF", "MF", 
                "CSH", "OSH", "SAV", "WSA", "GRA",  
                "WET")
igbp_colors <- c("#377e22", "#75fb4c", "#b1fca3", "#52976a",
                 "#8d3a64", "#f7cea0", "#d6fed0", "#f7ce46", "#f19e38",
                 "#2a6495")

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data/figures/npts_loops")

for (npts_min in 5:20){
  npts_min_text <- as.character(npts_min)
  if (npts_min < 10) {npts_min_text <- paste("0", npts_min_text, sep = "")}
  
  flx_slope_merged_spring_precip_sub <- subset(flx_slope_merged_spring_precip, subset = Npts >= npts_min)
  
  title_phrase <- paste(npts_min, "+ years of data; ", nrow(flx_slope_merged_spring_precip_sub), " flux sites", sep = "")
  
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = Climate), color = "black", size = 1, shape = 21) +
    geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 2, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
    stat_poly_eq(formula = my.formula, 
                 aes(x = log10(TC_aridity),
                     y = Slope,
                     label = paste(..eq.label.., sep = "")),
                 eq.x.rhs = "log(x)",
                 output.type = "text",
                 parse = FALSE, size = 3.5, na.rm = TRUE,
                 label.y = 0.93, label.x = 0.9) +
    stat_poly_eq(formula = log.formula, 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE, size = 3.5, na.rm = TRUE,
                 label.y = 0.89, label.x = 0.9) +
    coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "TerraClimate Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = climate_names, values = climate_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_slope
  
  # vertical histogram of slopes as Trevor suggested, combine with ggpubr
  flx_slope_merged_spring_precip_sub$AI_threshold <- "wet"
  flx_slope_merged_spring_precip_sub$AI_threshold[which(flx_slope_merged_spring_precip_sub$TC_aridity < 0.65)] <- "dry"
  p_spring_hist <- ggplot(data = flx_slope_merged_spring_precip_sub) +
    geom_histogram(aes(y = Slope, fill = AI_threshold), binwidth = 0.2, color = "black") +
    coord_cartesian(ylim = c(-1.75, 2.75)) +
    labs(y = "", fill = "Aridity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  #p_spring_hist
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("terraclimate_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
  
  # version with IGBP
  p_spring_slope <- ggplot(data = flx_slope_merged_spring_precip_sub, aes(x = TC_aridity, y = Slope)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0.65, linetype = "dotted") +
    geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
    geom_point(aes(fill = IGBP), color = "black", size = 2, shape = 21) +
    geom_text_repel(aes(label = SITE_ID), color = "gray20", size = 1, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
    stat_poly_eq(formula = my.formula, 
                 aes(x = log10(TC_aridity),
                     y = Slope,
                     label = paste(..eq.label.., sep = "")),
                 eq.x.rhs = "log(x)",
                 output.type = "text",
                 parse = FALSE, size = 3.5, na.rm = TRUE,
                 label.y = 0.93, label.x = 0.9) +
    stat_poly_eq(formula = log.formula, 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE, size = 3.5, na.rm = TRUE,
                 label.y = 0.89, label.x = 0.9) +
    coord_cartesian(xlim = c(0, 4), ylim = c(-1.75, 2.75)) + # ylim = c(-1.3, 1.3)
    scale_x_continuous(breaks = c(0, 0.65, 1, 2, 3, 4), labels = c("0", "0.65", "1", "2", "3", "4")) + 
    labs(x = "TerraClimate Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "IGBP",
         title = title_phrase) +
    scale_color_discrete(guide = "none") +
    scale_fill_manual(breaks = igbp_names, values = igbp_colors) +
    theme(#legend.position = c(0.85, 0.8), 
      legend.key = element_blank(), 
      panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
  p_spring_slope
  
  p_spring_slope_hist <- ggarrange(p_spring_slope, p_spring_hist, nrow = 1, align = "h", labels = "auto", widths = c(2,1))
  ggsave(paste("terraclimate_added_sites_gpp_precip_ec_spring_cru_aridity_log10_slope_hist_AIthreshold_IGBP_min", npts_min_text, ".png", sep = ""), 
         p_spring_slope_hist, width = 10, height = 5, units = "in")
}
