library(tidyverse)
library(ggpubr)
library(reshape2)
library(ggpmisc)
library(ggrepel)
library(ggtext)

# library(showtext)
# font_add()
# can leave font as default Arial for now, will figure out how to change to Helvetica if I have to later on

# Load in flux site data and trendy associated data

# new aridity output plots, flux site data
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/plots/flux_site_summary_energy_swc1_sensitivity/new_aridity")
ec_p <- read.csv("ec_gpp_sens_P_F_sum.csv")
ec_ta <- read.csv("ec_gpp_sens_TA_F_mean.csv")
ec_par <- read.csv("ec_gpp_sens_PAR_F_sum.csv")

# trendy grid cells
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/results/v4_ec_site_locations")
trendy_models <- read.csv("allmodels_gpp_precip_ec_sites_data.csv")

trendy_models <- subset(trendy_models, ModelName != "SDGVM") # remove SDGVM

#####
# combine ec data
ec_all <- rbind.data.frame(ec_p, ec_ta, ec_par)

# add cru aridity from trendy model outputs to ec data
# subset(trendy_models, trendy_models$SITE_ID == "US-UMB", c(SITE_ID, ModelName, Aridity))
# trendy_models$Aridity[trendy_models$SITE_ID == "US-Ha1"]
# trendy_models$Aridity[trendy_models$SITE_ID == "US-UMB"]

trendy_models_sub <- subset(trendy_models, select = c(Aridity, Precip_Slope, Ta_Slope, PAR_Slope))
trendy_model_means <- aggregate(trendy_models_sub, by = list(trendy_models$SITE_ID, trendy_models$Season), mean)
colnames(trendy_model_means)[1:3] <- c("SITE_ID", "Season", "CRU_Aridity")

trendy_model_means$Season <- tools::toTitleCase(trendy_model_means$Season) # capitalize first letter of Season column for merge
# melt trendy model means for met vars
colnames(trendy_model_means)[4:6] <- c("P_F_sum", "TA_F_mean", "PAR_F_sum")
trendy_model_means_melt <- melt(trendy_model_means, id.vars = c("SITE_ID", "Season", "CRU_Aridity"))
colnames(trendy_model_means_melt)[4:5] <- c("Met_Var", "Trendy_Slope")
ec_trendy_merged <- merge(ec_all, trendy_model_means_melt, by = c("SITE_ID", "Season", "Met_Var"))

ec_trendy_merged$Season <- factor(ec_trendy_merged$Season, levels = c("Spring", "Summer", "Fall"))

#####
# List of sites for each season, need intersection
spring_sites <- unique(ec_trendy_merged$SITE_ID[which(ec_trendy_merged$Season == "Spring")])
summer_sites <- unique(ec_trendy_merged$SITE_ID[which(ec_trendy_merged$Season == "Summer")])
fall_sites <- unique(ec_trendy_merged$SITE_ID[which(ec_trendy_merged$Season == "Fall")])

common_sites <- Reduce(intersect, list(spring_sites, summer_sites, fall_sites))

ec_trendy_merged_common <- ec_trendy_merged[which(ec_trendy_merged$SITE_ID %in% common_sites),]
setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/results/ec_trendy_compare")
write.csv(common_sites, "ec_sites_list_common_all_season.csv", row.names = F)
write.csv(ec_trendy_merged_common, "ec_trendy_merged_sites_common_all_season.csv", row.names = F)

#####
# plot ec aridity vs cru aridity avg for spring
df_aridity_compare <- subset(ec_trendy_merged_common, subset = Season == "Spring" & Met_Var == "P_F_sum", select = c(SITE_ID, pet_pm_aridity_flip, CRU_Aridity))
colnames(df_aridity_compare)[2] <- "EC_Aridity"

p_aridity_compare <- ggplot(df_aridity_compare, aes(x = CRU_Aridity, y = EC_Aridity)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_text(aes(label = SITE_ID), fontface = "bold", size = 2) +
  coord_equal() +
  theme_bw()
p_aridity_compare
ggsave("ec_cru_aridity_compare.png", p_aridity_compare, width = 8, height = 5, units = "in")

summary(lm(df_aridity_compare$EC_Aridity ~ df_aridity_compare$CRU_Aridity))

# Aridity levels histogram
p_hist_cru_aridity <- ggplot(df_aridity_compare) +
  geom_histogram(aes(x = CRU_Aridity), breaks = seq(0, 4, 0.25), fill = "gray90", color = "black") +
  ylim(0, 20) +
  theme_bw()
p_hist_cru_aridity

p_hist_ec_aridity <- ggplot(df_aridity_compare) +
  geom_histogram(aes(x = EC_Aridity), breaks = seq(0, 4, 0.25), fill = "gray90", color = "black") +
  ylim(0,20)+
  theme_bw()
p_hist_ec_aridity

p_hist_aridity <- ggarrange(p_hist_cru_aridity, p_hist_ec_aridity, nrow = 2, labels = "AUTO")
ggsave("hist_aridity_compare.png", p_hist_aridity, width = 8, height = 7, units = "in")


#####



# grid plot, points for sites?
# need to rewrite using common sites...
ggplot(ec_trendy_merged) + 
  geom_point(aes(x = CRU_Aridity, y = Slope), color = "blue") +
  geom_point(aes(x = CRU_Aridity, y = Trendy_Slope), color = "red") +
  # text labels instead
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  theme_bw()

p_ec_cru_aridity <- ggplot(ec_trendy_merged) + 
  geom_text(aes(x = CRU_Aridity, y = Slope, label = SITE_ID), size = 2, fontface = "bold", color = "gray20") +
  geom_text(aes(x = CRU_Aridity, y = Trendy_Slope, label = SITE_ID), size = 2, fontface = "bold", color = "blue") +
  #geom_point(aes(x = CRU_Aridity, y = Trendy_Slope), color = "red") +
  # text labels instead
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  theme_bw()
p_ec_cru_aridity
ggsave("ec_cru_aridity.png", p_ec_cru_aridity, width = 12, height = 10, units = "in")

#####

# Binning for aridity levels
aridity_breaks <- c(0, 0.5, 1, 1.5, 10) # need to relevel?
ec_trendy_merged_common$CRU_Breaks <- cut(ec_trendy_merged_common$CRU_Aridity, aridity_breaks)

table(ec_trendy_merged_common$CRU_Breaks, ec_trendy_merged_common$Season)/3

# box plot
ggplot(ec_trendy_merged_common) +
  geom_boxplot(aes(x = CRU_Breaks, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  theme_bw()


# copy and align slope and trendy slope with new label to do colored box plot together
ect1 <- ec_trendy_merged_common
ect2 <- ec_trendy_merged_common

ect1$Slope_Label <- "FLUXNET" # label which is which
ect2$Slope_Label <- "TRENDY"

ect2$Slope <- ect2$Trendy_Slope # replace the slope in ect2 with the trendy slope

ecmc <- rbind.data.frame(ect1, ect2) # combine
ecmc$Trend_Slope <- NA # remove original trendy slope column from analysis, it's now in Slope column

# new box plot
ecmc$Met_Var[ecmc$Met_Var == "P_F_sum"] <- "P [gC / mm]"
ecmc$Met_Var[ecmc$Met_Var == "PAR_F_sum"] <- "PAR [gC / MJ]"
ecmc$Met_Var[ecmc$Met_Var == "TA_F_mean"] <- "Ta [gC / *C]"

p_ecmc <- ggplot(ecmc) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = CRU_Breaks, y = Slope, color = Slope_Label), fill = "white", outlier.size = 1) +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  scale_color_manual(values = c("#3182bd", "#31a354")) +
  labs(x = "CRU Aridity") +
  theme_bw()
p_ecmc
ggsave("ec_trendy_slope_boxplot_compare.png", p_ecmc, width = 8, height = 6, units = "in")


#####
# Simplified figure 1, just trend line from Spring flux sites

# remove australia sites too
ec_spring_precip <- subset(ec_trendy_merged_common, Season == "Spring" & Met_Var == "P_F_sum" & SITE_ID != "AU-Stp" & SITE_ID != "AU-Tum")
my.formula <- y ~ x
log.formula <- y ~ I(log10(x))
loge.formula <- y ~ I(log(x))
inv.formula <- y ~ I(1/x)

#nudge_x = 0.17, nudge_y = 0.005,

setwd("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/trendy/results/ec_trendy_compare")

# need to set new theme, change legend.position to be inside the plot
# move regression line equation to be inside plot as well
# need to keep y-axis range the same between plots

# original version for reference
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = TRUE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text(aes(label = SITE_ID, color = Climate2), size = 2, fontface = "bold") +
  #geom_text(aes(label = SITE_ID, color = Climate2), size = 2, fontface = "bold", position=position_dodge(0.9)) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  #geom_label_repel(aes(label = SITE_ID, color = Climate2), size = 3, max.overlaps = 30) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log(CRU_Aridity),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.09, label.x = 0.97) +
  stat_poly_eq(formula = loge.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.03, label.x = 0.97) +
  #coord_cartesian(xlim = c(0, 2.5), ylim = c(-1.5, 1.5)) +
  #facet_wrap(~Season, ncol = 3) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme_bw()
p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity.png", p_spring_slope, width = 8, height = 5, units = "in")


# modified, still log10 slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_log10.png", p_spring_slope, width = 6, height = 5, units = "in")


# modified, linear slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_linear.png", p_spring_slope, width = 6, height = 5, units = "in")

# modified, 1/x slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "(1/x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_invx.png", p_spring_slope, width = 6, height = 5, units = "in")


# no site labels
# modified, still log10 slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_log10_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")


# modified, linear slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_linear_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")

# modified, 1/x slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "(1/x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_invx_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")

#####
# versions with trendy slope
# modified, still log10 slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_log10.png", p_spring_slope, width = 6, height = 5, units = "in")


# modified, linear slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_linear.png", p_spring_slope, width = 6, height = 5, units = "in")

# modified, 1/x slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "(1/x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_invx.png", p_spring_slope, width = 6, height = 5, units = "in")


# no site labels
# modified, still log10 slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_log10_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")


# modified, linear slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_linear_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")

# modified, 1/x slope
p_spring_slope <- ggplot(data = ec_spring_precip, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "(1/x)",
               output.type = "text",
               parse = FALSE, size = 3.5, na.rm = TRUE,
               label.y = 0.93, label.x = 0.7) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, na.rm = TRUE,
               label.y = 0.89, label.x = 0.7) +
  coord_cartesian(xlim = c(0, 4), ylim = c(-1.3, 1.3)) +
  labs(x = "Annual Aridity (P/PET)", y = "GPP ~ Precip slope [g C / mm]", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
#p_spring_slope
ggsave("gpp_precip_ec_spring_cru_aridity_trendy_invx_nolabels.png", p_spring_slope, width = 6, height = 5, units = "in")



#####
# multiplot for precip, par, and temp from flux site data
ec_no_au <- subset(ec_trendy_merged_common, SITE_ID != "AU-Stp" & SITE_ID != "AU-Tum")

ec_no_au$Season <- factor(ec_no_au$Season, levels = c("Spring", "Summer", "Fall"), labels = c("Spring", "Summer", "Fall"))
ec_no_au$Met_Var <- factor(ec_no_au$Met_Var, levels = c("P_F_sum", "PAR_F_sum", "TA_F_mean"), 
                           labels = c("GPP ~ Precip [g C / mm]", "GPP ~ PAR [g C / MJ]", "GPP ~ Ta [g C / \u00b0C]")) # label with degree symbol!

# log10
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.88, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_ec_cru_aridity_log10.png", p_slopes_all, width = 10, height = 8, units = "in")

# linear
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.88, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_ec_cru_aridity_linear.png", p_slopes_all, width = 10, height = 8, units = "in")

# 1/x
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.93, label.x = 0.9) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.88, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_ec_cru_aridity_invx.png", p_slopes_all, width = 10, height = 8, units = "in")


#####
# trendy model version
# log10
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = log.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = log10(CRU_Aridity),
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.09, label.x = 0.9) +
  stat_poly_eq(formula = log.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.03, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_trendy_cru_aridity_log10.png", p_slopes_all, width = 10, height = 8, units = "in")

# linear
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = my.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.09, label.x = 0.9) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.03, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_trendy_cru_aridity_linear.png", p_slopes_all, width = 10, height = 8, units = "in")

# 1/x
p_slopes_all <- ggplot(data = ec_no_au, aes(x = CRU_Aridity, y = Trendy_Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = inv.formula) +
  geom_point(aes(fill = Climate2), color = "black", size = 2, shape = 21) +
  #geom_text_repel(aes(label = SITE_ID, color = Climate2), size = 2.5, max.overlaps = 30, bg.color = "white", bg.r = 0.05) +
  stat_poly_eq(formula = inv.formula, 
               aes(x = CRU_Aridity,
                   y = Trendy_Slope,
                   label = paste(..eq.label.., sep = "")),
               #eq.x.rhs = "log(x)",
               output.type = "text",
               parse = FALSE, size = 3, na.rm = TRUE,
               label.y = 0.09, label.x = 0.9) +
  stat_poly_eq(formula = inv.formula, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               parse = TRUE, size = 3, na.rm = TRUE,
               label.y = 0.03, label.x = 0.9) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = "Annual Aridity (P/PET)", y = "Slope", fill = "Climate") +
  scale_color_discrete(guide = "none") +
  facet_grid( rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") + # switch labels to left side
  theme(legend.key = element_blank(), strip.background = element_blank(), strip.placement = "outside", strip.text.y = ggtext::element_markdown(), # keep labels outside and parse for y strip only
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_slopes_all
ggsave("gpp_slopes_grid_trendy_cru_aridity_invx.png", p_slopes_all, width = 10, height = 8, units = "in")


#####
# check lat lon locations of site list
flux_site_locations <- read.csv("C:/Users/David Miller/Documents/dlm_files/keenan_postdoc/fluxnet2015/fluxnet2015_site_locations_plaintext.csv")
colnames(flux_site_locations)[1] <- "SITE_ID" # label was scrambled on import
flux_site_locations_sub <- flux_site_locations[which(flux_site_locations$SITE_ID %in% ec_spring_precip$SITE_ID),]

# plot map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
coastlinemap <- ne_coastline(scale = 'medium', returnclass = 'sf')
ggplot(flux_site_locations_sub) +
  geom_sf(data = coastlinemap, color = "gray50", size = 0.25) + 
  geom_point(aes(x = LOCATION_LONG, y = LOCATION_LAT), shape = 1) +
  #geom_text_repel(aes(x = LOCATION_LONG, y = LOCATION_LAT, label = SITE_ID), size = 1, max.overlaps = 50, bg.color = "white", bg.r = 0.05) +
  coord_sf(ylim = c(30, 85), expand = FALSE) + theme_bw()

#####
# Box plot comparison

# copy and align slope and trendy slope with new label to do colored box plot together
ect1 <- ec_no_au
ect2 <- ec_no_au

ect1$Slope_Label <- "FLUXNET" # label which is which
ect2$Slope_Label <- "TRENDY"

ect2$Slope <- ect2$Trendy_Slope # replace the slope in ect2 with the trendy slope

ecmc_no_au <- rbind.data.frame(ect1, ect2) # combine
ecmc_no_au$Trendy_Slope <- NA # remove original trendy slope column from analysis, it's now in Slope column

# set aridity bins
#aridity_bins <- c(0, 0.5, 0.75, 1, 1.25, 1.5, 2, 10)
aridity_bins <- c(0, 0.5, 1, 1.5, 10)
ecmc_no_au$aridity_bins <- cut(ecmc_no_au$CRU_Aridity, aridity_bins)

#####
# Spring precip only
ecmc_no_au_springp <- subset(ecmc_no_au, subset = Season == "Spring" & Met_Var == "GPP ~ Precip [g C / mm]")
table(ecmc_no_au_springp$aridity_bins, ecmc_no_au_springp$Season)
# plot boxplot

p_ecmc <- ggplot(ecmc_no_au_springp) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = aridity_bins, y = Slope, color = Slope_Label), fill = "white", outlier.size = 1) +
  #facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  scale_color_manual(values = c("#3182bd", "#31a354")) +
  labs(x = "Annual Aridity [P / PET]", y = "GPP ~ Precip [g C / mm]", color = "Data Source") +
  coord_cartesian(ylim = c(-1.3, 1.3)) + 
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA))
p_ecmc
ggsave("gpp_precip_spring_boxplot_compare.png", p_ecmc, width = 6, height = 5, units = "in")

# FIGURE 2??
# try again, correct size text
p_ecmc <- ggplot(ecmc_no_au_springp) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = aridity_bins, y = Slope, color = Slope_Label), fill = "white", outlier.size = 0.75, weight = 0.5) +
  #facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y") +
  scale_color_manual(values = c("#3182bd", "#31a354")) +
  labs(x = "Annual Aridity [P / PET]", y = "GPP ~ Precip [g C / mm]", color = "Data Source") +
  coord_cartesian(ylim = c(-1.3, 1.3)) +
  scale_x_discrete(breaks = c("(0,0.5]", "(0.5,1]", "(1,1.5]", "(1.5,10]"),
                   labels = c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5+")) +
  theme(legend.position = c(0.85, 0.8), legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 6), legend.title = element_text(size = 7),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6, color = "black"),
        axis.ticks = element_line(color = "black"))
#p_ecmc
ggsave("gpp_precip_spring_boxplot_compare_size2.png", p_ecmc, width = 89, height = 80, units = "mm")
ggsave("gpp_precip_spring_boxplot_compare_size2.pdf", p_ecmc, width = 89, height = 80, units = "mm")

# Create figure with all comparisons, similar to initial version I showed Trevor
p_ecmc_all <- ggplot(ecmc_no_au) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = aridity_bins, y = Slope, color = Slope_Label), fill = "white", outlier.size = 0.75, size = 0.5) +
  facet_grid(rows = vars(Met_Var), cols = vars(Season), scales = "free_y", switch = "y") +
  scale_color_manual(values = c("#3182bd", "#31a354")) +
  labs(x = "Annual Aridity [P / PET]", y = NULL, color = "Data Source") +
  #coord_cartesian(ylim = c(-1.3, 1.3)) +
  scale_x_discrete(breaks = c("(0,0.5]", "(0.5,1]", "(1,1.5]", "(1.5,10]"),
                   labels = c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5+")) +
  theme(#legend.position = c(0.85, 0.8), 
        legend.key = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", fill = NA), 
        strip.placement = "outside", strip.text.y = ggtext::element_markdown(), strip.background = element_blank(), strip.text = element_text(size = 7),
        legend.text = element_text(size = 6), legend.title = element_text(size = 7),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6, color = "black"),
        axis.ticks = element_line(color = "black"))
ggsave("gpp_boxplot_compare_all.png", p_ecmc_all, width = 183, height = 150, units = "mm")
ggsave("gpp_boxplot_compare_all.pdf", p_ecmc_all, width = 183, height = 150, units = "mm")
