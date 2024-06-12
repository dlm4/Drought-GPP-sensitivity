library(tidyverse)
library(reshape2)

# terraclimate vars
setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
flx_tc_vars <- read.csv("flx_site_combinedsource_terraclimate_data.csv")


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

setwd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/processed_data")
write.csv(flx_tc_vars_aridity, "flx_site_combinedsource_terraclimate_data_aridity_sites.csv", row.names = FALSE)
