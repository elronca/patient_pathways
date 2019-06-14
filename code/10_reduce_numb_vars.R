##########################################################################################
##
## Clean up variables
##
##########################################################################################

library(tidyverse)

load(file = file.path("workspace", "acute_edited.RData"))

diagnostic_vars <- grep("orig", names(cases), value = TRUE)

diagnostic_vars <- select(cases, c("id", diagnostic_vars))

cases <- select(cases, c("pat_id", "dis_year","seq_no_corr", "age", "sex", "LOS_OECD", "BUR", "type", "type_2", "pd", "pd_add", "d_p_treat", 
           "d_next_hosp", "pd_title_group", "clinic","acute_chron", "center_SCI", "cause", "lesion_level_d", "lesion_level", 
           "completeness", "admission_pre", "admission_mode", "discharge_post_treat", "discharge_place",
           "pd_title_full", "pd_title_lvl_3", "pd_title_chapter", "acute_new", "id"))




save(cases, file = file.path("workspace", "vars_reduced.RData"))
save(diagnostic_vars, file = file.path("workspace", "diagnostic_vars.RData"))


rm(cases, diagnostic_vars)
