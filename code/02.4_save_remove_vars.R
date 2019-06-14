
#############################################################
##
##  Save/Remove/Order variables
##
#############################################################

library(tidyverse)

load(file = file.path(".", "workspace", "cases_releveled.RData"))


# Side diagnoses ----------------------------------------------------------

side_diagnoses <- c("id", "sd_1", "sd_2", 
                    "sd_3", "sd_4", "sd_5", "sd_6", "sd_7", "sd_8", "sd_9", "sd_10", 
                    "sd_11", "sd_12", "sd_13", "sd_14", "sd_15", "sd_16", "sd_17", 
                    "sd_18", "sd_19", "sd_20", "sd_21", "sd_22", "sd_23", "sd_24", 
                    "sd_25", "sd_26", "sd_27", "sd_28", "sd_29", "sd_30", "sd_31", 
                    "sd_32", "sd_33", "sd_34", "sd_35", "sd_36", "sd_37", "sd_38", 
                    "sd_39", "sd_40", "sd_41", "sd_42", "sd_43", "sd_44", "sd_45", 
                    "sd_46", "sd_47", "sd_48", "sd_49")

vars_side_diagnoses <- select(cases, side_diagnoses)

save(vars_side_diagnoses, file = file.path(".", "workspace", "vars_side_diagnoses.RData"))


# Time related ----------------------------------------------------------

time_vars <- c("id", "dis_year", "LOS_OECD",
               "d_next_hosp", 
               "Tage_bis_zur_nächsten_teilstat._Hospitalisierung", 
               "Tage_bis_zur_nächsten_ambul._Hospitalisierung", 
               "d_adm_to_p_treat", "seq_no", "seq_no_corr",
               "Alter_in_Tagen_für__Einjährige",
               "adm_month", "Aufenthalt_in_Intensivmedizin_in_Stunden", 
               "Administrativer_Urlaub_und_Ferien_in_Stunden",
               "Dauer_der_künstlichen_Beatmung_Stunden",
               "d_p_treat", "dis_month", "adm_year", "adm_date", "dis_date")

vars_date_time <- select(cases, time_vars)

save(vars_date_time, file = file.path(".", "workspace", "vars_date_time.RData"))


# Remove not used variables

time_vars <- setdiff(time_vars, c("id", "LOS_OECD", "seq_no_corr", "d_p_treat", "dis_year"))

cases <- select(cases, -time_vars)



# Treatment characteristics -----------------------------------------------

admin_char <- c("id", "admission_pre", "admission_mode", 
                "admission_aut", "treat_mode", "cost_class", 
                "cost_site", "cost_site2", "discharge_dec",
                "discharge_post_treat", "type", "discharge_place")

vars_adm_char <- select(cases, admin_char)

save(vars_adm_char, file = file.path(".", "workspace", "vars_adm_char.RData"))


# Remove temporarly not used variables

admin_char <- setdiff(admin_char, c("id", "type"))

cases <- select(cases, -admin_char)



# Medication -----------------------------------------------

med_vars <- c("id", "Medikament_1", "Medikament_2", "Medikament_3", "Medikament_4", 
                   "Medikament_5", "Medikament_6", "Medikament_7", "Medikament_8", 
                   "Medikament_9", "Medikament_10", "Medikament_11", "Medikament_12", 
                   "Medikament_13", "Medikament_14")

vars_medi <- select(cases, med_vars)

save(vars_medi, file = file.path(".", "Workspace", "vars_medi.RData"))


# Remove not used variables

med_vars <- setdiff(med_vars, c("id"))

cases <- select(cases, -med_vars)


# Personal characteristics -----------------------------------------------

vars_pers_char <- select(cases, one_of("id", "Schweizer___Nicht_Schweizer"))

save(vars_pers_char, file = file.path(".", "workspace", "vars_pers_char.RData"))

cases <- select(cases, -Schweizer___Nicht_Schweizer)



# Case information -----------------------------------------------

case_vars <- c("id", "Rekordart","stat_type", "Patientengruppen_Datensatz",
               "Schweregrad_der_akuten_Erkrankung", "Art_des_Score", 
               "NEMS,_Total_aller_Schichten", "Grund_des_1._Wiedereintrittes", 
               "Grund_des_2._Wiedereintrittes", "Grund_des_3._Wiedereintrittes", 
               "Grund_des_4._Wiedereintrittes", "Weitere_Wiedereintritte", "DRG_Status")

vars_case_info <- select(cases, case_vars)

save(vars_case_info, file = file.path(".", "workspace", "vars_case_info.RData"))

case_vars <- setdiff(case_vars, c("id"))

cases <- select(cases, -case_vars)




# Change order of variables and records -----------------------------------------------

cases <- select(cases, pat_id, seq_no_corr, age, sex, LOS_OECD, type, clinic, BUR, 
                pd, pd_add, sd_1:sd_49, d_p_treat, dis_year, id) %>% 
  
  arrange(pat_id, seq_no_corr) 



# Save dataset - Clear workspace -----------------------------------

save(cases, file = file.path(".", "workspace", "cases_sorted.RData"))

rm("admin_char", "case_vars", "cases", "med_vars", "side_diagnoses", 
  "time_vars", "vars_adm_char", "vars_case_info", "vars_date_time", 
  "vars_medi", "vars_pers_char", "vars_side_diagnoses")