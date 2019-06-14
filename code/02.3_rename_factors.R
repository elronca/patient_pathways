
#############################################################
##
##  Rename categories of variables
##
#############################################################


library(tidyverse)

load(file = file.path(".", "workspace", "cases_dates_sorted.RData"))


# Recode variable types --------------------------------------------

to_factor <- c("sex", "type", "clinic", "Schweizer___Nicht_Schweizer", "admission_pre", "admission_mode", "admission_aut", 
               "cost_class", "cost_site", "cost_site2", "discharge_dec", "discharge_place", 
               "discharge_post_treat")

cases <- cases %>% 
  mutate_at(vars(to_factor), as.factor) %>%
  mutate_at(vars(id), as.character) 



# Rename categories (factor levels) ---------------------------------------

cases <- cases %>% 
  
  mutate(
    
    Schweizer___Nicht_Schweizer = fct_recode(Schweizer___Nicht_Schweizer, "Swiss" = "1", "non_Swiss" = "2"),
    
    sex = fct_recode(sex, "male" = "1", "female" = "2"),
    
    admission_pre = fct_recode(admission_pre,
                               "Zuhause" = "1",
                               "Zuhause_SPITEX" = "2",
                               "Kranken_Pflege_heim" = "3",
                               "Altersheim" = "4",
                               "Psychiatrie" = "5",
                               "anderes Spital" = "6",
                               "Strafvollzugsanstalt" = "7",
                               "andere" = "8",
                               "unbekannt" = "9"),
    
    admission_mode = fct_recode(admission_mode,
                                "Notfall" = "1",
                                "geplant" = "2",
                                "Geburt" = "3",
                                "Uebertritt_intern" = "4",
                                "Verlegung_24h" = "5",
                                "andere" = "8",
                                "unbekannt" = "9"),
    
    admission_aut = fct_recode(admission_aut,
                               "selbst" = "1",
                               "Rettungsdienst" = "2",
                               "Arzt" = "3",
                               "Therapeut" = "4",
                               "sozialmed_Dienst" = "5",
                               "Justiz" = "6",
                               "andere" = "8",
                               "unbekannt" = "9"),
    
    cost_class = fct_recode(cost_class,
                            "allgemein" = "1",
                            "halbprivat" = "2",
                            "privat" = "3",
                            "unbekannt" = "9"),
    
    cost_site = fct_recode(cost_site,
                           "Allgemein" = "M000",
                           "Intensivmedizin" = "M050",
                           "Innere" = "M100",
                           "Chirurgie" = "M200",
                           "Gyn" = "M300",
                           "Paed" = "M400",
                           "Psych" = "M500",
                           "Ophthal" = "M600",
                           "OtoRhiLar" = "M700",
                           "DermaVaso" = "M800",
                           "Radio" = "M850",
                           "Geriat" = "M900",
                           "Physik_Reha" = "M950",
                           "Andere" = "M990"),
    
    cost_site2 = fct_recode(cost_site2,
                            "KK" = "1",
                            "IV" = "2",
                            "Mili" = "3",
                            "Unfall" = "4",
                            "Selbstzahl" = "5",
                            "andere" = "8",
                            "unbekannt" = "9"),
    
    discharge_dec = fct_recode(discharge_dec,
                               "Spital" = "1",
                               "Patient" = "2",
                               "DrittPerson" = "3",
                               "IntUebertr" = "4",
                               "Tod" = "5",
                               "anderes" = "8",
                               "unbekannt" = "9"),
    
    discharge_place = fct_recode(discharge_place,
                                 "Zuhause" = "1",
                                 "Kranken_Pflege_heim" = "2",
                                 "Altersheim" = "3",
                                 "Psychiatrie" = "4",
                                 "Rehaklin" = "5",
                                 "anderes Spital" = "6",
                                 "Strafvollzugsanstalt" = "7",
                                 "andere" = "8",
                                 "unbekannt" = "9",
                                 "Todesfall" = "0"), 
    
    discharge_post_treat = fct_recode(discharge_post_treat,
                                      "geheilt" = "1",
                                      "Behandl_ambul" = "2",
                                      "Spitex" = "3",
                                      "Behandl_stat" = "4",
                                      "Rehab_amb_stat" = "5",
                                      "anderes" = "8",
                                      "unbekannt" = "9",
                                      "Todesfall" = "0"), 
    
    type = fct_recode(type,
                      "Zentrumsvers_Univ" = "K111",
                      "Zentrumsversorgung" = "K112",
                      "Grundversorgung_Niv_3" = "K121",
                      "Grundversorgung_Niv_4" = "K122",
                      "Grundversorgung_Niv_5" = "K123",
                      "Psych_Niv_1" = "K211",
                      "Psych_Niv_2" = "K212",
                      "Rehab_klinik" = "K221",
                      "Spez_klinik_Chirurg" = "K231",
                      "Spez_klinik_Paed" = "K233",
                      "Spez_klinik_Geri" = "K234",
                      "Spez_klinik_Div" = "K235"),
    
    clinic = fct_recode(clinic,
                        "SuvaCare" = "Clinique romande de réadaptation (SuvaCare) Sion",
                        "RehaB" = "REHAB Basel",
                        "SPZ" = "Schweizer Paraplegiker-Zentrum Nottwil"))



save(cases, file = file.path(".", "workspace", "cases_releveled.RData"))

rm(to_factor, cases)
