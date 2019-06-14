####################################
##
##  Create patient pathes
##
####################################

library(tidyverse)
pblapply <- pbapply::pblapply

load(file = file.path("workspace", "internal_transfers_merged.RData"))


# Create variable with abbreviation ---------------------------------------

cases <- cases %>% 
  
  mutate(type_abbr = case_when(
    
    type_2 == "center_SCI" ~ "SCI",
    type_2 == "Zentrumsvers_Univ" ~ "U",
    type_2 == "Rehab_klinik" ~ "R",
    type_2 == "Zentrumsversorgung" ~ "T",
    type_2 %in% c("Spez_klinik_Div", "Spez_klinik_Paed", "Spez_klinik_Geri", "Spez_klinik_Chirurg") ~ "S",
    type_2 %in% c("Psych_Niv_1", "Psych_Niv_2") ~ "P",
    type_2 == "Grundversorgung_Niv_3" ~ "B3",
    type_2 == "Grundversorgung_Niv_4" ~ "B4",
    type_2 == "Grundversorgung_Niv_5" ~ "B5"))

table(cases$type_abbr, useNA = "always")

# Versorgungsniveau 1 (K111): Zentrumsversorgung	5 Unispit?ler
# Versorgungsniveau 2 (K112): Zentrumsversorgung	Betriebe mit 9000 - 30000 station?ren F?llen
# Versorgungsniveau 3 (K121): Grundversorgung	Betriebe mit 6000 - 9000 station?ren F?llen
# Versorgungsniveau 4 (K122): Grundversorgung	Betriebe mit 3000 - 6000 station?ren F?llen
# Versorgungsniveau 5 (K123): Grundversorgung	Betriebe mit 0 - 3000 station?ren F?llen
# Spezialklinik: Chirurgie (K231)	
# Spezialklinik: Gyn?kologie / Neonatologie (K232)	
# Spezialklinik: P?diatrie (K233)	
# Spezialklinik: Geriatrie (K234)	
# Spezialklinik: Diverse Spezialkliniken (K235)


# Draw patient pathes -----------------------------------------------------

# Kleine Buchstaben bedeuten, dass diese Hospitalisierungen eigentlich keinen SCI code hatten aber dass es sich dabei
# um Hospitalisierungen handelt, welche direkt vor einer akuten SCI Hospitalisierung geschehen sind.


draw_pat_path <- function(x) {
  
  filter(cases, pat_id == x) %>% 
    mutate(type_abbr = if_else(acute_new %in% "hosp_b4_acute", tolower(type_abbr), type_abbr)) %>% 
    pull(type_abbr) %>% 
    paste(collapse = "-") %>% 
    enframe(value = "pat_path") %>% 
    mutate(name = x) %>% 
    rename(pat_id = name)
  
}


# Adding individual pathways to dataset

cases <- pblapply(unique(cases$pat_id), draw_pat_path) %>% 
  bind_rows() %>% 
  full_join(cases, by = "pat_id") %>% 
  select(-pat_path, everything())


# Get a table with frequency of patient pathes of all patients
  
pat_path_freq_table <- cases %>% 
  group_by(pat_id) %>% 
  distinct(pat_path) %>%
  ungroup() %>% 
  group_by(pat_path) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

pat_path_freq_table_TSCI <- cases %>% 
  filter(cause == "traumatic") %>% 
  group_by(pat_id) %>% 
  distinct(pat_path) %>%
  ungroup() %>% 
  group_by(pat_path) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

pat_path_freq_table_NTSCI <- cases %>% 
  filter(cause == "nontraumatic") %>% 
  group_by(pat_id) %>% 
  distinct(pat_path) %>%
  ungroup() %>% 
  group_by(pat_path) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))


pat_all_diag_chapter <- cases %>% 
  mutate(pd_title_chapter = unlist(lapply(pd_title_chapter, "[[", 1))) %>% 
  group_by(pd_title_chapter) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

pat_all_diag_groups <- cases %>% 
  mutate(pd_title_group = unlist(lapply(pd_title_group, "[[", 1))) %>% 
  group_by(pd_title_group) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

pat_all_diag_groups_NTSCI <- cases %>% 
  filter(cause == "nontraumatic") %>% 
  mutate(pd_title_group = unlist(lapply(pd_title_group, "[[", 1))) %>% 
  group_by(pd_title_group) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

pat_all_diag_groups_TSCI <- cases %>% 
  filter(cause == "traumatic") %>% 
  mutate(pd_title_group = unlist(lapply(pd_title_group, "[[", 1))) %>% 
  group_by(pd_title_group) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))


# Test pathways

sample_pathway <- filter(cases, pat_path == "t-R")



# Save pathways -----------------------------------------------------------

write_delim(pat_path_freq_table, delim = ";", file.path("output", "pat_path_freq_table.csv"))

save(cases, file = file.path("workspace", "cases_pathways.RData"))
save(pat_path_freq_table, file = file.path("workspace", "pat_path_freq_table.RData"))

rm("cases", "draw_pat_path", "pat_all_diag_chapter", "pat_all_diag_groups", 
     "pat_all_diag_groups_NTSCI", "pat_all_diag_groups_TSCI", "pat_path_freq_table", 
     "pat_path_freq_table_NTSCI", "pat_path_freq_table_TSCI", "pblapply", 
     "sample_pathway")

