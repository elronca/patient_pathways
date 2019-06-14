
#############################################################
##
##  Make data.frame a tibble and rename variables
##
#############################################################

library(tidyverse)


load(file = file.path(".", "workspace", "data_SCI_raw.RData"))

cases <- as_tibble(cases)



# Rename variable names according to the list provided by the BfS ---------

codebook <- read_csv2(
  
  file = file.path(".", "data", "MetaMedGeoTypoDRG_Gemp.csv"),
  
  col_types = cols(
    Variable = col_character(),
    Bezeichnung = col_character(),
    Description = col_character(),
    Variable_sas = col_character(),
    `Bemerkungen/Remarques` = col_character(),
    `geliefert (1=ja,0= nein)` = col_double())
  
  ) %>%
  
  select(var_names = Bezeichnung, Variable_sas) %>%
  
  mutate(
    
    Variable_sas = str_replace(Variable_sas, pattern = "_", replacement = "X_"),
    var_names = str_replace_all(var_names, pattern = " ", replacement = "_")
    
  )

# Find rows in Variable_sas of codebook that match the variable names of my cases dataset.

position_matching_var_titles <- match(colnames(cases), codebook$Variable_sas)

colnames(cases) <- codebook[position_matching_var_titles, ]$var_names


# Clean variable names ----------------------------------------------------

cases <- rename_all(cases, str_replace_all, c("\\(|\\)" = "", "-" = "_", "&" = "und", "<|/" = ""))


# Manually rename variables -----------------------------------------------

# General information

cases <- rename(cases,
                id = "Anonyme_Fallnummer",
                pat_id = "Anonyme_Patientennummer",
                type = "Typologie",
                S_DRG = "SDRG",
                BUR = "Anonyme_Betriebsnummer",
                age = "Alter_bei_Eintritt",
                sex = "Geschlecht",
                admission_pre = "Aufenthaltsort_vor_dem_Eintritt",
                admission_mode = "Eintrittsart",
                admission_aut = "Einweisende_Instanz",
                treat_mode = "Behandlungsart",
                cost_class = "Klasse",
                cost_site = "Hauptkostenstelle",
                cost_site2 = "Hauptkostenträger_für_Grundversicherungsleistungen",
                discharge_dec = "Entscheid_für_Austritt",
                discharge_place = "Aufenthalt_nach_Austritt",
                discharge_post_treat = "Behandlung_nach_Austritt",
                clinic = "Name_der_Klinik_nur_mit_Einverständniserklärung")


# Principle (Haupt-) and side diagnoses (Nebendiagnosen)

cases <- cases %>%
  rename(pd = "MD_Hauptdiagnose", pd_add = "MD_Zusatz_zu_Hauptdiagnose") %>%
  rename_at(vars(starts_with("MD")), ~str_replace_all(., c(".Nebendiagnose" = "", "MD" = "sd")))


# Time/sequence related variables

cases <- rename(cases,
                seq_no = "Sequenznummer", # The chronological order of the hospitalizations.
                dis_year = "Jahr", # Discharge year, actually record year but not used as that in the study.
                adm_month = "Eintrittsmonat",
                LOS_OECD = "Aufenthaltsdauer_OECD", # SwissDRG LOS + 1
                LOS_SDRG = "Aufenthaltsdauer_SwissDRG",
                d_p_treat = "Beginn_der_MD_Hauptbehandlung_JJJJMMTTSS",
                d_adm_to_p_treat = "Zeitdauer_zwischen_Eintritt_und_Hauptbehandlung",
                d_next_hosp = "Tage_bis_zur_nächsten_Hospitalisierung",
                stat_type = "Kennzeichnung_des_Statistikfalls")


# Save dataset - Clear workspace -----------------------------------

save(cases, file = file.path(".", "workspace", "vars_renamed.RData"))

rm("cases", "codebook", "position_matching_var_titles")
