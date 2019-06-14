
#############################################################
##
##  Get Begleittext of ICD-10 codes
##
#############################################################


## From: https://www.dimdi.de/static/de/klassifikationen/icd/icd-10-gm/kode-suche/

library(tidyverse)
library(zoo)

load(file = file.path("workspace", "cases_sorted.RData"))


## Load text to ICD-10-GM codes

icd_10_info <- read_delim(
  
  file.path(".", "data", "ICD_10_GM_files", "x1gma2013", "Klassifikationsdateien", "icd10gm2013syst_kodes.txt"), 
  delim = ";", 
  col_names = FALSE,
  col_types = cols(.default = col_character(), X1 = col_double(), X24 = col_double())) %>% 
  select(class_lvl = X1, chapter_number = X4, ICD_10_code = X8, title_full = X9, title_lvl_3 = X10) 

chapters <- read_delim(
  file.path("data", "ICD_10_GM_files", "x1gma2013", "Klassifikationsdateien", "icd10gm2013syst_kapitel.txt"), 
  delim = ";", 
  col_names = c("chapter_number", "chapter_title"),
  col_types = cols(chapter_number = col_character(), chapter_title = col_character()))

groups <- read_delim(file.path(".", "data", "ICD_10_GM_files", "x1gma2013", "Klassifikationsdateien", "icd10gm2013syst_gruppen.txt"), 
  delim = ";", col_names = c("start_code", "end_code", "chapter_title", "group_title"),
  col_types = cols(start_code = col_character(), end_code = col_character(), chapter_title = col_character(), group_title = col_character()))


# Explanation can be found in
# C:\Users\Elias\OneDrive\01_work\02_R\01_studies\patient_pathways_git\data\ICD_10_GM_files\x1gma2013 -> icd10gm2013syst_metadaten_liesmich.txt


## Merge chapter information

icd_10_info <- left_join(icd_10_info, chapters, by = "chapter_number")


## Merge group information - This information is provided in a very annoying way


# Find the starting letter of the group ICD-10 code and the ending letter, as well as the starting number and ending number
# Example: A00-A09 -> Infektiöse Darmkrankheiten


groups <- groups %>% 
  
  mutate(
    
    start_code_digit = str_replace_all(start_code, "[[:alpha:]]", ""),
    end_code_digit = str_replace_all(end_code, "[[:alpha:]]", ""),
    start_code_letter = str_replace_all(start_code, "[[:digit:]]", ""),
    end_code_letter = str_replace_all(end_code, "[[:digit:]]", "")
    
  ) %>% 
  
  mutate_at(vars(c("start_code_digit", "end_code_digit")), as.integer) %>% 
  
  # Some codes go over multiple letters (Unfälle + Tätlicher Angriff), which makes the whole thing complicted
  
  # V01-X59  Unfälle
  # X85-Y09  Tätlicher Angriff
  
  # Those that go over multiple letters will be coded with 0
  
  mutate(unique_letters = if_else(start_code_letter == end_code_letter, 1, 0))


# Have a look at those cases

groups %>% filter(unique_letters == 0)


# Here we need to enter the values manually

manually_added <- tibble(
  
  start_code = c("V01", "W01", "X01", "X85", "Y01"), 
  end_code = c("V99", "W99", "X59", "X99", "Y09"), 
  chapter_title = c("20", "20", "20", "20", "20"),
  group_title = c("Unfälle", "Unfälle", "Unfälle", "Tätlicher Angriff", "Tätlicher Angriff"),
  start_code_digit = c(1, 1, 1, 85, 1),
  end_code_digit = c(99, 99, 59, 99, 9),
  start_code_letter = c("V", "W", "X", "X", "Y"),
  end_code_letter = c("V", "W", "X", "X", "Y"),
  unique_letters = c(1, 1, 1, 1, 1)

  )

# These ICD-10 group titles with multiple letters are removed and replaced with the manually added table

groups <- groups %>% filter(unique_letters != 0)
  
groups <- bind_rows(groups, manually_added)


# Sequences between the numbers of each code are generated such as A00-A09 and added to the dataset

codes_digit_seq <- mapply(seq, from = groups$start_code_digit, to = groups$end_code_digit)
groups <- mutate(groups, codes_digit_seq = codes_digit_seq)
  

# Sequences including the letters are generated and added to the dataset

sprintf_pattern <- str_c(groups[["start_code_letter"]], "%02d")
code_sequence <- groups[["codes_digit_seq"]]
group_codes_list <- mapply(sprintf, sprintf_pattern, code_sequence)
groups <- mutate(groups, ICD_10_code = group_codes_list)


# Now we unlist this list column that contains group codes nested in lists so that every newly generated group code gets its own row

groups <- groups %>% tidyr::unnest(ICD_10_code)


# Now we select ICD_10_codes and the group titles and merge that information into the dataset containing all icd10 information

groups <- select(groups, ICD_10_code, group_title)
icd_10_info <- left_join(icd_10_info, groups, by = "ICD_10_code")


# Missing values in group titles is now carried forward until the next group title

icd_10_info <- mutate(icd_10_info, group_title = na.locf0(group_title, fromLast = FALSE))



# Save file - clear workspace ---------------------------------------------

save(icd_10_info, file = file.path("workspace", "icd_10_codes_titles.RData"))

rm("chapters", "code_sequence", "codes_digit_seq", "group_codes_list", 
  "groups", "manually_added", "sprintf_pattern")




# Add ICD_information to PD ---------------------------------------------------

to_merge_with_pd <- icd_10_info %>% 
  
  select(ICD_10_code, 
         pd_title_full = title_full, 
         pd_title_lvl_3 = title_lvl_3, 
         pd_title_group = group_title, 
         pd_title_chapter = chapter_title)


cases <- cases %>% 
  mutate(pd_cat_text = pd) %>% 
  left_join(to_merge_with_pd, by = c("pd" = "ICD_10_code"))


# Add most frequent diagnoses that are coded as addition to the principle diagnosis ---------------------

cases <- mutate(cases, pd_add_txt = case_when(
  
  pd_add == "X599" ~ "Verletzung infolge Exposition/Unfall",
  pd_add == "G992" ~ "Myelopathie bei anderenorts klassifizierten Krankheiten",
  pd_add == "V99" ~ "Transportmittelunfall",
  pd_add == "Y849" ~ "Medizinische Massnahme, nicht näher bezeichnet",
  pd_add == "G551" ~ "Kompression von Nervenwurzeln und Nervenplexus bei Bandscheibenschäden",
  pd_add == "Y828" ~ "Sonstige und nicht näher bezeichnete medizintechnische Geräte und Produkte im Zusammenhang mit Zwischenfällen",
  pd_add == "G553" ~ "Kompression von Nervenwurzeln und Nervenplexus bei sonstigen Krankheiten der Wirbelsäule und des Rückens",
  pd_add == "Y579" ~ "Komplikation mit Arzneimittel oder Droge, nicht näher bezeichnet",
  pd_add == "D630" ~ "Anämie bei Neubildungen",
  pd_add == "J91" ~ "Pleuraerguss bei anderenorts klassifizierten Krankheiten",
  pd_add == "W499" ~ "Exposition gegenüber sonstigen oder nicht näher bezeichnete Art und Weise",
  pd_add == "G550" ~ "Kompression von Nervenwurzeln und Nervenplexus bei Neubildungen",
  pd_add == "M4954" ~ "Neuropathische Spondylopathie",
  pd_add == "M9078" ~ "Knochenfraktur bei Neubildungen",
  pd_add == "G552" ~ "Kompression von Nervenwurzeln und Nervenplexus bei Spondylose",
  pd_add == "Y69" ~ "Nicht näher bezeichnete Zwischenfälle bei chirurgischem Eingriff und medizinischer Behandlung",
  pd_add == "M4959" ~ "Wirbelkörperkompression bei anderenorts klassifizierten Krankheiten",
  
  TRUE ~ NA_character_))


save(cases, file = file.path("workspace", "ICD10_txt_added.RData"))

rm("cases", "icd_10_info", "to_merge_with_pd")

