##############################
##
##  Impute lesion level
##
##############################

library(tidyverse)
library(zoo)
library(pbapply)

rbindlist <- data.table::rbindlist

load(file = file.path("workspace", "spec_cent_identified.RData"))



########################################################################################################
##
## Define evidence level characteristic coding. The highest evidence will be used to determine the 
##
## lesion characteristics in case there are different lesion characteristics in a single patient
##
########################################################################################################

cases <-  mutate(cases, 
                 
                 type_evidence = case_when(
                   
                   type_2 == "center_SCI" ~ 1,
                   type_2 == "Zentrumsvers_Univ" ~ 2,
                   type_2 == "Spez_klinik_Chirurg" ~ 3,
                   type_2 == "Spez_klinik_Div" ~ 4,
                   type_2 == "Spez_klinik_Paed" ~ 5,
                   type_2 == "Spez_klinik_Geri" ~ 6,
                   type_2 == "Zentrumsversorgung" ~ 7,
                   type_2 == "Rehab_klinik" ~ 8,
                   type_2 == "Grundversorgung_Niv_3" ~ 9,
                   type_2 == "Grundversorgung_Niv_4" ~ 10,
                   type_2 == "Grundversorgung_Niv_5" ~ 11,
                   type_2 == "Psych_Niv_1" ~ 12,
                   type_2 == "Psych_Niv_2" ~ 13,
                   TRUE ~ 14
                   
                 )
                 
)



# Impute lesion levels where there is only one lesion level per person --------


# Keep original coding of lesion characteristics

cases <- cases %>% 
  
  mutate(
    
    lesion_level_orig = lesion_level,
    lesion_level_d_orig = lesion_level_d,
    completeness_orig = completeness,
    cause_orig = cause
    
  )

# Find persons with different lesion levels in different hospitalizations

cases <- cases %>% 
  
  group_by(pat_id) %>% 
  
  mutate(
    
    distinct_ll = n_distinct(lesion_level, na.rm = TRUE),
    distinct_ll_d = n_distinct(lesion_level_d, na.rm = TRUE),
    distinct_compl = n_distinct(completeness, na.rm = TRUE),
    distinct_cause = n_distinct(cause, na.rm = TRUE)
    
  ) 


# Get distinct codings per person by lesion characteristics

check_diff_in_coding <- function(df) {
  
  df %>% 
    distinct(pat_id, .keep_all = TRUE) %>% 
    ungroup() %>% 
    select(starts_with("distinct_")) %>% 
    lapply(table) %>% 
    lapply(as.data.frame, stringsAsFactors = FALSE) %>% 
    bind_rows(.id = "test_singularity") %>% 
    rename(diff_les_char_by_pat = Var1, n = Freq)
  
}

check_diff_in_coding(cases)


# Impute lesion characteristics in those that have no conflicting information

cases <- cases %>% 
  
  mutate(
    
    lesion_level = if_else(distinct_ll == 1, zoo::na.locf0(lesion_level, fromLast = TRUE), lesion_level),
    lesion_level = if_else(distinct_ll == 1, zoo::na.locf0(lesion_level, fromLast = FALSE), lesion_level),
    
    lesion_level_d = if_else(distinct_ll_d == 1, zoo::na.locf0(lesion_level_d, fromLast = TRUE), lesion_level_d),
    lesion_level_d = if_else(distinct_ll_d == 1, zoo::na.locf0(lesion_level_d, fromLast = FALSE), lesion_level_d),
    
    completeness = if_else(distinct_compl == 1, zoo::na.locf0(completeness, fromLast = TRUE), completeness),
    completeness = if_else(distinct_compl == 1, zoo::na.locf0(completeness, fromLast = FALSE), completeness),
    
    cause = if_else(distinct_cause == 1, zoo::na.locf0(cause, fromLast = TRUE), cause),
    cause = if_else(distinct_cause == 1, zoo::na.locf0(cause, fromLast = FALSE), cause)
    
  ) %>% 
  
  ungroup





###################################################################################################################
##
## Impute lesion characteristics in patients with different codings of lesion characteristics
## in different hospitalizations.
##
## Lesion characterstic of the most recent hospitalization from the hospital with the highest evidence level 
## will be used.
##
###################################################################################################################

harmonize_les_char <- function(df, les_char) {
  
  dupli_var <- case_when(
    
    les_char == "lesion_level" ~ "distinct_ll",
    les_char == "lesion_level_d" ~ "distinct_ll_d",
    les_char == "completeness" ~ "distinct_compl",
    les_char == "cause" ~ "distinct_cause"
    
  )
  
  if(all(is.na(df[[les_char]])) | all(df[[dupli_var]] == 1)) {
    
    # If there is no lesion characteristics coding at all or
    # if there are no discrepancies in the coding of lesion characteristics
    # return original data
    
    return(df)
    
  } else {
    
    most_plausible_les_char <- df %>%
      
      # Remove row with missing information about lesion characteristic
      drop_na(les_char) %>% 
      
      # Sort columns so that the last observations is the one with the highest evidence level (= lowest number)
      # Within the evidence level sort that the most recent hosptitalization comes last
      arrange(desc(type_evidence), seq_no_corr) %>% 
      
      # Of the hospital with the hightest evidence level, tahke the most recent lesion level coding
      pull(les_char) %>% last()
    
    # Assign this lesion characterstic to all lesion characterstics
    
    mutate(df, !!les_char := most_plausible_les_char)

  }
  
}


# Apply function to correct lesion characteristics

cases_l <- cases %>% split(.$pat_id) %>% 
  
  pblapply(harmonize_les_char, les_char = "lesion_level") %>% 
  pblapply(harmonize_les_char, les_char = "lesion_level_d") %>% 
  pblapply(harmonize_les_char, les_char = "completeness") %>% 
  pblapply(harmonize_les_char, les_char = "cause")
  
  cases <- as_tibble(data.table::rbindlist(cases_l)) %>% 
  
    arrange(pat_id, seq_no_corr)


# Get proportion of lesion characteristics before and after the imputation/ correction --------

calc_prop_coding <- function(x) {
  
  summarize(cases, 
            
            les_char = x,
            
            n_total = n(),
            
            n_miss_orig = sum(is.na(!! sym(paste(x, "orig", sep = "_")))),
            
            n_miss_imputed = sum(is.na(!! sym(x))),
            
            prop_n_miss_orig = round(100 * n_miss_orig / n_total),
            
            prop_n_miss_imputed = round(100 * n_miss_imputed / n_total)
            
  )
  
}

bind_rows(lapply(c("lesion_level", "lesion_level_d", "completeness", "cause"), calc_prop_coding))


# Proportion of coding of lesion characteristics by hospital type ----------

calc_prop_coding <- function(x) { 
  
  cases %>% 
    
    filter(any_SCI_code == 1) %>% 
    
    group_by(type_2) %>% 
    
    summarize(
      
      n = n(),
      missing_codes = sum(is.na(!! sym(x))),
      prop_coded = 100 - round(100 * (missing_codes / n), 0),
      prop_hosp = round(100 * (n / sum(cases$any_SCI_code)), 0)
      
    ) %>% 
    
    arrange(desc(prop_coded)) %>% 
    
    select(type_2, prop_coded, prop_hosp) %>% 
    
    rename(!! paste("prop_coded", x, sep = "_") := prop_coded)
  
}

calc_prop_coding("lesion_level_d_orig")
calc_prop_coding("completeness_orig")


# Proportion of coding non-traumatic by hospital type

cases %>% 
  
  group_by(type_2) %>% 
  
  summarize(
    
    n = n(),
    n_nt = sum(acute_nt),
    prop_nt = round(100 * (n_nt / n), 0),
    prop_hosp = round(100 * (n / sum(cases$any_SCI_code)), 0)
    
    ) %>% 
  
  arrange(desc(prop_nt)) %>% 
  
  select(type_2, prop_nt, prop_hosp)


# Check whether there are still different lesion characterstics in one patient--------

cases %>% 
  
  group_by(pat_id) %>% 
  
  mutate(
    
    distinct_ll = n_distinct(lesion_level, na.rm = TRUE),
    distinct_ll_d = n_distinct(lesion_level_d, na.rm = TRUE),
    distinct_compl = n_distinct(completeness, na.rm = TRUE),
    distinct_cause = n_distinct(cause, na.rm = TRUE)
    
  ) %>% 
  
  check_diff_in_coding


# Remove variables - Save dataset - Clear workspace -----------------------------------

cases <- select(cases, -starts_with("distinct_"))

save(cases, file = file.path("workspace", "les_char_imp.RData"))

rm("calc_prop_coding", "cases", "cases_l", "check_diff_in_coding", 
  "harmonize_les_char", "rbindlist")


