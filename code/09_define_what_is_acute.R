####################################################################
##
##  Reduce dataset to acute SCI episode
##
####################################################################


library(tidyverse)
library(zoo)
library(pbapply)

rbindlist <- data.table::rbindlist


load(file = file.path(".", "workspace", "les_char_imp.RData"))

load(file = file.path(".", "workspace", "vars_adm_char.RData"))

load(file = file.path(".", "workspace", "vars_date_time.RData"))


# Merge additional data into dataset --------------------------------------

vars_date_time <- select(vars_date_time, id, d_next_hosp)

vars_adm_char <- select(vars_adm_char, id, admission_pre, admission_mode, discharge_post_treat, discharge_place)

cases <- cases %>% 
  left_join(vars_date_time, by = "id") %>% 
  left_join(vars_adm_char, by = "id")

rm(vars_adm_char, vars_date_time)

(hosp_counter <- cases %>% summarize(n_all = n()))



# Remove patients without acute SCI codes ------------------

pat_ids_acute_codes <- filter(cases, acute_chron == "acute") %>% pull(pat_id) %>% unique()

cases <- filter(cases, pat_id %in% pat_ids_acute_codes); rm(pat_ids_acute_codes)

(hosp_counter <- cases %>% summarize(acute = n()) %>% bind_cols(hosp_counter, .))



# Function to exclude hospitalizations before acute SCI episode -----------

# Variables to track progress

cases <-  mutate(cases, 
                 acute_new = acute_chron,
                 acute_version = NA_character_)


# Test dataset

patients <- cases %>% 
  select(pat_id, seq_no_corr, discharge_post_treat, acute_chron, acute_new, d_next_hosp) %>% 
  split(.$pat_id)

pat <- patients[[1]]


check_b4_acute <- function(pat) {

  # We check how the patient was discharged on the last hospitalization before the first acute SCI hospitalization
  # Possible codes are: Todesfall, geheilt, Behandl_ambul, Spitex, anderes, unbekannt 
  # as well as Behandl_stat and Rehab_amb_stat
  
  # We are only interested in the last two since we want to know whether the hospitalizations right before the hospitalization
  # coded as acute SCI might be part of the aucte SCI episode.
  
  seq_no_1st_acute <- filter(pat, acute_new == "acute") %>% pull(seq_no_corr) %>% min() 
  
  dis_type_last_hosp_b4_1st_acute <- filter(pat, seq_no_corr < seq_no_1st_acute) %>% 
    filter(row_number() == n()) %>%  
    pull(discharge_post_treat) %>% 
    as.character
  
    # If there is no hospitaliztion before the first acute hospitalization then we return the original hospitalizations of the patient
  
  # Example patient: 12048, 24905
  
  
  if(length(dis_type_last_hosp_b4_1st_acute) == 0) {
    
    out <- pat
    
    out$acute_version <- "V1"
    
    return(out)
    
    
    
    # If there qwew hospitalizions before the first acute hospitalizaiton but the patient was not discharged to another
    # inpatient stay right before the first acute SCI hospitalization, then we expect that thi hospitalizations 
    # qwew not related to the acute SCI episode.
    # In this case we remove all hospitalizations before the first "acute SCI" hospitalization, since we assume
    # that in case these hospitalizations were associated with the acute SCI episode, 
    # the patient would be directly transfered to another hospital.
    
    # Patient example: 25809
    
  } else if (!dis_type_last_hosp_b4_1st_acute %in% c("Behandl_stat", "Rehab_amb_stat")) {
    
    out <- filter(pat, seq_no_corr >= seq_no_1st_acute)
    
    out$acute_version <- "V2"
    
    return(out)
    
    
    
    # If all hospitalization before 1st acute SCI hopsitalization were discharges to new inpatient hospitalizations then
    # we expect that they were related to the acute SCI episode. 
    # To highlight that we added this hospitalizations manually we mark them as "hosp_b4_acute".
    
    # Example patients: 1429, 3215
    
  } else {
    
    hosp_b4_acute <- filter(pat, seq_no_corr < seq_no_1st_acute)
    
    if(all(as.character(hosp_b4_acute$discharge_post_treat) %in% c("Behandl_stat", "Rehab_amb_stat"))) {
      
      out <- mutate(pat, acute_new = if_else(seq_no_corr %in% hosp_b4_acute$seq_no_corr, "hosp_b4_acute", acute_new))
      
      out$acute_version <- "V3"
      
      return(out)
      
      
      
      # There might be the case that are multiple inaptient transfers before the first aucte SCI hospitalizations but that
      # not all these hospitalizations ended with a discharge to another hospital. Therefore it is our aim now to identify
      # all hosptlizations right before the acute SCI episode where the patient was always discharged to another hospital.
      
      # To do that we search for the last hospitalization before the first hospitalization of the acute SCI episode where the
      # discharge was not to another hospital for an inpatient stay. We will then remove this hospitalization and all the
      # hospitalizations that happened before.
      
      # Example patients: 30774
      
    } else {
      
      
      # Find last non-inpatient discharge before aucte SCI episode and get sequence number 
      
      seq_no_last_discharge_non_inpatient <- filter(pat, seq_no_corr < seq_no_1st_acute) %>% 
        
        mutate(discharge_post_treat = if_else(discharge_post_treat %in% c("Behandl_stat", "Rehab_amb_stat"), NA_character_, as.character(discharge_post_treat))) %>% 
        
        drop_na(discharge_post_treat) %>% 
        
        pull(seq_no_corr) %>% 
        
        max() 
      
      
      # Find sequnce numbers of hospitalizations that happened after last non-inpatient discharge and before acute SCI episode 
      
      seq_no_pre_acute_SCI_hosp <- filter(pat, seq_no_corr < seq_no_1st_acute) %>% pull(seq_no_corr) %>% .[. > seq_no_last_discharge_non_inpatient]
      
      
      # Mark this hospitalizaitations as hosp_b4_acute
      
      out <- mutate(pat, acute_new = if_else(seq_no_corr %in% seq_no_pre_acute_SCI_hosp, "hosp_b4_acute", acute_new))
      
      
      # Remove all hospitalizations before hosp_b4_acute hospitalizations
      
      out <- filter(out, seq_no_corr > seq_no_last_discharge_non_inpatient)
      
      out$acute_version <- "V4"
      
      return(out)
      
    }
    
  }
  
}


cases_l <- split(cases, f = cases$pat_id)

cases_ll <- pblapply(cases_l, check_b4_acute)

cases <- as_tibble(data.table::rbindlist(cases_ll))


# Analytics

cases %>% distinct(pat_id, acute_version) %>% pull(acute_version) %>% table(useNA = "always")

# V1: The hospitalization with the acute SCI code is the first hospitalization of that patient in the dataset.

# V2: There are hospitalizations before the first acute hospitalization but they are rather not connected since the last.
#     discharge before the SCI code was not to another inpatient stay.

# V3: All hospitalizations before the acute SCI episode ended with discharges to other hospitals. Therefore they are now part of the acute SCI episode.

# V4: There were multiple hospitalizations before the acute SCI episode. We only keep those ones before the acute SCI episode where the
#     patient was transfered for inpatient stays from hospitals to hospitals.

(hosp_counter <- cases %>% summarize(hosp_excluded_b4_acute_SCI = n()) %>% bind_cols(hosp_counter, .))


# Remove hosp_b4_acute when there are more than 10 days until the hosp 

cases <- filter(cases, !(acute_new == "hosp_b4_acute" & d_next_hosp > 10 & !is.na(acute_new) & !is.na(d_next_hosp)))

(hosp_counter <- cases %>% summarize(hosp_excluded_b4_acute_SCI_10d = n()) %>% bind_cols(hosp_counter, .))


# Clear workspace of unused objects and dataset of unused variables

rm(cases_l, cases_ll)

cases <- select(cases, -acute_version)



# Recode hospitalizations in between acute SCI hospitalizations to be acute SCI hospitalizaitons--------

cases <- cases %>% 
  
  mutate(acute_temp = if_else(acute_chron == "acute", "acute", NA_character_)) %>% 
  
  group_by(pat_id) %>% 
  
  mutate(
    
    acute_temp = zoo::na.locf0(acute_temp, fromLast = TRUE),
    acute_temp = if_else(acute_new %in% "hosp_b4_acute", "hosp_b4_acute", acute_temp)) %>% 
  
  ungroup() %>% 
  
  mutate(acute_new = acute_temp) %>% 
  select(- acute_temp)



# Remove hospitalizations after acute SCI episode -------------------------

# Remove hospitalizations directly after last acute SCI hospitalizations if
# discharge place of the acute hospitalization was not an inpatient stay or
# when it took longer than 10 to the next hospitalization


# Example data

patients <- cases %>% 
  select(pat_id, seq_no_corr, discharge_post_treat, acute_chron, acute_new, d_next_hosp) %>% 
  split(.$pat_id)

(pat <- patients[[8]])

(pat <- patients[[19]])



remove_hosp_directly_after_last_acute <- function(pat) {
  
  seq_no_last_acute_be4_10d_gap <- pat %>% 
    
    # Last acute stay
    
    filter(acute_new == "acute") %>% filter(row_number() == n()) %>% 
    
    # Check whether there were more than 10 days until the next hospitalization or the bext hospitalization was not inpatient
    
    filter(d_next_hosp > 10 | !discharge_post_treat %in% c("Behandl_stat", "Rehab_amb_stat")) %>% 
    
    # Get sequence number of that stay
    
    pull(seq_no_corr)
  
  
  # If there were not more than 10 days until the next hospitalization from the last acute SCI hosp and if the next hospitalization was inpatient
  # then seq_no_last_acute_be4_10d_gap will be empty
  
  if(length(seq_no_last_acute_be4_10d_gap) == 0){
    
    return(pat)
    
  } else {
    
    # Cut directly after last acute hospitalization
    
    out <- filter(pat, seq_no_corr <= seq_no_last_acute_be4_10d_gap)
    
    return(out)
    
  }
  
}


cases_l <- split(cases, f = cases$pat_id)

cases_ll <- pblapply(cases_l, remove_hosp_directly_after_last_acute)

cases <- as_tibble(data.table::rbindlist(cases_ll))

rm(cases_l, cases_ll)

(hosp_counter <- cases %>% summarize(hosp_excluded_after_acute_SCI = n()) %>% bind_cols(hosp_counter, .))



# Remove hospitalizations sometimes after acute SCI hospitalizations if next hospitalization was not inpatient or when it took longer than 10 to the next hospitalization ---

# Example data

patients <- cases %>% 
  select(pat_id, seq_no_corr, discharge_post_treat, acute_chron, acute_new, d_next_hosp) %>% 
  split(.$pat_id)

(pat <- patients[[19]])


remove_hosp_after_acute <- function(pat) {
  
  # If there is no NA (= all acute -> nothing to determine/define) then return original file
    if( !any(is.na(pat$acute_new)) ) {
    
    return(pat)
    
  } else {
    
    seq_no_first_discharge_not_inpat <- pat %>% 
      
      filter(is.na(acute_new)) %>% 
      
      filter(d_next_hosp > 10 | !discharge_post_treat %in% c("Behandl_stat", "Rehab_amb_stat")) %>% 
      
      pull(seq_no_corr) %>% first()
    
  }
  
  if(length(seq_no_first_discharge_not_inpat) == 0){
    
    return(pat)
    
  } else {
    
    return(filter(pat, seq_no_corr <= seq_no_first_discharge_not_inpat))
    
  }
  
}


cases_l <- split(cases, f = cases$pat_id)

cases_ll <- pblapply(cases_l, remove_hosp_after_acute)

cases <- as_tibble(data.table::rbindlist(cases_ll))

rm(cases_l, cases_ll)

(hosp_counter <- cases %>% summarize(hosp_excluded_sometimes_after_acute_SCI = n()) %>% bind_cols(hosp_counter, .))



# glimpse

cases_glimpse <- cases %>% 
  
  select(pat_id, seq_no_corr, age, LOS_OECD, BUR, type, clinic, 
         d_next_hosp, d_p_treat, admission_pre:discharge_place, acute_chron, acute_new, cause, 
         pd_title_group, pd_title_full) %>% 
  
  arrange(pat_id, seq_no_corr)



save(cases, file = file.path(".", "workspace", "acute_edited.RData"))

rm("cases", "cases_glimpse", "check_b4_acute", "hosp_counter",   "pat", "patients", "rbindlist", 
   "remove_hosp_after_acute", "remove_hosp_directly_after_last_acute")


