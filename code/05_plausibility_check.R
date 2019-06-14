
#############################################################
##
##  Plausibility check
##
#############################################################

library(tidyverse)


## Load data

load(file = file.path("workspace", "cases_completeness.RData"))
load(file = file.path("workspace", "vars_date_time.RData"))



# Merge discharge year variable -----------------------------------------------------

cases <- left_join(cases, select(vars_date_time, id, adm_date), by = "id")

rm(vars_date_time)


# Check for implausible ages ---------------------------------------------------------

diff_years_dataset <- diff(range(cases$dis_year))


# Function checks for difference in years between hospitalizations

diff_max_min <- function(x) {
  
  time_range_hosp <- ifelse(length(na.omit(x)) == 0 | length(na.omit(x)) == 1, NA, diff(range(na.omit(x), na.rm = TRUE)))
  
  time_range_hosp <- round(time_range_hosp / 365, 0)
  
  return(time_range_hosp)
  
}


# Find hospitalizations of patients that differ implausible in age or years between hospitalizations

get_id_age_check <- cases %>% 
  
  select(id, pat_id, seq_no_corr, sex, age, d_p_treat) %>% 
  
  group_by(pat_id) %>% 
  
  mutate(
    
    tme_rng_hosp = diff_max_min(d_p_treat),
    
    rng_age = diff(range(age)),
    
    diff_check = abs(tme_rng_hosp - rng_age)
    
    ) %>% 
      
      filter(diff_years_dataset + 2 > 3 | diff_check > diff_years_dataset + 1) %>% 
      
      pull(pat_id) %>% 
  
  unique()


## Cases with a age difference between the hospitaliazations of more than 3 years 
## or with a difference between hospitalizations of more than 2 years will be removed

cases <- filter(cases, !pat_id %in% get_id_age_check)



# Check for implausible sexes ---------------------------------------------

get_id_sex_check <- cases %>% select(id, pat_id, age, sex, seq_no_corr, dis_year) %>% 
  group_by(pat_id) %>%
  mutate(unique_types = n_distinct(sex)) %>% 
  filter(unique_types > 1) %>% 
  pull(pat_id) %>% 
  unique()



# Remove patients with different sexes ------------------------------------

cases <- filter(cases, !pat_id %in% get_id_sex_check)


# Save dataset, remove variables and clear workspace ----------------------

cases <- select(cases, -adm_date)

save(cases, file = file.path("workspace", "plausibility_checked.RData"))

rm("cases", "diff_max_min", "diff_years_dataset", "get_id_age_check", "get_id_sex_check")

