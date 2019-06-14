
##############################################################################################
##
## Diagnostics - Calculate a score that gives an idea about the quality of SCI coding
##
##############################################################################################

library(tidyverse)

load(file.path(".", "workspace", "vars_reduced.RData"))

load(file.path(".", "workspace", "vars_side_diagnoses.RData"))


# Get principle and side diagnosis of patients

diagnoses <- cases %>% 
  select(id, pd, pd_add) %>% 
  left_join(vars_side_diagnoses, by = "id")

rm(vars_side_diagnoses)



# Add a new column with all ICD-10 SCI codes of a hospitalization --------------

ICD_10_SCI <- c("G82", "S14", "S24", "S34", "G834")
ICD_10_SCI <- paste(ICD_10_SCI, collapse = "|")

get_SCI_codes <- function(x) { unlist(str_subset(x, pattern = ICD_10_SCI)) %>% .[!is.na(.)] }

cases <- diagnoses %>% 
  split(., .$id) %>% 
  lapply(., get_SCI_codes) %>% 
  tibble(id = names(.)) %>% 
  rename(SCI_codes_all = ".") %>% 
  right_join(cases, by = "id") %>% 
  select(-SCI_codes_all, everything())


# Create a score that is based on how detailed the SCI is coded using ICD-10 codes


# Identify detailed lesion level code of every hospitalization

ICD_10_SCI_dt_ll <- c("G826", "S147", "S247", "S347")
ICD_10_SCI_dt_ll <- paste(ICD_10_SCI_dt_ll, collapse = "|")

find_SCI_codes <- function(x) str_subset(x, ICD_10_SCI_dt_ll)

score_dt_ll <- cases %>% 
  select(id, pat_id, SCI_codes_all) %>% 
  mutate(SCI_codes_all = lapply(SCI_codes_all, unique)) %>% 
  mutate(SCI_codes_dt_ll = lapply(SCI_codes_all, find_SCI_codes)) %>% 
  mutate(dt_ll_digits = if_else(unlist(lapply(SCI_codes_dt_ll, length)) > 0, 5, 0))

rm(find_SCI_codes)


# Function removes detailed lesion lesion level ICD-codes
remove_dt <- function(x) str_subset(x, ICD_10_SCI_dt_ll, negate = TRUE)

# Calculates the length of the ICD-10 codes
get_str_length <- function(x) str_length(x)

# Gets length of ICD-10 code that provides the most detailed information about the lesion

get_longest_string <- function(x) ifelse(purrr::is_empty(x), 0, max(x))

score_main_code <- score_dt_ll %>% 
  select(-SCI_codes_dt_ll, -dt_ll_digits) %>% 
  mutate(SCI_codes_no_dt = lapply(SCI_codes_all, remove_dt)) %>% 
  mutate(length_SCI_codes_no_dt = lapply(SCI_codes_no_dt, get_str_length)) %>% 
  mutate(all_codes_digits = unlist(lapply(length_SCI_codes_no_dt, get_longest_string)))

rm(remove_dt, get_str_length, get_longest_string)



# Final score: How much detail was coded about the SCI of each patient --------

# Average number of SCI coding letters per hospitalization per patient 

# Values for a single hospitalization
# 0: 0 no SCI code
# 3: G82 or S14 -> Absolute minimum to be counted as SCI code
# 4: G822 -> Lesion level defined but not acute/chronic and incomplete/complete
# 5: G8220 -> Acute complete paraplegia

# 10 -> G8220 & G8261 (C4-C5) -> Aditionally there is another 5 letter code for the detailed lesion level

SCI_coding_cred_score <- score_main_code %>% 
  select(id, pat_id, all_codes_digits) %>% 
  full_join(score_dt_ll %>% select(id, dt_ll_digits), by = "id") %>% 
  mutate(hosp_score = all_codes_digits + dt_ll_digits) %>% 
  group_by(pat_id) %>% 
  summarize(SCI_coding_cred_score = sum(hosp_score) / n())


# Half of the the patient have an SCI diagnosis for every hospitalization

ggplot(SCI_coding_cred_score, aes(x = SCI_coding_cred_score)) + geom_freqpoly(bins = 30) + theme_classic()

summary(SCI_coding_cred_score$SCI_coding_cred_score)



# Merge score into original dataframe --------------------------------------------------------

cases <- cases %>% 
  left_join(select(SCI_coding_cred_score, pat_id, SCI_coding_cred_score), by = "pat_id")


# Check average score by hospital type

cases %>% 
  group_by(type_2) %>% 
  summarize(n = n(),
            avg_score_hosp_type = mean(SCI_coding_cred_score)) %>% 
  arrange(desc(avg_score_hosp_type))


## Get the bad and the good

the_bad <- cases %>% 
  filter(SCI_coding_cred_score < 4) %>% 
  select(pat_id, seq_no_corr, age, sex, type_2, cause, completeness, pd_title_group, SCI_coding_cred_score, SCI_codes_all)

the_good <- cases %>% 
  filter(SCI_coding_cred_score > 8) %>% 
  select(pat_id, seq_no_corr, age, sex, type_2, cause, completeness, pd_title_group, SCI_coding_cred_score, SCI_codes_all)



# Save dataset and clear workspace

save(cases, file = file.path(".", "workspace", "coding_cred_score_added.RData"))

rm("cases", "diagnoses", "get_SCI_codes", "ICD_10_SCI", "ICD_10_SCI_dt_ll", 
  "SCI_coding_cred_score", "score_dt_ll", "score_main_code", "the_bad", 
  "the_good")
