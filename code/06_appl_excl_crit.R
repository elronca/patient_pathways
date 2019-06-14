
#############################################################
##
##  Apply inclusion/Exclusion criteria
##
#############################################################

library(tidyverse)
library(pbapply)

load(file = file.path("workspace", "plausibility_checked.RData"))


# Find hospitalizations with exclusion criteria ---------------------------

SpinaBifida <- "Q05"
MS <- "G35"
ALS <- "G122"
GBS <- "G610"

disease_excl <- str_c(c(SpinaBifida, MS, ALS, GBS), collapse = "|")


find_ex_crit <- function(x, ICD_codes) any(str_detect(x, ICD_codes))

ICD_cols <- str_subset(colnames(cases), "^pd|^sd")


# Find ICD-10 codes of diseases to exclude and code them with a 1

cases <- mutate(cases, 
                ex_crit = if_else(pbapply(X = cases[ICD_cols], MARGIN = 1, FUN = find_ex_crit, disease_excl), 1, 0))



# Exclude patients with spina bifida, multiple sclerosis, ALS and Guillain-Barré-Syndrom or those
# who are younger than 16 years

pat_excluded <- cases %>% 
  filter(ex_crit == 1 | age < 16) %>% 
  distinct(pat_id) %>% 
  pull(pat_id)

nrow(cases)

cases <- filter(cases, !pat_id %in% pat_excluded)

nrow(cases)


# Remove variables - save dataset - clear workspace -----------------------------------

cases <- select(cases, -starts_with("sd_"), -ex_crit)

save(cases, file = file.path("workspace", "appliedExclCrit.RData"))

rm("ALS", "cases", "disease_excl", "find_ex_crit", "GBS", "ICD_cols", 
  "MS", "pat_excluded", "SpinaBifida")

