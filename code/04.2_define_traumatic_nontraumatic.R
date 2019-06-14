#############################################################
##
##  Specifying 
##
##    - traumatic/non-traumatic cases
##
#############################################################

library(tidyverse)

load(file = file.path(".", "workspace", "cases_acute_chronic.RData"))


# Traumatic/Non-traumatic SCI ---------------------------------------------


## Categorize the hospitalization into traumatic/nontraumatic


cases <- cases %>% 
  
  mutate(
    
    cause = case_when(
      acute_nt == 1 & S_codes == 0 ~ "nontraumatic",
      acute_nt == 0 & S_codes == 1 ~ "traumatic",
      acute_nt == 1 & S_codes == 1 ~ "conflict",
      TRUE ~ NA_character_
    
      )
  
    )

table(cases$cause)



# Recode conflicts --------------------------------------------------------

# Principle diagnosis is the most important, therefore we take the pd as the reference

# Generate non-traumatic SCI ICD-10 codes

acute_nt_codes <- c("G820", "G821", "G822", "G823", "G824", "G825")
acute_nt_codes <- c(paste0(acute_nt_codes, "0"), paste0(acute_nt_codes, "1"))


# Recode conflicting cases

cases <- cases %>% 
  
  mutate(
    
    cause = if_else(cause == "conflict", 
                    
                    case_when(
                      
                      pd %in% acute_nt_codes ~ "nontraumatic",
                      grepl("^S14|^S24|^S34", pd) ~ "traumatic",
                      TRUE ~ as.character(cause)),
                    
                    cause)
    
  )
         
         


# When this did not solve the conflict, recode these cases as unknown/NA

table(cases$cause, useNA = "always")

cases <- mutate(cases, cause = if_else(cause == "conflict", NA_character_, cause))

table(cases$cause, useNA = "always")



# Remove variables - Save dataset - Clear workspace -----------------------------------

save(cases, file = file.path(".", "workspace", "cases_cause.RData"))

rm("acute_nt_codes", "cases")
