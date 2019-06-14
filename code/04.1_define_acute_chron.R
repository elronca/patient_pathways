
#############################################################
##
##  Specifying 
##
##    - acute and chronic SCI cases
##
#############################################################

library(tidyverse)
library(pbapply)

load(file = file.path(".", "workspace", "ICD10_txt_added.RData"))


# Function that determines the SCI relevance of a hospitalization 
# by highlighting relevant hosptitalizaitons with a 1 and others with a 0

# Arguments
#
# df            dataset
#
# vars_diag     pd (= principle diagnosis) and/or sd (= side diagnosis)
#
# SCI_codes     SCI-related ICD-10 codes. Hospitalizations with these codes will be highlighted



SCI_det <- function(df, vars_diag = c("pd", "sd"), SCI_codes) {
  
  vars_diag <- paste0("^", vars_diag, collapse = "|")
  
  vars_diag <- grep(vars_diag, colnames(df), value = TRUE)
  
  SCI_codes <- paste0("^", SCI_codes, collapse = "|")
  
  catch_SCI <- function(x) as.integer(any(grepl(SCI_codes, x)))
  
  pbapply(df[vars_diag], 1, catch_SCI)
  
}



# Acute traumatic SCI -----------------------------------------------------

cases$S_codes <- SCI_det(df = cases, SCI_codes = c("S14", "S24", "S34"))



# Acute nontraumtic SCI ---------------------------------------------------

acute_nt_codes <- c("G820", "G821", "G822", "G823", "G824", "G825")

acute_nt_codes <- c(paste0(acute_nt_codes,"0"), paste0(acute_nt_codes, "1"))

cases$acute_nt <- SCI_det(df = cases, SCI_codes = acute_nt_codes)



# Chronic cases -----------------------------------------------------------

cases$any_G8x_SCI_code <- SCI_det(df = cases, SCI_codes = c("G82", "G834"))



# Create variable: Acute/Chronic case -------------------------------------


# Cases with S codes alone will be coded as acute
# Cases with nt codes alone will be coded as acute
# Cases with S and nt codes will be coded as S_nt_code conflict and as acute
# Cases with acute codes and chronic codes will be coded as acute
# Cases with no SCI related codes will be coded as NA


cases <- cases %>% 
  
  mutate(
    
    any_SCI_code = if_else(S_codes + acute_nt + any_G8x_SCI_code != 0, 1, 0),
    S_code_alone = if_else(S_codes == 1 & any_G8x_SCI_code == 0, 1, 0),
    nt_code_alone = if_else(acute_nt == 1 & S_codes == 0 & any_G8x_SCI_code == 1, 1, 0),
    S_nt_code_conflict = if_else(acute_nt == 1 & S_codes == 1, 1, 0)
    
  ) %>% 
  
  mutate(
    
    acute_chron = case_when(
      
      S_code_alone == 1 | nt_code_alone == 1 | S_nt_code_conflict == 1 ~ "acute",
      any_G8x_SCI_code == 1 & (S_code_alone == 0 | nt_code_alone == 0 | S_nt_code_conflict == 0) ~ "chronic",
      any_SCI_code == 0 ~ NA_character_
      
    )
    
  )

table(cases$any_SCI_code, useNA = "always")
table(cases$S_code_alone, useNA = "always")
table(cases$nt_code_alone, useNA = "always")
table(cases$S_nt_code_conflict, useNA = "always")
table(cases$acute_chron, useNA = "always")

pat_no_SCI <- cases %>% 
  group_by(pat_id) %>% 
  summarize(tot_SCI_codes = sum(any_SCI_code, na.rm = TRUE)) %>% 
  filter(tot_SCI_codes == 0) %>% 
  pull(pat_id)

cases <- filter(cases, pat_id != pat_no_SCI)


# Save dataset - Clear workspace -----------------------------------

save(cases, file = file.path(".", "workspace", "cases_acute_chronic.RData"))

rm("acute_nt_codes", "cases", "SCI_det", "pat_no_SCI")
