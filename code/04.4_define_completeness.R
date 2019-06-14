
#############################################################
##
##  Specifying completeness
##
#############################################################

library(tidyverse)
library(pbapply)


load(file = file.path(".", "workspace", "cases_lesion_level.RData"))



# Variables with diagnosis information

ICD_cols <- str_subset(colnames(cases), "^pd|^sd")


# Create SCI codes ---------------------------------------------

chron_SCI <- c("G820", "G821", "G822", "G823", "G824", "G825")


# Complete lesions

complete_chron_SCI <- c(paste0(chron_SCI,"0"), paste0(chron_SCI,"2"), "G8340")

complete_acute_SCI <- c("S1411", "S2411", "S3410", "S3430")

complete_SCI_codes <- str_c(c(complete_chron_SCI, complete_acute_SCI), collapse = "|")


# Incomplete lesions

incomplete_chron_SCI <- c(paste0(chron_SCI, "1"), paste0(chron_SCI, "3"), "G8341")

incomplete_acute_SCI <- c("S1412", "S1413", "S2412", "S3411", "S3431")

incomplete_SCI_codes <- str_c(c(incomplete_chron_SCI, incomplete_acute_SCI), collapse = "|")


catch_complete_codes <- function(x) as.integer(any(grepl(complete_SCI_codes, x)))
catch_incomplete_codes <- function(x) as.integer(any(grepl(incomplete_SCI_codes, x)))


# Identify cases ----------------------------------------------------------

cases$complete <- pbapply(cases[ICD_cols], 1, catch_complete_codes)
cases$incomplete <- pbapply(cases[ICD_cols], 1, catch_incomplete_codes)


# Categorize the hospitalization into complete/incomplete SCI -------------

cases <- cases %>% 
  
  mutate(
    
    completeness = case_when(
      
      complete == 1 & incomplete == 0 ~ "complete",
      complete == 0 & incomplete == 1 ~ "incomplete",
      complete == 1 & incomplete == 1 ~ "conflict",
      TRUE ~ NA_character_
      
    )
    
  )

table(cases$completeness, useNA = "always")



# Principle diagnosis has priority and record accordingly -----------------

cases <- cases %>% 
  
  mutate(
    
    completeness = if_else(completeness == "conflict",
                           
                           case_when(
                             
                             completeness = str_detect(pd, incomplete_SCI_codes) ~ "incomplete",
                             
                             completeness = str_detect(pd, complete_SCI_codes) ~ "complete",
                             
                             TRUE ~ "conflict"
                             
                           ),
                           
                           completeness
                           
    )
    
  )

table(cases$completeness, useNA = "always")


# If still not resolved, use higher completion level --------------------------

cases[which(cases$completeness == "conflict"), "completeness"] <- "complete"


# Remove variables - Save dataset - Clear workspace -----------------------------------

cases <- select(cases, -complete, -incomplete)

save(cases, file = file.path(".", "workspace", "cases_completeness.RData"))

rm("ICD_cols", "cases", "catch_complete_codes", "catch_incomplete_codes", 
  "chron_SCI", "complete_acute_SCI", "complete_chron_SCI", "complete_SCI_codes", 
  "incomplete_acute_SCI", "incomplete_chron_SCI", "incomplete_SCI_codes")
