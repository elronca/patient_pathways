
#############################################################
##
##  Specifying lesion level
##
#############################################################

library(tidyverse)
library(pbapply)

load(file = file.path(".", "workspace", "cases_cause.RData"))


# Define detailed lesion level --------------------------------------------

# ICD-10 codes

Cauda <- c("G834", "S343")
S2_S5 <- c("G8267", "S3477")
L2_S1 <- c("G8266", "S3476", "S3475", "S3474", "S3473", "S3472")
T11_L1 <- c("G8265", "S3471", "S2477", "S2476") # S2476 -> T10/T11 :-/
T7_T10 <- c("G8264", "S2475", "S2474")          # S2474 -> T6/T7  :-/
T1_T6 <- c("G8263", "S2473", "S2472", "S2471")
C6_C8 <- c("G8262", "S1478", "S1477", "S1476")
C4_C5 <- c("G8261", "S1475", "S1474")
C1_C3 <- c("G8260", "S1473", "S1472", "S1471")


# Collapse the codes into one string

collapse_codes <- function(x) str_c(x, collapse = "|")

Cauda <- collapse_codes(Cauda)
S2_S5 <- collapse_codes(S2_S5)
L2_S1 <- collapse_codes(L2_S1)
T11_L1 <- collapse_codes(T11_L1) 
T7_T10 <- collapse_codes(T7_T10) 
T1_T6 <- collapse_codes(T1_T6)
C6_C8 <- collapse_codes(C6_C8)
C4_C5 <- collapse_codes(C4_C5)
C1_C3 <- collapse_codes(C1_C3)


# Function to search for string of ICD-10 codes

get_ll <- function(x, ICD_10_codes_d_lesion_level) any(str_detect(x, ICD_10_codes_d_lesion_level))


# Select the columns with ICD-10 codes

ICD_cols <- str_subset(colnames(cases), "^pd|^sd")


# Create a new column with the detailed lesion level
# If there will be multiple ICD-10 codes the lower lesion one will be overwritten with the ligher lesion one-

cases <- cases %>%
  
  mutate(
    
    lesion_level_d = case_when(
      
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, Cauda) ~ "Cauda",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, S2_S5) ~ "S2_S5",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, L2_S1) ~ "L2_S1",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, T11_L1) ~ "T11_L1",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, T7_T10) ~ "T7_T10",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, T1_T6) ~ "T1_T6",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, C6_C8) ~ "C6_C8",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, C4_C5) ~ "C4_C5",
      pbapply(X = .[ICD_cols], MARGIN = 1, get_ll, C1_C3) ~ "C1_C3",
      
      TRUE ~ NA_character_)
    
  )

table(cases$lesion_level_d, useNA = "always")



# Create paraplegia/tetraplegia variable ---------------------------------------------

para_codes <- c("G820", "G821", "G822", "G834", "S24", "S34", "G8263", "G8264", "G8265", "G8266", "G8267")
para_codes <- collapse_codes(para_codes)

tetra_codes <- c("G823", "G824", "G825", "S14", "G8260", "G8261", "G8262")
tetra_codes <- collapse_codes(tetra_codes)


catch_para_codes <- function(x) as.integer(any(grepl(para_codes, x)))
catch_tetra_codes <- function(x) as.integer(any(grepl(tetra_codes, x)))


# Identify paraplegic cases

cases$para <- pbapply(cases[ICD_cols], 1, catch_para_codes)
cases$tetra <- pbapply(cases[ICD_cols], 1, catch_tetra_codes)


# Categorize the hospitalization into paraplegia/tetraplegia

cases <- cases %>% 
  
  mutate(
    
    lesion_level = case_when(
    
    para == 1 & tetra == 0 ~ "para",
    para == 0 & tetra == 1 ~ "tetra",
    para == 1 & tetra == 1 ~ "conflict",
    
        TRUE ~ NA_character_)
    
    )

table(cases$lesion_level, useNA = "always")

conflict_cases <- filter(cases, lesion_level == "conflict")


# Recode conflict cases using the detailed lesion level -------------------

cases <- cases %>% 
  
  mutate(
    
    lesion_level = if_else(
    
    lesion_level == "conflict", 
    
    case_when(
      
      lesion_level_d == "Cauda" ~ "para",
      lesion_level_d == "S2_S5" ~ "para",
      lesion_level_d == "L2_S1" ~ "para",
      lesion_level_d == "T11_L1" ~ "para",
      lesion_level_d == "T7_T10" ~ "para",
      lesion_level_d == "T1_T6" ~ "para",
      lesion_level_d == "C6_C8" ~ "tetra",
      lesion_level_d == "C4_C5" ~ "tetra",
      lesion_level_d == "C1_C3" ~ "tetra",

      TRUE ~ lesion_level), 
    
    lesion_level)
    
    )

table(cases$lesion_level, useNA = "always")


# Principle diagnosis has priority and record accordingly -----------------

cases <- cases %>% 
  
  mutate(
    
    lesion_level = if_else(lesion_level == "conflict",
                           
                           case_when(
                             
                             lesion_level = grepl(tetra_codes, pd) ~ "tetra",
                             lesion_level = grepl(para_codes, pd) ~ "para",
                             TRUE ~ "conflict"
                             
                             ),
                           
                           lesion_level
                           
                           )
    
  )

table(cases$lesion_level, useNA = "always")


# If still not resolved, use higher lesion level --------------------------

cases[which(cases$lesion_level == "conflict"), "lesion_level"] <- "tetra"



# Find proportion of patients with missing lesion levels ------------------

lesion_level_prop_NA <- cases %>%
  
  group_by(pat_id) %>%
  
  summarize(
    
    n = n(),
    n_NA = sum(is.na(lesion_level))
    
  ) %>%
  
  mutate(lesion_level_prop_NA = round(100 * (n_NA / n), 0)) %>%
  
  arrange(desc(lesion_level_prop_NA))


## Remove patients with no lesion level at all

pat_no_lesion_level <- filter(lesion_level_prop_NA, lesion_level_prop_NA == 100) %>% pull(pat_id)

cases <- filter(cases, !pat_id %in% pat_no_lesion_level)


# Remove variables - Save dataset - Clear workspace -----------------------------------

cases <- select(cases, -para, -tetra)

save(cases, file = file.path(".", "workspace", "cases_lesion_level.RData"))

rm("C1_C3", "C4_C5", "C6_C8", "cases", "catch_para_codes", "catch_tetra_codes", 
  "Cauda", "collapse_codes", "conflict_cases", "get_ll", "ICD_cols", 
  "L2_S1", "lesion_level_prop_NA", "para_codes", "pat_no_lesion_level", 
  "S2_S5", "T1_T6", "T11_L1", "T7_T10", "tetra_codes")
