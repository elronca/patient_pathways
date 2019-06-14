##############################################
##
##  Identify Balgrist hospital
##
##############################################

library(tidyverse)
library(zoo)


load(file = file.path("Workspace", "appliedExclCrit.RData"))


## Get BUR numbers of specialized centers


spec_SCI <- cases %>% 
  filter(clinic %in% levels(clinic)) %>% 
  distinct(BUR) %>% 
  pull()


# Get BUR numbers of specialized surgical clinics

spez_surg <- cases %>% 
  filter(type == "Spez_klinik_Chirurg") %>% 
  distinct(BUR) %>% 
  pull()


# Get BUR numbers of university hosptitals

univ_hosp <- cases %>% 
  filter(type == "Zentrumsvers_Univ") %>% 
  distinct(BUR) %>% 
  pull()


# Investigate hospitalizations of the three types of hospitals ------------

hosp_spez_surg <- filter(cases, BUR %in% spez_surg)
hosp_spez_SCI <- filter(cases, BUR %in% spec_SCI)
hosp_spez_univ <- filter(cases, BUR %in% univ_hosp)


# Compare the LOS between the hospital types ------------------------------

# Specialized SCI clinics

hosp_spez_SCI %>% 
  
  group_by(clinic) %>% 
  
  summarise(
    
    mean_LOS = mean(LOS_OECD, na.rm = TRUE), 
    n_SCI = n()
    
  )



# University hospitals

hosp_spez_univ %>% 
  
  group_by(BUR) %>% 
  
  summarise(
    
    mean_LOS = mean(LOS_OECD, na.rm = TRUE),  
    n_SCI = n()
    
    ) %>% 
  
  arrange(desc(n_SCI))


# University hospital number 118 is an interesting cases. The persons with SCI stay here for quite some time.
# It could possibly be that this is the university hospital of Zurich which then also includes Balgrist.

# The university hospitals are: CHUV, HUG, INSEL, UZH, Basel
# What about Kispi?


## Specialized surgical hospitals

hosp_spez_surg %>% 
  
  group_by(BUR) %>% 
  
  summarise(
    
    mean_LOS = mean(LOS_OECD, na.rm = TRUE), 
    n_SCI = n()) %>% 
  
  arrange(desc(n_SCI)) %>% 
  print(n = 22)

# It looks like some specialized surgical clinics have less SCI cases than university hospitals. This is possible.
# However, it is unlikely that a specialized surgical clinic has more SCI cases than two of the specialized clinics.

# It is very likely that BUR 148 is Balgrist.

cases <- cases %>% 
  
  mutate(
    
    clinic = as.character(clinic),
    clinic = if_else(BUR == 148, "Balgrist", clinic),
    center_SCI = if_else(clinic %in% c("Balgrist", "RehaB", "SPZ", "SuvaCare"), 1, 0),
    type_2 = as.character(type),
    type_2 = if_else(center_SCI == 1, "center_SCI", type_2)
    
  )

table(cases$clinic, useNA = "always")
table(cases$type_2, useNA = "always")



# Remove variables - Save dataset - Clear workspace -----------------------------------

save(cases, file = file.path("workspace", "spec_cent_identified.RData"))

rm("cases", "hosp_spez_SCI", "hosp_spez_surg", "hosp_spez_univ", 
   "spec_SCI", "spez_surg", "univ_hosp")
