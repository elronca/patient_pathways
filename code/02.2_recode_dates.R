
#######################################################################################
##
##  Define admission/discharge date and chronological sequence of hosptalizations
##
#######################################################################################


library(tidyverse)
library(lubridate)
library(zoo)
library(pbapply)
rbindlist <- data.table::rbindlist

load(file = file.path(".","workspace", "vars_renamed.RData"))



###############################################################################
##
## Remove hospitaizations that overlap years and comprise incomplete data 
##
###############################################################################


table(cases$stat_type) # stat_type = Kennzeichnung des Statistikfalls


## From the BFS documentation

# Fall ? A ?: 
# H?ufigster Fall, mit Austrittsdatum zwischen 1. Januar und 31. Dezember. 
# Diagnosen und Behandlungen werden erhoben, es kann eine Aufenthaltsdauer berechnet werden. 
# Der gesamte Minimaldatensatz muss ausgef?llt werden.


#  Fall ? B ? 
# entspricht einem Fall mit Behandlungsbeginn w?hrend der Erhebungsperiode, 
# bei welchem die Hospitalisation ?ber den 31. Dezember hinausgeht. 
# In diesem Falle m?ssen die Variablen der Bereiche ? 1.5. Austrittsmerkmale ?, 
# ? 1.6. Diagnosen ? und ? 1.7. Behandlungen ?, nicht ausgef?llt werden.


# Fall ? C ? 
# entspricht einer l?ngeren Behandlung, deren Beginn schon im Vorjahr der Erhebungsperiode 
# eingetreten ist und bei welcher die Hospitalisation ?ber den 31. Dezember der Erhebungsperiode hinausgeht. 
# In diesem Falle muss der gesamte Minimaldatensatz ohne ?1.5. Austrittsmerkmale ?, ausgef?llt werden.



## This data will be handled accordingly:

# Type A: All hospitalizations with type A will be used for further analysis
#
# Type C: Those are only a few cases where relevant information is missing. 
#         Those hospitalizations will be removed
#           a) as they are very uncommon
#           b) as they are only a few cases
#           c) as they do not serve the purpose of our analysis since important information is missing.
#
# Tybe B: These cases will be removed
#           a) as the cases of 2012 which are incomplete are comprised in the year 2013 again and would be used twice
#           b) as important information such as admission date or length of stay is missing for cases of the year 2013.



# Remove type C cases as they are complicating things are not suited for our purpose

cases <- filter(cases, !stat_type %in% c("B", "C"))


# Die Sequenznummer gibt dabei die zeitliche Reihenfolge der station?ren Aufenthalte pro Person an.
# https://www.fmh.ch/files/pdf15/2014_09_22_Schlussbericht_Studie_Verschiebungen_stationr_-_ambulant.pdf

# Als Pflege- resp. Aufenthaltstage gelten der Aufnahmetag sowie jeder weitere Tag
# des Spitalaufenthaltes ohne den Verlegungs- oder Entlassungstag. Vollst?ndige
# Urlaubstage z?hlen ebenfalls nicht zur Aufenthaltsdauer. Betrachtungsrahmen in der
# KS ist das Erhebungsjahr (Kalenderjahr).



################################################################################
##
## Create admission/discharge date
##
################################################################################


# (1) Admission month plus LOS divided by an approximation of the days of a month gives an estimation of the discharge month.
# (2) Estimate the year of admission
# (3) Gives the discharge month without the year
# (4) Replaces month 0 with month 12
# (5) We don't know the exact day of discharge date or admission date so we assume it to be the first of the month.
#     The date is prepared as a string
# (6) The date is transferred into a proper date format


# !!Note Most of these variables will be hardly used as they are horribly inaccurate 
# Important variable is d_p_treat originally: Beginn_der_MD_Hauptbehandlung_JJJJMMTTSS

cases <- cases %>% 
  
  mutate(
    
    days_per_month = 365/12,
    dis_month = adm_month + round(LOS_SDRG / days_per_month, 0), # (1)
    adm_year = dis_year - floor(dis_month / 12), # (2)
    dis_month = dis_month %% 12, # (3)
    dis_month = if_else(dis_month == 0, 12, dis_month), # (4)
    adm_date = as.Date(paste(adm_year, adm_month, "01", sep = "-")), # (5)
    dis_date = as.Date(paste(dis_year, dis_month, "01", sep = "-")), # (5)
    d_p_treat =  substr(d_p_treat, 1, 8), # (5)
    d_p_treat = ymd(d_p_treat) #(6)
    
  ) %>% 
  
  select(-days_per_month)
  
cases %>% select(pat_id, dis_month, adm_year, dis_month, adm_date, dis_date, d_p_treat)


# The sequence number seems to be off at times ----------------------------



# Sometimes the sequence number does not seem to be consistent with the dates of principle treatment
# See example below

my_pat <- cases %>%
  filter(pat_id == "121257") %>%
  select(id, pat_id, seq_no, dis_date, d_p_treat, LOS_OECD, BUR, admission_mode) %>%
  arrange(seq_no) %>% 
  print()


# If hospitalizations are ordered according to the date of the principle treatment then the internal transfer makes sense.

corrected_seq_numb <- my_pat %>%
  arrange(seq_no) %>%
  pull(seq_no)

my_pat %>%
  arrange(d_p_treat) %>%
  mutate(seq_no = corrected_seq_numb)


# Function that produced a corrects the sequence number based on the day of the principle treatment
# if there are no missing values in the day of principle treatmant

rearrange_seq_no_by_d_p_treat <- function(x) {
  
  if(any(is.na(x$d_p_treat))) {
    
    return(x)
    
  } else {
    
    corrected_seq_numb <- x %>% 
      arrange(seq_no) %>% 
      pull(seq_no)
    
    x <- x %>% 
      arrange(d_p_treat) %>% 
      mutate(seq_no_corr = corrected_seq_numb)
    
    return(x)
    
  }
  
}

# Copy the original sequence number and give it a new name

cases <- mutate(cases, seq_no_corr = seq_no)


# Apply function that corrects the sequence number via the date of principle treatment to every patient separately

cases_l <- split(cases, cases$pat_id)

cases_ll <- pblapply(cases_l, rearrange_seq_no_by_d_p_treat)

cases <- as_tibble(rbindlist(cases_ll))


# Save dataset and clear workspace -----------------------------------------------------

save(cases, file = file.path(".", "workspace", "cases_dates_sorted.RData"))

rm("cases", "cases_l", "cases_ll", "corrected_seq_numb", "my_pat", 
   "rbindlist", "rearrange_seq_no_by_d_p_treat")