
##############################################################################################
##
## Diagnostics - Analysis of coding of lesion characteristics
##
##############################################################################################

library(tidyverse)


# Load data

load(file = file.path("workspace", "vars_reduced.RData"))
load(file = file.path("workspace", "diagnostic_vars.RData"))

cases <- cases %>% 
  
  select(id, pat_id, age, sex, type_2, dis_year, cause, completeness, lesion_level_d) %>% 
  
  left_join(diagnostic_vars, by = "id")


# Determine proportions of records coded with a completeness or detailed lesion level ICD-10 code ----------

prop_coded <- cases %>% 
  
  drop_na(lesion_level_orig, cause) %>% 
  
  group_by(type_2, cause) %>% 
  
  summarize(n = n(),
            
            completenss_miss_n = sum(is.na(completeness_orig)),
            completenss_coded_perc = 100 - round(100 * completenss_miss_n / n),
            
            d_lesion_level_miss_n = sum(is.na(lesion_level_d_orig)),
            d_lesion_level_coded_perc = 100 - round(100 * d_lesion_level_miss_n / n )) %>% 
  
  select(type_2, cause, n, completenss_coded_perc, d_lesion_level_coded_perc) %>% 
  
  ungroup %>% 
  
  mutate(type_2 = fct_relevel(type_2, 
                              
                              c("center_SCI", "Zentrumsvers_Univ", 
                                "Zentrumsversorgung", "Rehab_klinik", 
                                "Spez_klinik_Chirurg", "Grundversorgung_Niv_3", 
                                "Grundversorgung_Niv_4",  "Grundversorgung_Niv_5", 
                                "Spez_klinik_Div", "Spez_klinik_Geri", 
                                "Spez_klinik_Paed", "Psych_Niv_1")),
         
         type_2 = fct_recode(type_2,
                             
                             "SCI Zentrum" = "center_SCI", 
                             "Universitätsspital" = "Zentrumsvers_Univ", 
                             "Kantonsspital" = "Zentrumsversorgung", 
                             "Rehaklinik" = "Rehab_klinik", 
                             "Chirurgische Klinik" = "Spez_klinik_Chirurg", 
                             "Grosses Lokalspital" = "Grundversorgung_Niv_3", 
                             "Mittleres Lokalspital" = "Grundversorgung_Niv_4",  
                             "Kleines Lokalspital" = "Grundversorgung_Niv_5", 
                             "Diverse Spezialkliniken" = "Spez_klinik_Div", 
                             "Geriatrische Spezialklinik" = "Spez_klinik_Geri", 
                             "Pädiatrische Spezialklinik" = "Spez_klinik_Paed", 
                             "Grössere Psychiatrie" = "Psych_Niv_1"))



# Prepare data to be plotted ----------------------------------------------------


to_plot <- gather(prop_coded, key = "lesion_char", value = "perc_coded", -type_2, -cause, -n) %>% 
  
  mutate(
    
    lesion_char = fct_recode(lesion_char, "Completeness" = "completenss_coded_perc", "Lesion level" = "d_lesion_level_coded_perc"),
    
    cause = fct_recode(cause, "Nontraumatic" = "nontraumatic", "Traumatic" = "traumatic"),
    
    cause = fct_relevel(cause, "Traumatic", "Nontraumatic")
    
    ) %>% 
  
  group_by(type_2, lesion_char) %>% 
  
  mutate(n_tot = sum(n)) %>% 
  
  ungroup %>% 
  
  arrange(type_2)


# Define names of hospitals and numbers of individuals who visited each hospital


col.labs <- paste0(c(
  
  # names
  
  "SCI Zentrum\n", "Universitäts-\nspital", "Kantons-\nspital", "Rehaklinik\n", 
  "Chirurgische\nKlinik", "Grosses\nLokalspital", "Mittleres\nLokalspital", 
  "Kleines\nLokalspital", "Diverse\nSpezialkliniken", "Geriatrische\nSpezialklinik", 
  "Pädiatrische\nSpezialklinik", "Grössere\nPsychiatrie"),
  
  # n = 
  
  "\n\nn = ", 
  
  # number of hospitalizations per hospital type
  
  to_plot %>% distinct(type_2, .keep_all = TRUE) %>% pull(n_tot)
  
)

names(col.labs) <- dput(levels(to_plot$type_2))



p <- ggplot(to_plot)

p <- p + geom_bar(aes(x = cause, y = perc_coded, fill = cause), stat = "identity", position = 'dodge')

p <- p + labs(x = NULL, 
              y = "Proportion of missing information/ codes (%)", 
              fill = "Cause of SCI",
              title = "Proportion of records where the lesion completeness and detailed lesion level were coded",
              subtitle = "stratified by hospital type and cause of injury",
              caption = "The lesion completeness is part of the non-traumatic ICD-10 SCI code. Therefore, the information about the completeness of injury is considerably higher in non-traumatic cases than in traumatic cases.")

p <- p + theme_bw()

p <- p + theme(
  
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(), 
  plot.title = element_text(size = 12, face = "bold"),
  plot.subtitle = element_text(size = 10),
  plot.caption = element_text(face = "italic", size = 9)
  
)


p <- p + facet_grid(rows = vars(lesion_char), cols = vars(type_2), labeller = labeller(type_2 = col.labs))

p <- p + theme(
  strip.text.x = element_text(angle = 0, size = 7),
  strip.text.y = element_text(angle = 270, size = 8),
  strip.background = element_rect(colour = "black", fill = "white"),
  panel.grid = element_blank(),
  legend.position = "bottom")

print(p)

ggsave(file.path("output", "prop_les_comp_levl_coded.pdf"), device = "pdf", width = 10, height = 5)


# In the nontraumatic cases the completeness of lesion is much more often coded than in traumatic cases.

# The reason for this is likely that the completeness of a lesion is part of the acute nontraumatic SCI code whereas
# an acute traumatic SCI can also be coded without defining the completeness of a lesion.

# The reason why not all completeness codes are available for nontraumatic cases is that, the compleness of injury information 
# was imputed # for all hospitalizations of a patient if it was existent in at least one hospitalization of a patient.

# If we do a regression with the completeness as variable we will 
# (1) loose a lot of traumatic cases and increase the proportion of nontraumatic cases.
# (2) have a higher proportion of patients who went to specialized clinics as they were more often coding the completeness 
#     in nontraumatic cases than other hospitals.


# Clear workspace  - Proportions of completeness and detailed level coded by clinic --------

rm("col.labs", "diagnostic_vars", "p", "prop_coded", "to_plot")



# Proportion of SCI center visitors after removing missings ---------------

# Get proprotion of patients who visted a specialized SCI center

cases_prep <- cases %>% 
  
  mutate(SCI_cent_hosp = if_else(cases$type_2 == "center_SCI", 1, 0)) %>% 
  
  group_by(pat_id) %>% 
  
  mutate(SCI_cent_pat = sum(SCI_cent_hosp),
         SCI_cent_pat = if_else(SCI_cent_pat >= 1, 1, 0)) %>% 
  
  ungroup %>% 
  
  select(-SCI_cent_hosp)



#' Function to calculate proportion of patients who visted a specialized SCI center
#' after removing missings values in lesion characteristics variables

prop_SCI_cent_visit <- function(drop_var) {
  
  cases_prep %>% 
    
  {if(any(!drop_var %in% "none")) drop_na(., drop_var) else .} %>%
    
    distinct(pat_id, .keep_all = TRUE) %>% 
    
    summarize(
      
      n = n(), 
      
      n_visited_SCI_cent = sum(SCI_cent_pat),
      
      n_not_visited_SCI_cent = n - sum(SCI_cent_pat),
      
      prop_visited_SCI_cent = n_visited_SCI_cent / n) %>%
    
    
    mutate(vars_missings_dropped = paste(drop_var, collapse = ", "))
  
}


drop_vars <- list("none", 
                  "cause", 
                  "completeness", 
                  "lesion_level_d", 
                  c("cause", "completeness", "lesion_level_d")
)

# Run the function with the different variables from which I want to remove the missing values

prop_visit_SCI_cent <- bind_rows(lapply(drop_vars, prop_SCI_cent_visit)) %>% print


# Plot the proportion of patients who visited a specialized center at least once without removing missings
# and after removing missings in in cause of SCI, completeness of injury, detailed lesion level, 
# and all lesion characteristics


to_plot <- prop_visit_SCI_cent %>% 
  select(vars_missings_dropped, prop_visited_SCI_cent) %>% 
  mutate(vars_missings_dropped = str_replace_all(vars_missings_dropped, ", ", " &\n"))
  

p <- ggplot(to_plot)

p <- p + aes(x = reorder(vars_missings_dropped, prop_visited_SCI_cent), y = prop_visited_SCI_cent)

p <- p + geom_bar(stat = "identity")

p <- p + xlab("Variables where missings were removed")

p <- p + ylab("Proportion of patients who visited an SCI center")

p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1))

p <- p + ggtitle("Proportion of patients who visited a specialized center", 
                 subtitle = "after removing missing values in different variables")

p <- p + theme_bw()

p <- p + theme(panel.grid = element_blank())

print(p)

ggsave(file.path("output", "visit_spec_cent_after_removing_missings.pdf"), device = "pdf", width = 5, height = 5)

#' The problem here is that when removing missing values in important variables 
#' for example in a complete-case regression, we are increasing
#' involuntarley the proportion of visits to SCI centers.
#' 
#' The reason for this is that SCI centers are more likely to code variables 
#' such as completness or detailed lesion level.
#' 
#' If we are removing missings in these variables it looks like the proportion of individuals who are visiting
#' SCI centers is higher than it acutally is. This is missleading.



# Clear workspace - Missings removed --------------------------------------

rm("cases", "cases_prep", "drop_vars", "p", "prop_SCI_cent_visit", 
  "prop_visit_SCI_cent", "to_plot")
