###################################################################################
##
## Diagnostics - Proportions of patients who visited a SCI center by hospital type
##
###################################################################################

library(tidyverse)

load(file = file.path("workspace", "vars_reduced.RData"))
load(file = file.path("workspace", "diagnostic_vars.RData"))

cases <- cases %>% select(id, pat_id, age, sex, type_2, dis_year, cause, completeness, lesion_level) %>% 
  left_join(diagnostic_vars, by = "id")

rm(diagnostic_vars)



# Proportion of patinets who visited a specialized SCI center -------------

# For every hospital type we want to find the proportion of patients who visited a specialized SCI center.
# This might give us an idea about the referral practice of these hospitals

# x = vector of hospital types I want to include
# drop_vars = We use only records where the hospital coded an SCI diagnosis

calc_prop_SCI_cent_visit_per_hosp_type <- function(x, vars_remove_missings = "lesion_level_orig", 
                                                   cause_SCI = NULL, completeness_SCI = NULL, lesion_level_SCI = NULL) {
  
  # Find patients who visited a SCI center at least once
  
  cases %>% 
    
    mutate(SCI_center_patient = if_else(cases$type_2 == "center_SCI", 1, 0)) %>% 
    
    group_by(pat_id) %>% 
    
    mutate(SCI_center_patient = sum(SCI_center_patient),
           SCI_center_patient = if_else(SCI_center_patient >= 1, "SCI_center_patient", "no_SCI_center_patient")) %>% 
    
    ungroup %>% 
    
    mutate(SCI_center_patient = as_factor(SCI_center_patient)) %>% 
    
    
    # Find patients who visited a SCI center among all patients and among those who visisted a SCI center at least once
    
    {if(x != "all") filter(., type_2 == x) else .} %>%
    
    {if(any(cause_SCI %in% c("nontraumatic", "traumatic"))) filter(., cause == cause_SCI) else .} %>% 
    {if(any(completeness_SCI %in% c("complete", "incomplete"))) filter(., completeness == completeness_SCI) else .} %>%
    {if(any(lesion_level_SCI %in% c("para", "tetra"))) filter(., lesion_level == lesion_level_SCI) else .} %>%
    
    drop_na(vars_remove_missings) %>% 
    
    {
      
      if(nrow(.) == 0) 
        
        return(NULL) 
      
      else 
        
        distinct(., pat_id, .keep_all = TRUE) %>% 
        
        group_by(SCI_center_patient) %>% 
        
        summarize(n = length(SCI_center_patient)) %>% 
        
        complete(SCI_center_patient, fill = list(n = 0)) %>% 
        
        spread(SCI_center_patient, n) %>% 
        
        mutate(all_patients = SCI_center_patient + no_SCI_center_patient,
               prop_SCI_center_patients = if_else(all_patients != 0, round(100 * SCI_center_patient / all_patients, 0), NaN)) %>% 
        
        mutate(hosp_type := x)
      
    }
  
}


# This function plots the proportion of patients who visited a specialized SCI center for every hospital type

plot_prop_SCI_cent <- function(df, my_ylab, my_title, hosp_types) {
  
  
  # Prepare data to plot
  
  to_plot <- select(df, -all_patients, -prop_SCI_center_patients) %>% 
    
    filter(hosp_type != "center_SCI") %>% # we are not interested in how many persons who visited an SCI center visited an SCI center
    
    gather("SCI_center_patient", "n_patients_per_category", -hosp_type) %>% # From wide to long
    
    mutate_at(vars(c("hosp_type", "SCI_center_patient")), as_factor)
  
  
  # Plot data
  
  p <- ggplot(to_plot, aes(x = hosp_type, y = n_patients_per_category))
  
  p <- p + geom_bar(aes(fill = SCI_center_patient), stat = "identity")
  
  p <- p + xlab("Type of hospital")
  p <- p + ylab(my_ylab)
  p <- p + ggtitle(my_title, subtitle = "among patients who visited a specific hospital type")
  
  p <- p + theme(
    axis.text.x = element_text(angle = -45, hjust = 0, size = 11),
    panel.background = element_blank())
  
  p <- p + scale_fill_discrete(name = "visited\nSCI center", labels = c("Yes", "No"))
  
  
  # Add percentage of persons who visited a SCI center
  
  prop_SCI_cent <- to_plot %>% 
    group_by(hosp_type) %>% 
    mutate(all_patients = sum(n_patients_per_category),
           prop_SCI_cent = round(n_patients_per_category / all_patients * 100)) %>% 
    filter(SCI_center_patient == "SCI_center_patient")
  
  
  
  
  p <- p + geom_text(data = prop_SCI_cent,
                     
                     aes(x = hosp_type, y = all_patients + max(all_patients)/20, label = paste0(prop_SCI_cent, "%")),
                     
                     size = 3)
  
  plot(p)
  
  
}




# Describe and plot proportions of patients who visited a SCI center by hospital type -----------------------

hosp_types <- cases %>% group_by(type_2) %>% tally %>% arrange(desc(n)) %>% pull(type_2) %>% c("all", .) %>% as_factor


# All patients

all_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type) %>% 
  
  bind_rows()


plot_prop_SCI_cent(all_patients, 
                   hosp_types = hosp_types,
                   my_ylab = "All patients", 
                   my_title = "Patients who visited a specialized center")


cat(
  all_patients %>% filter(hosp_type == "all") %>% pull(prop_SCI_center_patients), 
  "% of all patients were treated at a specialized SCI clinic.",
  sep = ""
  )

cat(
  all_patients %>% filter(hosp_type == "Zentrumsversorgung") %>% pull(prop_SCI_center_patients), 
  "% of all patients who were treated at a Zentrumsspital were also treated at a specialized SCI clinic. 
  It seems that only one in five Zentrumsspitäler refer their patients to a specialized SCI clinic.",
  sep = ""
)






# Traumatic SCI patients

TSCI_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, cause_SCI = "traumatic") %>% 
  
  bind_rows()

plot_prop_SCI_cent(TSCI_patients, 
                   my_ylab = "TSCI patients",
                   my_title = "Patients with TSCI who visited a specialized center")


# Nontraumatic SCI patients

NTSCI_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, cause_SCI = "nontraumatic") %>% 
  
  bind_rows()

plot_prop_SCI_cent(NTSCI_patients, 
                   my_ylab = "NTSCI patients",
                   my_title = "Patients with NTSCI who visited a specialized center")



# Complete SCI

complete_SCI_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         completeness_SCI = "complete", vars_remove_missings = "lesion_level_orig", "completeness") %>% 
  
  bind_rows()

plot_prop_SCI_cent(complete_SCI_patients, 
                   my_ylab = "Patients with complete SCI",
                   my_title = "Patients with complete SCI who visited a specialized center")



# Incomplete SCI

incomplete_SCI_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         completeness_SCI = "incomplete", vars_remove_missings = "lesion_level_orig", "completeness") %>% 
  
  bind_rows()

plot_prop_SCI_cent(incomplete_SCI_patients, 
                   my_ylab = "Patients with incomplete SCI",
                   my_title = "Patients with incomplete SCI who visited a specialized center")


# Paraplegia

para_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         lesion_level_SCI = "para", vars_remove_missings = c("lesion_level_orig", "lesion_level")) %>% 
  
  bind_rows()

plot_prop_SCI_cent(para_patients, 
                   my_ylab = "Patients with paraplegia",
                   my_title = "Patients with paraplegia who visited a specialized center")


# Tetraplegia

tetra_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         lesion_level_SCI = "tetra", vars_remove_missings = c("lesion_level_orig", "lesion_level")) %>% 
  
  bind_rows()

plot_prop_SCI_cent(tetra_patients, 
                   my_ylab = "Patients with tetraplegia",
                   my_title = "Patients with tetraplegia who visited a specialized center")


# Complete tetraplegia

complete_tetra_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         lesion_level_SCI = "tetra", completeness_SCI = "complete", 
         vars_remove_missings = c("lesion_level_orig", "lesion_level", "completeness")) %>% 
  
  bind_rows()

plot_prop_SCI_cent(complete_tetra_patients, 
                   my_ylab = "Patients with complete tetraplegia",
                   my_title = "Patients with complete tetraplegia who visited a specialized center")


# Incomplete paraplegia

incomplete_para_patients <- hosp_types %>% 
  
  lapply(calc_prop_SCI_cent_visit_per_hosp_type, 
         lesion_level_SCI = "para", completeness_SCI = "incomplete", 
         vars_remove_missings = c("lesion_level_orig", "lesion_level", "completeness")) %>% 
  
  bind_rows()

plot_prop_SCI_cent(incomplete_para_patients, 
                   my_ylab = "Patients with incomplete paraplegia",
                   my_title = "Patients with incomplete paraplegia who visited a specialized center")



# Clear workspace ---------------------------------------------------------

rm("all_patients", "calc_prop_SCI_cent_visit_per_hosp_type", "cases", 
  "complete_SCI_patients", "complete_tetra_patients", "hosp_types", 
  "incomplete_para_patients", "incomplete_SCI_patients", "NTSCI_patients", 
  "para_patients", "plot_prop_SCI_cent", "tetra_patients", "TSCI_patients")
