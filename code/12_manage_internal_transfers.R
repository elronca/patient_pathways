

####################################################################
## Combine internal transfers (Interne ?bertritte) into one record
####################################################################


library(tidyverse)

flatten <- rlang::flatten
pblapply <- pbapply::pblapply
na.locf0 <- zoo::na.locf
rbindlist <- data.table::rbindlist

load(file = file.path(".", "workspace", "coding_cred_score_added.RData"))

n_rows_original_file <- nrow(cases)


# Variable I need of record before the internal transfer

vars_start_it_names <- c("pat_id",  "type", "type_2", "clinic", "BUR", "seq_no_corr", "d_p_treat", "admission_pre", "admission_mode")


# Variable I need of the internal transfer

vars_end_it_names <- c("age", "sex", "discharge_post_treat", "discharge_place", "acute_chron", "cause", "lesion_level_d", 
                       "lesion_level", "completeness", "dis_year", "center_SCI", "d_next_hosp", "acute_new")


# Variables that will be combined into a list

vars_to_combine_in_list_names <- c("id", "pd", "pd_add", "pd_title_full", "pd_title_lvl_3",  
                                "pd_title_group", "pd_title_chapter", "SCI_codes_all")


# Variables were sums or averages will be calculated

vars_to_sum_up_names <- c("LOS_OECD")
vars_to_average_names <- c("SCI_coding_cred_score")


cases <- cases %>% 
  select(vars_start_it_names, vars_end_it_names, vars_to_combine_in_list_names, vars_to_sum_up_names, vars_to_average_names) %>% 
  mutate_at(vars(vars_to_combine_in_list_names), as.list)


# Find the patients with internal transfers

# pat_id_it <- cases %>% filter(admission_mode == "Uebertritt_intern") %>% pull(pat_id)

# Find patients with multiple internal transfers


# pat_id_mult_it <- cases %>% 
#   
#   mutate(mult_it = admission_mode == "Uebertritt_intern") %>% 
#   
#   group_by(pat_id) %>% 
#   
#   mutate(mult_it = sum(mult_it)) %>% 
#   
#   filter(mult_it > 1) %>% 
#   
#   pull(pat_id) %>% 
#   
#   unique



cases_l <- split(cases, cases$pat_id)

# x <- cases_l[[pat_id_mult_it[6] %>% paste]] %>% select(pat_id, seq_no_corr, BUR, admission_mode)
# x


# This function splits the hospitalizations of patients with mutliple internal transfers --------
# into different lists. The new lists will contain not more than one internal transfer
# except for interal transfers that follow directly after each other.


split_multiple_it_pp <- function(x) {
  
  if(sum(x$admission_mode %in% "Uebertritt_intern") > 1){
    
    # Find positions of internal transfers
    it_logical <- c(x[, "admission_mode"] == "Uebertritt_intern")
    
    # Find repetitions of internal transfers and other hospitalizations
    rle_object <- rle(it_logical)
    
    # Recode internal transfers as 1 and non-it as 0
    it <- as.integer(rle_object$values)
    
    # Recode non-internal transfers as NA
    it[it == 0] <- NA
    
    # Add a value of 1 to every new internal transfer, this number will be used to split the hospitalizations
    # of a patient into different chunks with only one internal transfer per chunk.
    non_NA <- !is.na(it)
    it[non_NA] <- it[non_NA] + seq(from = 1, to = length(it[non_NA]))
    
    # Impute NA's with with the number of the internal transfers backward (NOCB)
    # as these hospitalizations will be kept in the same chunk.
    it <- na.locf0(it, fromLast = TRUE, na.rm = FALSE)
    
    # Replace the NA that remain after NOCB with a new integer (Hospitalizations that are not part of an internal transfer)
    
    replace_remaining_NA_with_numb <- function(x) {
      
      if(sum(is.na(it)) > 0) {
        
        max_numb <- max(it, na.rm = TRUE)
        n_NA <- sum(is.na(it), na.rm = TRUE)
        
        it[is.na(it)] <- tail(seq(from = max_numb, to = max_numb + n_NA), n = n_NA)
        
        return(it)
        
        
      } else {
        
        return(x)
        
      }
    }
    
    it <- replace_remaining_NA_with_numb(it)
    
    # Get back to the original number of internal transfers
    it <- rep(it, rle_object$lengths)
    
    # Assign new patient_id to patients with more than one internal transfers
    # The old patient id will be extended with the number assigned in the code above
    it <- paste(unique(x$pat_id), it, sep = "_")
    
    # Split patient's observations by new pat_ids so that the patients with the new
    # pat_ids now only have one internal transfer among their hospitalizations
    out <- split(x, it)
    
    return(out)
    
  } else {
    
    return(x)
    
  }
  
}


# Split hospitalizations of patients with multiple internal transfers into separate lists with new patient id's

cases_l <- pblapply(cases_l, split_multiple_it_pp)

# New lists were sublists of the list, this is not what we want, therefore we "flatten" the lists to get only one list element

cases_l <- suppressWarnings(rlang::flatten(cases_l))



# Nested lists are flattened. Ignored warning: Outer names are only allowed for unnamed scalar atomic inputs 
# I do not need the names


# Get patiens with consecutive internal transfers

# pat_id_mult_it <- cases_l %>%
# 
#   sapply(function(x) {sum(x$admission_mode %in% "Uebertritt_intern")}) %>%
#   .[. > 1] %>%
#   names
# 
# x <- cases_l[[pat_id_mult_it[2] %>% paste]]
# # x <- select(x, pat_id, seq_no_corr, BUR, admission_mode)
# 
# x



#' Get indicies of
#' - all hospitalizations of a specific patient
#' - internal transfer,
#' - hospitalization before internal transfer,
#' - other hospitalizations of that specific patient
#' 

merge_int_trans <- function(x) {
  
  # If there are no internal transfers in a patient -> Return the patients data as it is
  
  if(!any(x$admission_mode %in% "Uebertritt_intern")) {
    
    return(x)
    
  # If there are sequences of repeating BUR numbers. If there are none. There are no real internal transfers
      } else if (any(rle(x$BUR)$lengths > 1)) {
    
  # Search for consecutive BUR numbers using rle function. (Computes the lengths and values of runs of equal values in a vector)
    rep_BUR <- rle(x$BUR)
    
    # Get indices of repeating sequences of consecutive visits to the same hospital
    index_rep_BUR <- which(rep(rep_BUR$lengths > 1, rep_BUR$lengths))
    
    # Get indices of internal transfers
    index_it <- which(x$admission_mode %in% "Uebertritt_intern")
    
    # Get indices where repeating sequences of visits to the same hospital and internal transfers overlap
    index_rep_BUR_equals_it <- intersect(index_it, index_rep_BUR)
    
    # Index of record direclty before hospitaliazation coded as internal transfer (1st admision to internal transfer)
    index_start_it <- min(index_rep_BUR_equals_it) - 1
    
    # In case internal transfer was the first hospitalization (what is unlikely but possible (e.g. at the beginning of a new year))
    index_start_it[index_start_it == 0] <- 1 

    # Indices of internal transfers
    index_it <- index_rep_BUR_equals_it
    
    # Index of last hospitalization coded as internal transfers
    index_end_it <- max(index_rep_BUR_equals_it)
    
    # Get variables I want from record before the hospitalizations that is coded as internal transfer
    vars_start_it <- x %>% select(vars_start_it_names) %>% slice(index_start_it)
    
    # Get variable I want from internal transfer
    vars_end_it <- x %>% select(vars_end_it_names) %>% slice(index_end_it)
    
    # Variables that need to be summarized accross all records
    
    all_rows_it <- unique(c(index_start_it, index_it))
    
    vars_summed_up <- x %>%
      slice(all_rows_it) %>%
      select(vars_to_sum_up_names) %>%
      summarize_all(sum, na.rm = TRUE)
    
    vars_averaged <- x %>%
      slice(all_rows_it) %>%
      select(vars_to_average_names) %>%
      summarize_all(mean, na.rm = TRUE)
    
    
    vars_listed <- x %>% 
      slice(all_rows_it) %>%
      select(vars_to_combine_in_list_names)
    
    vars_listed <- as_tibble(as.data.frame(t(lapply(vars_listed, unlist))))
    
    hosp_complete <- bind_cols(vars_start_it, vars_end_it, vars_summed_up, vars_averaged, vars_listed) 
    
    hosp_not_affected_by_internal_transfer <- x[setdiff(1:nrow(x), all_rows_it), ]
    
    out <- bind_rows(hosp_not_affected_by_internal_transfer, hosp_complete) %>% 
      arrange(seq_no_corr)
    
    return(out)
    
  } else {
    
    return(x) 
    
  }
  
}

cases_ll <- pblapply(cases_l, merge_int_trans)

cases <- as_tibble(data.table::rbindlist(cases_ll, use.names = TRUE))



rm(cases_l, cases_ll)

cases <- cases %>% 
  arrange(pat_id, seq_no_corr)




cat(n_rows_original_file - nrow(cases), "fewer hospitalizations due to collapsing of internal transfers")


# Clear workspace and save file -------------------------------------------

save(cases, file = file.path(".", "workspace", "internal_transfers_merged.RData"))

rm("cases", "flatten", "merge_int_trans", "n_rows_original_file", 
  "na.locf0", "pblapply", "rbindlist", "split_multiple_it_pp", 
  "vars_end_it_names", "vars_start_it_names", "vars_to_average_names", 
  "vars_to_combine_in_list_names", "vars_to_sum_up_names")
