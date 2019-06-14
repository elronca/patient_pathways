
###################################################################################
##
##  Extract the hospitalization of patients with at least one SCI related ICD-code
##
###################################################################################

library(data.table)
library(tidyverse)


raw_data <- "C:/Users/Elias/OneDrive/01_work/02_R/03_raw data/HOST"
# raw_data <- "D:/OneDrive/01_work/02_R/03_raw data/HOST"

####################################################################################################

# Get the variables of the datasets and check that they do not differ between the datasets

var_names_12 <- colnames(fread(file.path(raw_data, "DATA_BFS_MS_TYPO_GEMP_2012.DAT"), nrows = 1))
var_names_13 <- colnames(fread(file.path(raw_data, "DATA_BFS_MS_TYPO_GEMP_2013.DAT"), nrows = 1))

identical(var_names_12, var_names_13)

var_names <- var_names_12

rm(var_names_12, var_names_13)



# Get the variables with general information

var_gen_info <- str_subset(var_names, "_ID|^_0|^_1|_4_4|_4_7|_4_8")



# Get the variables with the diagnosis information (keep only those that end with a 0, the others are empty)

var_diag <- str_subset(var_names, "^_4_2")
var_diag <- str_subset(var_diag, "0$")



#################################################################################################
##
## Function: Extraction of hospitalization of patients with at least one SCI related ICD-code
##
#################################################################################################

# Description

# The function extracts

# (1) all hospitalizations of persons with at least one SCI related ICD10 code
# (2) the patient id's of the persons with SCI

# The output is a list where the 1st element is consists of the hospitalizations and the 2nd element is the id's of patients with SCI



# Arguments
#
# df            The dataset to read. The full path including the filename has to be provided.
#
# my_vars       The variables of the dataset that will be loaded. The patient id = X_0_2_V01a is mandatory.
#
# codes         ICD-10 codes. The dataset will be searched for hospitalizations of these patients. They will be returned including the pat_id
#
# pat_ID_add    A vector of patient id's. All hospitalizations of these specific patients will be additionally extracted.
#               This is necessary since a patient might have an SCI diagnosis in one year but not in the other one. Thus those records would be lost.




get_hosp_SCI <- function(df, my_vars, codes, pat_ID_add = NULL){
  
  df_full <- fread(df, select = my_vars, na.strings = '')                     ## Read
  
  colnames(df_full) <- str_replace(colnames(df_full), "^_", "X_")                     ## Rename variables
  
  SCI_codes <- paste0("^", codes, collapse = "|")
  
  SCI_logic <- df_full[, lapply(.SD, function(x) grepl(SCI_codes, x))]        ## Identify SCI cases
  
  pat_id_SCI <- df_full[rowSums(SCI_logic) > 0, X_0_2_V01a]                   ## Extract id's of SCI cases
  
  df_SCI <- df_full[X_0_2_V01a %in% unique(c(pat_id_SCI, pat_ID_add))]        ## Add additional patient id's and extract all hosp of pat with SCI codes
  
  df_SCI <- df_SCI[X_0_2_V01a != 0]                                           ## Pat ID 0 is not a real patient
  
  return(list(df_SCI, unique(pat_id_SCI)))
  
}



# Extract hospitalizations of year 2012 -----------------------------------------------

SCI_2012 <- get_hosp_SCI(
  
  df = file.path(raw_data, "DATA_BFS_MS_TYPO_GEMP_2012.DAT"), 
  
  my_vars = c(var_gen_info, var_diag, "_4_3_V015", "KlinikName"),
  
  codes = c("G82", "G834", "S14", "S24", "S34")
  
)


# Get list elements -------------------------------------------------------

# Hospitalizations

hosp_2012 <- SCI_2012[[1]]

# Patient id's

pat_id_2012 <- SCI_2012[[2]]




# Extract hospitalizations of year 2013 -----------------------------------------------

SCI_2013 <- get_hosp_SCI(
  
  df = file.path(raw_data, "DATA_BFS_MS_TYPO_GEMP_2013.DAT"), 
  
  my_vars = c(var_gen_info, var_diag, "_4_3_V015", "KlinikName"),
  
  codes = c("G82", "G834", "S14", "S24", "S34"),
  
  pat_ID_add = pat_id_2012
  
)

hosp_2013 <- SCI_2013[[1]]



# Combine hospitalizations from the years 2012 & 2013 ---------------------------------

cases <- as.data.frame(rbind(hosp_2012, hosp_2013, stringsAsFactors = FALSE))


# Save dataset - Clear workspace - Detach data.tabe -----------------------------------

save(cases, file = file.path(".", "workspace", "data_SCI_raw.RData"))

rm("cases", "get_hosp_SCI", "pat_id_2012", "raw_data", "var_diag", "var_gen_info", "var_names", 
   "hosp_2012", "hosp_2013", "SCI_2012", "SCI_2013")

detach("package:data.table", unload = TRUE)
