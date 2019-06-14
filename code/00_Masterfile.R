
#######################################
## Master file for patient pathways ###
#######################################

## Load necessary packages ------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rlang)
library(data.table)
library(zoo)
library(pbapply)
library(openxlsx)
library(effects)
library(lmtest)


# Check working directory

getwd()


# Create required folders

dir.create(file.path("output"), showWarnings = FALSE)
dir.create(file.path("workspace"), showWarnings = FALSE)
dir.create(file.path("data"), showWarnings = FALSE)


dir()


## Run scripts ---------------------------------------------------------------------------


# Load data
# Change path to folder in file 01_load_data.R, where the hospital discharge data is stored on your computer.

source(file.path("code", "01_load_data.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Rename variables and save diagnosis codes

source(file.path("code", "02.1_rename_variables.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Create date variables

source(file.path("code", "02.2_recode_dates.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Rename variable categories

source(file.path("code", "02.3_rename_factors.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Save/Remove/Order variables

source(file.path("code", "02.4_save_remove_vars.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Add ICD-10 information about the principle diagnosis

source(file.path("code", "03_add_ICD10_txt_to_pd.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Define whether a persons went to the hospital for an acute or chornic SIC

source(file.path("code", "04.1_define_acute_chron.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Categorize tramatic/non-tramatic

source(file.path("code", "04.2_define_traumatic_nontraumatic.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Define Lesion level

source(file.path("code", "04.3_define_lesion_level.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Define completeness of lesion

source(file.path("code", "04.4_define_completeness.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Do a plausibility check

source(file.path("code", "05_plausibility_check.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Apply SwiSCI exclusion crtieria

source(file.path("code", "06_appl_excl_crit.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Identify the Balgrist hospital via BUR code and persons with SCI who visited a specific hospital

source(file.path("code", "07_identify_Balgrist.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Use the lesion characteristics information of one hospitalization of a patient to complete the information
# in the other hospitalizations of the same patient.

source(file.path("code", "08_impute_lesion_characteristics.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Define what is really an acute SCI episode or what is chronic

source(file.path("code", "09_define_what_is_acute.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Remove variables that are not necessary for further analyses

source(file.path("code", "10_reduce_numb_vars.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Identify the proportion of patients who visited different types of hospital who also visited a SCI center.
# This might give an idea of the referral behavior of different types of hospitals.

source(file.path("code", "11.1_diagnostics_prop_SCI_cent.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Even after imputing lesion information there are sill a lot of patients with SCI who are missing information
# about their lesion characterstics. Different types of hospitals seem to have coded SCI characterstics more or less 
# complete. If we  remove those patients where the lesion information is not complete (e.g. in a regression) we
# will weight those hospitals who did a complete coding higher which will most likely lead to biased information
# about the likelihood of persons with SCI to visit a specialized clinics. -> It will seem higher than it acutally is.

# -> Check pdfs in output folder for a better quality of the figures

source(file.path("code", "11.2_diagnostics_prop_d_ll_and_compl_coded.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# We add a quality measure of the coding. How much lesion characteristics information is coded by person. The measure counts
# the average SCI-related ICD-10 digits per hospitalization of a patients.

# The maximum is 10. Full information about whether the patient has a complete or incomplete lesion and whether the SCI
# is traumatic or non traumatic (5 digits e.g. G82.00). Furthermore there will be an exact coding of the lesion level 
# (e.g. G82.60). This will be 5 aditional digits. In the worst case there will be only the G82 or S14 code in one
# hospitalization of a patient and more hospitalizations of the same patient without an SCI related ICD-10 code.

source(file.path("code", "11.3_diagnostics_SCI_coding_cred_score_added.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# There are internal transfers in the same hospital. Those should be part of the same hospitalization in the patient
# pathways. Therefore we merge these hospitalizations.

source(file.path("code", "12_manage_internal_transfers.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Create the patient pathways

source(file.path("code", "13_pat_pathways.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# Get a sample of patients to look at them more specifically. This will allow us (or Anke) to find problems with
# how the pathways are constructed right now. Maybe we need to apply additional exclusion criteria.

source(file.path("code", "14_get_sample_pat_path.R"), encoding = "utf-8", local = TRUE, echo = TRUE)


# We use logistic regression to find characteristics of persons with SCI that are associated with a 
# higher propensity to visit specialized SCI clinics. Furthermore, we construct baseline characteristics 
# tables to present the characteristics of these persons.

source(file.path("code", "15_explore_cases.R"), encoding = "utf-8", local = TRUE, echo = TRUE)



