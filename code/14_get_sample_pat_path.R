
## Get a sample of patients to study patient pathways in more detail

library(tidyverse)
library(openxlsx)

load(file = file.path(".", "workspace", "cases_pathways.RData"))


## Get a sample of patients ----------------------------------------------------

sample_size = 100

# set.seed(42)

pat_id_sample <- sample(unique(cases$pat_id), sample_size)
pat_sample <- filter(cases, pat_id %in% pat_id_sample)


## Code hospitalizations of patients alternating with 0 and 1 to make them better distinguishable ------

pat_sample <- pat_sample %>% 
  
  mutate(
    
    new_pat = lag(pat_id, n = 1),
    new_pat = if_else(pat_id == new_pat, 0, 1),
    new_pat = na_if(new_pat , 0),
    new_pat = replace(new_pat, 1, 1)
    
  )

every_2nd_pat <- seq(1, sample_size, 2)

pat_sample[!is.na(pat_sample$new_pat), ]$new_pat[every_2nd_pat] <- 0


# Fill new pat numbers of patients according to the one (1/0) that was assigned

pat_sample <- fill(pat_sample, new_pat)


## Prepare dataset to save as excel (xlsx file) -----------------------------------------------

pat_sample <- pat_sample %>% 
  
  select(new_pat, pat_id, age, sex, cause, lesion_level, completeness, 
         type_2, LOS_OECD, admission_pre, admission_mode, discharge_post_treat, discharge_place, 
         pd_title_full, pd_title_chapter, SCI_codes_all, SCI_coding_cred_score, pat_path, 
         -c(type, BUR, seq_no_corr, d_p_treat, acute_chron)) %>% 
  
  rename(coding_qual = SCI_coding_cred_score) %>% 
  
  mutate(coding_qual = round(coding_qual, 1))


# Get dimensions of dataset

my_rows <- 1:nrow(pat_sample)
my_cols <- 1:ncol(pat_sample)


# Define column width text in columns

col_width_col_text <- pat_sample %>% 
  mutate_all(as.character) %>% 
  lapply(function(x) max(nchar(x), na.rm = TRUE)) %>% 
  unlist %>% 
  {ifelse(.>30, ./3, .)} %>% 
  round(0) %>% 
  `+` (5)


col_width_col_names <- colnames(pat_sample) %>% 
  nchar %>%
  `+` (3)

longer_title_cols <- col_width_col_text < col_width_col_names

col_width_col_text[longer_title_cols] <- col_width_col_names[longer_title_cols]


# To define further down the column where the text will be wrapped

# ...columns that a have text which is longer than 30 charactersters
# ...additional columns

wrap_text <- which(col_width_col_text > 30)

manully_warp <- which(names(pat_sample) %in% c("SCI_codes_all"))
wrap_text <- c(wrap_text, manully_warp)


# Save as format excel table

wb <- createWorkbook("pat_path_sample")

addWorksheet(wb, "Sheet 1")

writeData(wb, 1, pat_sample)


# Color hospitalizations of alternating patients with white and yellow ("wheat2") 

conditionalFormatting(wb, 
                      sheet = 1, 
                      rows = my_rows, 
                      cols = my_cols, 
                      rule = "$A1 == 1", 
                      style = createStyle(fontColour = "black", bgFill = "wheat2"))


# Freeze top row (column names)

freezePane(wb, sheet = 1, firstRow = TRUE)


# Define columns where text will be wrapped

addStyle(wb, sheet = 1, 
         createStyle(wrapText = TRUE), 
         rows = my_rows, 
         cols = wrap_text, gridExpand = TRUE)


# Define header style

header_style <- createStyle(fontSize = 12, 
                            fontColour = "#FFFFFF", 
                            halign = "center",
                            fgFill = "#4F81BD", 
                            border = "TopBottom", 
                            borderColour = "#4F81BD")


# Apply header style

addStyle(wb, sheet = 1, 
         header_style, 
         rows = 1, 
         cols = my_cols, 
         gridExpand = TRUE)


# Define width of columns

setColWidths(wb, 
             sheet = 1, 
             cols = my_cols, 
             widths = col_width_col_text)


# Save excel file

suppressWarnings(
  
  saveWorkbook(wb, 
               file = file.path("output", "pat_path_sample.xlsx"), 
               overwrite = TRUE)
  
)


# Clear workspace

rm("cases", "col_width_col_names", "col_width_col_text", "every_2nd_pat", 
   "header_style", "longer_title_cols", "manully_warp", "my_cols", 
   "my_rows", "pat_id_sample", "pat_sample", "sample_size", "wb", 
   "wrap_text")
