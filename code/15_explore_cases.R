
library(tidyverse)
library(effects)
library(lmtest)
library(openxlsx)
library(tableone)

rbindlist <- data.table::rbindlist	

load(file = file.path(".", "workspace", "cases_pathways.RData"))


# Prepare dataset for regression ------------------------------------------------------

cases <- cases %>% 
  
  mutate(SCI_cent_hosp = if_else(cases$type_2 == "center_SCI", 1, 0)) %>% 
  
  group_by(pat_id) %>% 
  
  summarize(SCI_cent_pat = sum(SCI_cent_hosp)) %>% 
  
  mutate(SCI_cent_pat = if_else(SCI_cent_pat >= 1, 1, 0)) %>% 
  
  right_join(cases, by = "pat_id") %>% 
  
  select(-SCI_cent_pat, everything()) %>% 
  
  mutate(lesion_level_d = fct_relevel(lesion_level_d, "Cauda", "S2_S5", 
                                      "L2_S1", "T11_L1", "T7_T10", "T1_T6", 
                                      "C6_C8", "C4_C5", "C1_C3")) %>% 
  
  mutate(dis_year = as.factor(dis_year)) %>% 
  
  mutate(age_cat = cut(age, 
                       breaks = c(0, 30, 45, 60, 75, Inf), 
                       labels = c("16_30", "31_45", "46_60", "61_75", "75_plus"))
  ) %>% 
  
  mutate_if(is.character, as.factor)



# Get dataset with patients only ---------------------------------------------------------------

cases_pat <- cases %>% distinct(pat_id, .keep_all = TRUE) 
cases_pat_12 <- cases %>% filter(dis_year == 2012) %>% distinct(pat_id, .keep_all = TRUE) 
cases_pat_13 <- cases %>% filter(dis_year == 2013) %>% distinct(pat_id, .keep_all = TRUE) 


# Find missings in lesion characteristics -------------------------------------------

# Year 2012 (dataset reduced to one hospitalization per patient)

missings_12 <- sapply(cases_pat_12, function(x) sum(is.na(x)))

nrow(cases_pat_12); missings_12[c("cause", "lesion_level_d", "completeness")]


# year 2013 (dataset reduced to one hospitalization per patient)

missings_13 <- sapply(cases_pat_13, function(x) sum(is.na(x)))

nrow(cases_pat_13); missings_13[c("cause", "lesion_level_d", "completeness")]



# Function that extracts regression estimates and calculates margins -----------------------------

get_marginals <- function(fit, digits = 0) {
  
  # Extract estimates and computes margines
  
  marginals <- allEffects(fit)
  
  extract_marginals <- function(x) as.data.frame(marginals[[x]])
  
  var_names <- names(marginals)
  
  marginals_l <- lapply(var_names, extract_marginals)
  
  cat_per_var <- sapply(marginals_l, nrow)
  
  
  marginals_df <- as_tibble(rbindlist(marginals_l, use.names = FALSE)) %>% 
    
    mutate(
      
      variable = rep(var_names, cat_per_var),
      variable = if_else(duplicated(variable), "", variable)
      
    ) %>% 
    
    mutate_if(is.factor, as.character) %>% 
    
    rename(categories = var_names[1], estimate = fit)
  
  
  # Calculates and adds p values (using likelihood ratio test)
  
  pval_lrtest <- function(var_names, fit) {
    
    lrtest(fit, var_names)$"Pr(>Chisq)"[2]
    
  }
  
  p_vals <- unlist(lapply(var_names, pval_lrtest, fit = fit))
  names(p_vals) <- names(fit$xlevels)
  p_vals <- formatC(p_vals, 3, format = "f")
  
  marginals_df$p_vals <- NA
  
  marginals_df[!is.na(match(marginals_df$variable, names(p_vals))), "p_vals"] <- p_vals
  
  marginals_df$p_vals <- str_replace_na(marginals_df$p_vals, "")
  
  
  # Dataset tidying
  
  marginals_df <- marginals_df %>% 
    
    select(variable, everything()) %>% 
    
    mutate_at(vars(estimate, se, lower, upper), list( ~ round(. * 100, digits)))
  
  format_CI <- function(my_col) formatC(marginals_df[[my_col]], digits, format = "f")
  
  estim <- format_CI("estimate")
  lower_L <- format_CI("lower")
  upper_L <- format_CI("upper")
  
  CI_95 <- paste0(estim, " (", lower_L, " \u2013 ", upper_L, ")")
  
  visit_spec_sent <- mutate(marginals_df, CI_95 = CI_95) %>% 
    
    select(variable, categories, CI_95, p_vals)
  
  
  return(visit_spec_sent)
  
}



## Identfiy predictors of visiting a specialized SCI center using logistic regression -----------------

outcome_var <- "SCI_cent_pat"

reg_formula <- function(x) {as.formula(paste(outcome_var, paste(x, collapse = " + "), sep = " ~ "))}

my_tables <- list()


# Year 2012

explanatory_vars <- c("age_cat", "sex", "cause", "lesion_level")

fit_2012 <- glm(reg_formula(explanatory_vars), family = "binomial", data = cases_pat_12)
reg_res_2012 <- get_marginals(fit_2012, digits = 0) 

cat("Strangely there are much more men hospitalized in SCI centers than women")


# Year 2013

fit_2013 <- glm(reg_formula(explanatory_vars), family = "binomial", data = cases_pat_13)
reg_res_2013 <- get_marginals(fit_2013, digits = 0)

cat("Strangely there are much more men hospitalized in SCI centers than women")



# Year 2012 and 2013

explanatory_vars <- c(explanatory_vars, "dis_year")

fit_all <- glm(reg_formula(explanatory_vars), family = "binomial", data = cases_pat)
reg_res_all <- get_marginals(fit_all, digits = 0) 

cat("Strangely there are huge differences in the propensity to be hospitalized in SCI centers 
    between the years 2012 and 2013 and betwween men and women")



# Including completeness (with many missings) --------------

explanatory_vars <- c(explanatory_vars, "completeness")

cases_pat_na_dropped <- cases_pat %>% drop_na(explanatory_vars)

fit_compl <- glm(reg_formula(explanatory_vars), family = "binomial", data = cases_pat_na_dropped)

cat(nrow(cases_pat) - nrow(cases_pat_na_dropped), "of", nrow(cases_pat),"observations deleted due to missingness")

reg_res_compl <- get_marginals(fit_compl, digits = 0)

# -> missings had to be removed, otherwise lrtest does not work: 
# -> "models could not be fitted to the same size of dataset"



## Logistic regressions including completeness (with many missings) --------------

explanatory_vars <- setdiff(c(explanatory_vars, "lesion_level_d"), "lesion_level")

cases_pat_na_dropped <- cases_pat %>% drop_na(explanatory_vars)

fit_compl_d_ll <- glm(reg_formula(explanatory_vars), family = "binomial", data = cases_pat_na_dropped)

cat(nrow(cases_pat) - nrow(cases_pat_na_dropped), "of", nrow(cases_pat),"observations deleted due to missingness")

reg_res_compl_d_ll <- get_marginals(fit_compl_d_ll, digits = 0)

# -> missings had to be removed, otherwise lrtest does not work: 
# -> "models could not be fitted to the same size of dataset"


## Clear workspace -------------------------------------------------------------------------

rm(list = str_subset(ls(), "fit_|reg_res", negate = TRUE))


## Write results to excel file ---------------------------------------------------------------------------------------------


wb <- createWorkbook("reg_results")

sheet_names <- str_subset(ls(), "reg_res") %>% 
  str_replace("reg_", "")

reg_tables <- str_subset(ls(), "reg_res") %>% 
  lapply(function(x) eval(parse(text = x))) %>% 
  set_names(sheet_names)


# Add worksheets

invisible(
  lapply(sheet_names, function(x) { addWorksheet(wb, x) })
)


# Fill worksheets

invisible(
  mapply( function(sheet, x) {writeData(wb, sheet, x)}, sheet = sheet_names, x = reg_tables)
)

# Define column width 

col_width <- reg_tables %>% 
  rapply(str_length, how = "replace") %>% 
  rapply(max, how = "list") %>% 
  rapply(function(x) x + 3, how = "list")


invisible(
  
  lapply(seq_along(col_width), function(i) { 
    
    setColWidths(wb, 
                 sheet = i, 
                 cols = 1:length(col_width[[i]]), 
                 widths = col_width[[i]]) 
  })
  
)



# Write table

suppressWarnings(
  
  saveWorkbook(wb, file = file.path("output", "results_regression.xlsx"), overwrite = TRUE)
  
)

# Note: zip::zip() is deprecated, please use zip::zipr() instead


## clear workspace

rm(list = str_subset(ls(), "fit_", negate = TRUE))


# Get table descriptive tables -----------------------------------------------------------


# Get models

my_model_names <- str_subset(ls(), "fit_")

my_models <- my_model_names %>% 
  lapply(function(x) { eval(parse(text = x)) }) %>% 
  set_names(my_model_names)


# Get explanatory variables

my_vars <- my_models %>% 
  lapply(function(glm) { attributes(glm[["terms"]])["term.labels"] }) %>% 
  sapply(unlist) %>% 
  unname


# Get model data

my_data <- mapply(
  FUN = function(glm, my_vars) { glm[["data"]] %>% select(one_of(my_vars)) }, 
  glm = my_models, 
  my_vars = my_vars
)


# Construct table ones

my_table_ones <- list()



for(i in 1:length(my_models)) {
  
  my_table_ones[[i]] <- CreateTableOne(vars = my_vars[[i]], data = my_data[[i]]) %>% 
    print(printToggle = FALSE) %>% 
    as.data.frame %>% 
    rownames_to_column
  
  colnames(my_table_ones[[i]])[1] <- paste0("model = ", names(my_data[i]))
  
}

names(my_table_ones) <- names(my_models)


# Define column width in table one

col_width <- my_table_ones %>% 
  rapply(str_length, how = "replace") %>% 
  rapply(max, how = "list")


# Write table ones to excel

wb <- createWorkbook("descriptive_tables")


# Add worksheets

invisible(
  
  lapply(names(my_table_ones), function(x) {addWorksheet(wb, x)})
  
)


# Write data into worksheets

invisible(
  
  mapply(FUN = function(sheet, x) {writeData(wb, sheet, x)}, 
         sheet = names(my_table_ones), 
         x = my_table_ones)
  
)


# Set column widths



invisible(
  
  lapply(seq_along(col_width), function(i) {
    
    setColWidths(wb, sheet = i, cols = 1:length(col_width[[i]]), widths = col_width[[i]]) 
    
  })
  
)


saveWorkbook(wb, file = file.path("output", "descriptive_tables.xlsx"), overwrite = TRUE)

rm(list = ls())
