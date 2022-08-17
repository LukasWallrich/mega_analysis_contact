# Load required packages
library(tidyverse)
library(here)
library(haven)
library(naniar)
#remotes::install_github("lukaswallrich/timesaveR")
library(timesaveR)
library(mice)

source(here("dataset_prep_functions.R"))

##################################################################
##                   1. Read and prepare data                   ##
##################################################################

dataset_name <- "_____"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()
# data %>% select(-all_of(outlist), -where(is.numeric)) %>% summarise(across(everything(), print_levels)) %>% glimpse()

# Fix types
data <- data %>% 
  mutate(_____)

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0 ) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ", 
    glue::glue("Variables had up to {max(miss_summary$pct_miss)} missing data")
}

## Identify whether missingness was planned. If so, log. 
#Otherwise, impute and log that

log("___")

## IF IMPUTING, otherwise delete:
# Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "weight")

# Check if any remaining variables are constant or linearly dependent
# If so, add to outlist
ini <- mice(data, maxit = 0)
ini$loggedEvents %>% filter(!out %in% outlist) 

#Use quickpred extension that considers unordered factors correctly
source("https://raw.githubusercontent.com/LukasWallrich/rNuggets/5dc76f1998ca35b07a0434c5c6b19d4812147daa/R/mice_quickpred_extension.R")

pred <- quickpred_ext(data, exclude = outlist)

#impute with mice
data_imp <- parlmice(data, pred = pred, maxit = 50, m = 10, cluster.seed = 300688, printFlag = FALSE, n.core = 5, n.imp.core = 2)

data <- complete(data_imp, action = "long", include = TRUE)

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- create_scales(data)

# Manual for more complicated scales

##################################################################
##                   3. Recode vars                             ##
##################################################################

# Recode variables
# add more than gender
data <- data %>% mutate(
  gender = fct_collapse(as_factor(gender), 
    male = "1", female = "2",
    other_level = "other/not reported")
)

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

data %>% 
  filter(.imp == "0") %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -weight) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("data_processed/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

data %>% 
  filter(.imp != "0") %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -.id) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("data_processed/{dataset_name}_cor_table_generated_MI (N = {.$n[1,2]}).html")))

##################################################################
##                 5. Create output files                       ##
##################################################################

# Create filter variable to remove outgroup members
# Drop outgroup members and unneeded vars, rename contact & attitudes
data <- data %>% mutate(filter = case_when(white ~ "high_status",
                                           not_black ~ "not_outgroup")) %>%
  filter(!is.na(filter)) %>%
  select(everything())

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, metadata$dataset)

message("Done processing ", dataset_name)