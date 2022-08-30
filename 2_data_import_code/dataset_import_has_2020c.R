# Suppress package startup messages
library <- function(...) suppressPackageStartupMessages(base::library(...))

# Load required packages
library(tidyverse)
library(here)
library(haven)
library(naniar)
#remotes::install_github("lukaswallrich/timesaveR")
library(timesaveR)
library(mice)

source(here("0_code_helpers", "dataset_prep_functions.R"))

##################################################################
##                   1. Read and prepare data                   ##
##################################################################

dataset_name <- "has_2020c"

data <- read_and_augment(dataset_name, delim = ";", locale = locale(decimal_mark = ","))

##################################################################
##                   2. Check missing data                      ##
##################################################################

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0 ) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ",
    glue::glue("Variables had up to {round(max(miss_summary$pct_miss),2)}% missing data"))
}

log("Missing predominantly contact measures - possibly mostly planned missingness, since contact_frequency is always given. Due to residualised data, this cannot be assessed directly. Only max 0.06% missing data on attitude measures.

Paper is running CFA with listwise deletion - highly problematic with planned missingness.

Here, we don't impute contact measures for cases with all contact measures missing (apart from frequency); not imputing contact_friends_frequency, not using it for imputations.
")

data <- data %>% mutate(share_contact_na = rowMeans(across(c(starts_with("contact_"), -contact_frequency), is.na)))

# Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "weight", "contact_friends_frequency", "share_contact_na")

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()

# Check if any remaining variables are constant or linearly dependent
# If so, add to outlist
ini <- mice(data, maxit = 0)
ini$loggedEvents %>% filter(!out %in% outlist)

#Use quickpred extension that considers unordered factors correctly - from https://raw.githubusercontent.com/LukasWallrich/rNuggets/5dc76f1998ca35b07a0434c5c6b19d4812147daa/R/mice_quickpred_extension.R

source(here("0_code_helpers", "mice_quickpred_extension.R"))

pred <- quickpred_ext(data, exclude = outlist)

#impute with mice
message("Starting imputation")
data_imp <- mice(data, pred = pred, maxit = 50, m = 10, seed = 300688, printFlag = FALSE)#cluster.seed = 300688, printFlag = FALSE, n.core = 5, n.imp.core = 2)

data <- complete(data_imp, action = "long", include = TRUE)

data <- data %>%
  mutate(across(c(starts_with("contact_"), -contact_frequency),
           ~if_else(share_contact_na != 1, .x, NA_real_)))

data <- data %>% group_by(.id) %>%
  dplyr::mutate(contact_friends_frequency =
    ifelse(is.na(first(contact_friends_frequency)), NA_real_, contact_friends_frequency)) %>%
    ungroup()

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- create_scales(data)

##################################################################
##                   4. Recode vars                             ##
##################################################################

# Recode variables - revert negative contact,
# as already recoded in dataset

data <- data %>% mutate(
 contact_negative = -1 * contact_negative) %>%
 select(-share_contact_na)

##################################################################
##               5. Reproduce descriptives                      ##
##################################################################

data %>%
  filter(.imp == "0") %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -weight, -.id, -.imp) %>%
  select(all_of(sort(colnames(.)))) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

log("Attention check not documented and thus ignored. According to article, negligible impact.")

data %>%
  filter(.imp != "0") %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -.id) %>%
  select(all_of(sort(colnames(.)))) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_MI (N = {.$n[1,2]}).html")))


##################################################################
##                 6. Create output files                       ##
##################################################################

data <- data %>% mutate(filter = "high_status") %>%
  filter(!is.na(filter))

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)

# Reset log - before sourcing completed file
# reset_log()