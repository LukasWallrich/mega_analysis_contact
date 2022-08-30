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

dataset_name <- "wal_2021"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

data <- filter(data, ethnicity %in% c(10, 11, 12)) %>%
    mutate(ethnicity = factor(ethnicity), gender = factor(gender)) # White respondents only (others have no outcome data)

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ",
    glue::glue("Variables had up to {max(miss_summary$pct_miss)} missing data"))
}

#Filter those without any contact responses
contact_responses <- data %>% select(matches("ExpBl")) %>%
    mutate(across(everything(), ~!is.na(.x))) %>% rowSums()

log(glue::glue("Dropping {sum(contact_responses == 0)} cases without any contact responses"))

data <- data[contact_responses != 0,]

#Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "weight")

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
data_imp <- mice(data, pred = pred, maxit = 50, m = 10, seed = 300688, printFlag = FALSE) #cluster.seed = 300688, printFlag = FALSE, n.core = 5, n.imp.core = 2)

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

# Article used fiml for missing data - so minor deviations to be expected here

data %>%
  filter(.imp == "0") %>%
  filter(BrCitizen %in% c(1, 2)) %>% #as per original article
  select(!starts_with("CMA_"), -weight, -.id, -.imp, -BrCitizen) %>%
  select(all_of(sort(colnames(.)))) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

data %>%
  filter(.imp != "0") %>%
  filter(BrCitizen %in% c(1, 2)) %>% #as per original article
  select(!starts_with("CMA_"), -.id, -BrCitizen) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_MI (N = {.$n[1,2]}).html")))


##################################################################
##                 5. Create output files                       ##
##################################################################

data <- data %>% mutate(filter = if_else(BrCitizen == 1, "high_status", "not_outgroup")) %>%
  filter(!is.na(filter)) %>% select(-BrCitizen)

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)
