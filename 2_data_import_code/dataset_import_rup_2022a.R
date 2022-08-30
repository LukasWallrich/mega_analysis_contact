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

dataset_name <- "rup_2022a"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()
# data %>% select(-all_of(outlist), -where(is.numeric)) %>% summarise(across(everything(), print_levels)) %>% glimpse()

data <- data %>% mutate(gender = case_when(gender == 1 ~ "male",
                                            gender == 2 ~ "female",
                                            TRUE ~ "other/not reported") %>% as_factor())

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0 ) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ",
    glue::glue("Variables had up to {round_(max(miss_summary$pct_miss), 2)}% missing data"))
}

# Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "weight")
data <- haven::zap_labels(data)

# Check if any remaining variables are constant or linearly dependent
# If so, add to outlist
ini <- mice(data, maxit = 0)
ini$loggedEvents %>% filter(!out %in% outlist)

#Use quickpred extension that considers unordered factors correctly
source("https://raw.githubusercontent.com/LukasWallrich/rNuggets/5dc76f1998ca35b07a0434c5c6b19d4812147daa/R/mice_quickpred_extension.R")

pred <- quickpred_ext(data, exclude = outlist)

#impute with mice
data_imp <- mice(data, pred = pred, maxit = 50, m = 10, seed = 300688, printFlag = FALSE) #cluster.seed = 300688, printFlag = FALSE, n.core = 5, n.imp.core = 2)

data <- complete(data_imp, action = "long", include = TRUE)

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- create_scales(data)

##################################################################
##                   3. Recode vars                             ##
##################################################################

# None needed

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

data %>%
  filter(.imp == "0") %>%
  filter(TRUE) %>% #original article likely used listwise deletion - not reported, so ignored here
  select(!starts_with("CMA_"), -weight, -.id, -.imp) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

data %>%
  filter(.imp != "0") %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -.id) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_MI (N = {.$n[1,2]}).html")))

##################################################################
##                 5. Create output files                       ##
##################################################################

# Create filter variable - no outgroup members in sample

data <- data %>% mutate(filter = "not_outgroup",
                        attitude_thermometer_serbs = attitude_feelings_serbs - attitude_feelings_bosniaks,
                        attitude_thermometer_croats = attitude_feelings_croats - attitude_feelings_bosniaks
                                           ) %>%
  filter(!is.na(filter)) %>%
  select(-matches("_feelings_"))

#Separate two outgroups
data1 <- data %>% select(!ends_with("_croats")) %>% mutate(CMA_outgroup_specific = "Serbs") %>% rename_with(~str_remove(., "_serbs"))
data2 <- data %>% select(!ends_with("_serbs")) %>% mutate(CMA_outgroup_specific = "Croats")  %>% rename_with(~str_remove(., "_croats"))
data <- rbind(data1, data2)

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)