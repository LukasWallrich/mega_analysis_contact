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

dataset_name <- "dec_2019"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ",
    glue::glue("Variables had up to {max(miss_summary$pct_miss)} missing data"))
}

log("All missingness due to explicit 'don't know' options - thus no imputation performend")

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
    male = "Male", female = "Female",
    other_level = "other/not reported"),
  age = 2017 - year_birth - rbinom(1, 1, (1-9.5/12)), #Data collected in Sep/Oct 2017
  CMA_country = case_when(
    country == 1 ~ "Belgium",
    country == 2 ~ "Sweden",
    country == 3 ~ "France",
    country == 4 ~ "Netherlands"
  )
) %>% select(-year_birth)

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

data %>%
  filter(TRUE) %>%
  select(!starts_with("CMA_"), -weight, -country, -country_birth, -nationality) %>%
  select(all_of(sort(colnames(.)))) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

##################################################################
##                 5. Create output files                       ##
##################################################################

data <- data %>% mutate(filter = case_when(
    country == 1 & country_birth == 1 ~ "high-status", #Belgium
    country == 2 & country_birth == 4 ~ "high-status", #Sweden
    country == 3 & country_birth == 2 ~ "high-status", #France
    country == 4 & country_birth == 3 ~ "high-status",  #Netherlands
    country == 1 & nationality == 1 ~ "not_outgroup", #Belgium
    country == 2 & nationality == 4 ~ "not_outgroup", #Sweden
    country == 3 & nationality == 2 ~ "not_outgroup", #France
    country == 4 & nationality == 3 ~ "not_outgroup",  #Netherlands
    TRUE ~ NA_character_),
    CMA_subgroup = CMA_country) %>%
    filter(!is.na(filter)) %>%
    select(-country, -nationality, -country_birth)

# Convert to long data
data_long <- data %>% zap_labels() %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)
#reset_log()