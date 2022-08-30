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

dataset_name <- "fir_2022"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0 ) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ",
    glue::glue("Variables had up to {max(miss_summary$pct_miss)} missing data"))
}

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- create_scales(data)

##################################################################
##                   4. Recode vars                             ##
##################################################################

# Recode variables
# add more than gender
data <- data %>% mutate(
  gender = fct_collapse(as_factor(gender),
    male = "-1", female = "1",
    other_level = "other/not reported"),
    CMA_outgroup_specific = if_else(target == -1, "Kurdish", "Syrian")
) %>% select(-target)

##################################################################
##               5. Reproduce descriptives                      ##
##################################################################

data %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -weight, CMA_outgroup_specific) %>%
  select(all_of(sort(colnames(.)))) %>%
  nest_by(CMA_outgroup_specific) %>%
  mutate(cors = list(cor_matrix(data)), n = nrow(data)) %>%
  summarise(report_cor_table(cors, filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_rating {CMA_outgroup_specific} (N = {n}).html")))) %>%
  invisible()


data %>%
  select(!starts_with("CMA_"), -weight) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_no_missing_data (N = {range_(.$n, 0, TRUE)}).html")))

##################################################################
##                 5. Create output files                       ##
##################################################################

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)