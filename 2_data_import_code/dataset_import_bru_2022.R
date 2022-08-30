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

dataset_name <- "bru_2022"

data <- read_and_augment(dataset_name)

#Data contains repeated measures for two outgroups, depening on ingroup
data <- data %>% mutate(ingroup = case_when(ethnicity == 1 ~ "white",
                                            ethnicity == 2 ~ "muslim", #Identified by nationality in article - so categorised ethnicity
                                            ethnicity == 3 ~ "black")) %>%
            make_long() %>% separate(contact_type_detail, c("contact_type_detail", "contact_target")) %>%
            rename(attitude_target = attitude_type_detail) %>%
            filter(attitude_target == contact_target) %>%
            filter(attitude_target != ingroup) %>%
            mutate(CMA_outgroup_specific = attitude_target) %>%
            select(-starts_with("contact_"), -starts_with("attitude_")) %>%
            rename(contact_friends_frequency = contact, attitude_rejection = attitude)

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

##################################################################
##                   3. Create scales                           ##
##################################################################

# Already created in dataset provided by authors

##################################################################
##                   4. Recode vars                             ##
##################################################################

#Reverse attitude so that higher score -> less rejection
data <- data %>% mutate(attitude_rejection = reverse_code(attitude_rejection))

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

# Separate correlation matrices per ingroup and outgroup

data %>%
  filter(TRUE) %>% #as per original article
  select(!starts_with("CMA_"), -weight, CMA_outgroup_specific) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(ethnicity = as_factor(ethnicity)) %>%
  nest_by(ethnicity, CMA_outgroup_specific) %>%
  mutate(cors = list(cor_matrix(data)), n = nrow(data)) %>%
  summarise(report_cor_table(cors, filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_{ethnicity} rating {CMA_outgroup_specific} (N = {n}).html")))) %>%
  invisible()


##################################################################
##                 5. Create output files                       ##
##################################################################

data <- data %>% mutate(filter = if_else(ethnicity == 1, "high_status", "not_outgroup")) %>%
  filter(!is.na(filter)) %>% select(-ethnicity) %>%
  mutate(CMA_subgroup = ingroup)

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)
