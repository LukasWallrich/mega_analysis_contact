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

dataset_name <- "hon_2021b"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

# Fix types
data <- data %>% 
  mutate(age = as.numeric(age))

miss_summary <- miss_var_summary(data)

if(sum(miss_summary$n_miss) == 0 ) {
    log("MISSING DATA: ", "All cases were complete")
} else {
    log("MISSING DATA: ", 
    glue::glue("Variables had up to {round(max(miss_summary$pct_miss),2)}% missing data"))
}

# Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "weight", "ethn_black", "attention")

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()
# data %>% select(-all_of(outlist), -where(is.numeric)) %>% summarise(across(everything(), print_levels)) %>% glimpse()

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

# Contact measure
calculate_share <- function(category) {
  out <- data[[paste0(category, "_black")]]/rowSums(data %>% select(starts_with(paste0(category, "_"))))
  out[rowSums(data %>% select(starts_with(paste0(category, "_")))) == 0] <- 0
  out
}
categories <- c("friends", "family_friends", "affiliates", "professionals", "relatives", "doctors", "social_media", "neighbors")

shares <- map_dfc(categories, ~tibble(!!(paste0(.x, "_share")) := calculate_share(.x)))

data <- data %>% select(-starts_with(paste0(categories, "_"))) %>% bind_cols(shares)

data <- data %>% mutate(
  contact_original = rowMeans(data %>% select(ends_with("_share"))),
  contact_no_social_media = rowMeans(data %>% select(ends_with("_share") & !starts_with("social_media")))
) %>% select(-ends_with("_share"))

##################################################################
##                   4. Recode vars                             ##
##################################################################

# Recode variables
# add more than gender
data <- data %>% mutate(
  gender = fct_collapse(as_factor(gender), 
    male = "1", female = "2",
    other_level = "other/not reported"),
  attitude_blm_support = case_when(attitude_blm_support == 1 ~ 1, attitude_blm_support == 2 ~ 0),
  attention = ifelse(attention == 3, "pass", "fail"),
  not_black = !ethn == 1,
  white =  ethn == 4
) %>% select(-ethn)

##################################################################
##               5. Reproduce descriptives                      ##
##################################################################

data %>% 
  filter(.imp == "0") %>%
  filter(white, attention == "pass") %>% #as per original article
  select(!starts_with("CMA_"), -weight, -.id, -.imp) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

data %>% 
  filter(.imp != "0") %>%
  filter(white, attention == "pass") %>% #as per original article
  select(!starts_with("CMA_"), , -.id) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_generated_MI (N = {.$n[1,2]}).html")))

##################################################################
##                 6. Create output files                       ##
##################################################################


data <- data %>% mutate(filter = case_when(white ~ "high_status",
                                           not_black ~ "not_outgroup")) %>%
  filter(!is.na(filter)) %>%
  select(-contact_original, contact_share = contact_no_social_media, -white, -not_black,
    attitude_civic = attitude_blm_support, attitude_behavior = attitude_blm_action)

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)
