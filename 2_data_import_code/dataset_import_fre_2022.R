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

dataset_name <- "fre_2022" ## https://osf.io/4ea9w/?view_only=d3d3c9cbefb8418ca8b493ba5495628d

  variables <- read_csv(here("0_metadata/variables.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

  metadata <- read_csv(here("0_metadata/metadata.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

    data <- read_sav(here("1_data_raw", metadata$dataset, metadata$filename))  #, col_select = all_of(variables$name))


eth_lookup <- data %>% {set_names(.$EthS, .$ID)}

all_friends <- data %>% select(starts_with("Frnd")) %>% unlist() %>% unique()

eth_lookup[as.character(setdiff(all_friends, names(eth_lookup)))] <- NA

data <- data %>% mutate(across(starts_with("Frnd"), ~if_else(!is.na(.x), eth_lookup[as.character(.x)] == 3 - EthS, NA))) %>%
    mutate(
        OutgroupFriends_W1 = rowSums(across(matches("Frnd.*W1")), na.rm = TRUE) * if_else(rowSums(across(matches("Frnd.*W1"), is.na)) == 10, NA_real_, 1),
        OutgroupFriends_W2 = rowSums(across(matches("Frnd.*W2")), na.rm = TRUE) * if_else(rowSums(across(matches("Frnd.*W2"), is.na)) == 10, NA_real_, 1),
        OutgroupFriends_W3 = rowSums(across(matches("Frnd.*W3")), na.rm = TRUE) * if_else(rowSums(across(matches("Frnd.*W3"), is.na)) == 10, NA_real_, 1),
        OutgroupFriends_W4 = rowSums(across(matches("Frnd.*W4")), na.rm = TRUE) * if_else(rowSums(across(matches("Frnd.*W4"), is.na)) == 10, NA_real_, 1),
        OutgroupFriends_W5 = rowSums(across(matches("Frnd.*W5")), na.rm = TRUE) * if_else(rowSums(across(matches("Frnd.*W5"), is.na)) == 10, NA_real_, 1),
    )

data <- data %>% filter(EthS %in% c(1:2)) %>%
    mutate(
      CMA_dataset = metadata$dataset,
      CMA_year = metadata$year,
      CMA_country = metadata$country,
      CMA_outgroup_category = metadata$outgroup_category,
      CMA_outgroup_specific = if_else(EthS == 1, "Asian", "White"),
      CMA_design = metadata$design,
      CMA_open = metadata$open,
      weight = 1,
      CMA_subgroup = if_else(EthS == 1, "White", "Asian"),
    ) %>%
    select(starts_with("CMA_"), weight, all_of(variables %>%
      {
        set_names(pull(., name), pull(., var))
      }))


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


#Prepare for imputation
outlist <- setdiff(c(str_subset(names(data), "CMA.*"), "weight"), "CMA_outgroup_specific")

data <- data %>% mutate(CMA_outgroup_specific = as_factor(CMA_outgroup_specific))

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()

# Check if any remaining variables are constant or linearly dependent
# If so, add to outlist
ini <- mice(data, maxit = 0)
ini$loggedEvents %>% filter(!out %in% outlist)

#Use quickpred extension that considers unordered factors correctly - from https://raw.githubusercontent.com/LukasWallrich/rNuggets/5dc76f1998ca35b07a0434c5c6b19d4812147daa/R/mice_quickpred_extension.R

source(here("0_code_helpers", "mice_quickpred_extension.R"))

pred <- quickpred_ext(data, exclude = outlist)

data <- data %>% zap_labels()

#impute with mice
message("Starting imputation")
data_imp <- mice(data, pred = pred, maxit = 50, m = 10, seed = 300688, printFlag = FALSE)#cluster.seed = 300688, printFlag = FALSE, n.core = 5, n.imp.core = 2)

data <- complete(data_imp, action = "long", include = TRUE)

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- data %>% pivot_longer(matches("__W[1-5]{1}$"), names_to = c(".value", "wave"),
            names_sep = "__")

#data <- create_scales(data)

##################################################################
##                   4. Recode vars                             ##
##################################################################

# Recode variables
# add more than gender
data <- data %>% mutate(
  gender = fct_collapse(as_factor(gender),
    male = "1", female = "2",
    other_level = "other/not reported")
)

##################################################################
##               5. Reproduce descriptives                      ##
##################################################################

data %>%
 filter(.imp == "0") %>%
  select(!starts_with("CMA_"), CMA_outgroup_specific, -weight, -.id, -.imp) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  nest_by(CMA_outgroup_specific) %>%
  mutate(cors = list(cor_matrix(data)), n = nrow(data)) %>%
  summarise(report_cor_table(cors, filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_pairwise_deletion_rating {CMA_outgroup_specific} (N = {n}).html")))) %>%
  invisible()

data %>%
 filter(.imp != "0") %>%
  select(!starts_with("CMA_"), CMA_outgroup_specific, -weight, -.id) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  nest_by(CMA_outgroup_specific) %>%
  mutate(cors = list(cor_matrix_mi(data)), n = nrow(data)) %>%
  summarise(report_cor_table(cors, filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_cor_table_MI_rating {CMA_outgroup_specific} (N = {n}).html")))) %>%
  invisible()

##################################################################
##                 5. Create output files                       ##
##################################################################

data <- data %>%
    mutate(CMA_partid = paste(CMA_dataset, CMA_partid, sep = "_"),
           CMA_outgroup_specific = as.character(CMA_outgroup_specific))

# Convert to long data
data_long <- data %>% make_long()

# Save aligned data
save_data(data, data_long, dataset_name)

message("Done processing ", dataset_name)