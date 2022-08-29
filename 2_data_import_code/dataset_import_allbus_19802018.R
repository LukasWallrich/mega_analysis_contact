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

dataset_name <- "allbus_19802018"

data <- read_and_augment(dataset_name)

##################################################################
##                   2. Check missing data                      ##
##################################################################

# Check types and missingness codes
# data %>% select(-all_of(outlist)) %>% summarise(across(where(is.numeric), range_)) %>% glimpse()
#  

# Fix types
data <- data %>% 
  mutate(across(c(german, gender), as_factor),
  german = if_else(german != "CANNOT BE GENERATED", german, factor(NA)) %>% droplevels()) %>%
  # remove foreigners post 1994 since they were not asked about contact
   filter(collection_year < 1994 | is.na(german) | german != "NO")

# Remove years when contact was not measured
relevant_years <- data %>%
    select(collection_year, contact1) %>%
    group_by(collection_year) %>%
    miss_var_summary() %>%
    filter(pct_miss != 100) %>%
    pull(collection_year)

data <- data %>% filter(collection_year %in% relevant_years)

# Assess missing data

miss_summary <- data %>% select(-weight, -starts_with("CMA_")) %>% 
    group_by(collection_year) %>% miss_var_summary() 


# Drop variable-year combinations that are all missing
variables <- read_csv(here("0_metadata/variables.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

variables <- map_dfr(relevant_years, function(year) {
    variables %>% mutate(collection_year = year)
}) %>% anti_join(miss_summary %>% rename(var = variable) %>% filter(pct_miss == 100))

# Assess years separately

variables_years <- split(variables, variables$collection_year)
data_years <- split(data, data$collection_year)

data_years <- map2(data_years, names(data_years), function(df, year) {
    df %>% select(all_of(variables_years[[as.character(year)]]$var), starts_with("CMA_"))
})

# Remove those with all contact items missing in 1990 and 2000 - split ballots

data_years[["1990"]] <- data_years[["1990"]] %>% 
    filter(rowSums(across(starts_with("contact"), is.na)) != 4)

data_years[["2000"]] <- data_years[["2000"]] %>% 
    filter(rowSums(across(starts_with("contact"), is.na)) != 4)

# Report missing

log("TESTING MISSINGNESS after removing respondents in 1990 and 2000 who were in ballot without contact questions")

walk(relevant_years, function(year) {
    miss_summary <- data_years[[as.character(year)]] %>% miss_var_summary()
    log(glue::glue("Year {year}:"))
    if (sum(miss_summary$n_miss) == 0) {
        log("MISSING DATA: ", "All cases were complete")
    } else {
        log(
            "MISSING DATA: ",
            glue::glue("Variables had up to {round_(max(miss_summary$pct_miss), 2)}% missing data")
        )
    }
})

## IF IMPUTING, otherwise delete:
# Prepare for imputation
outlist <- c(str_subset(names(data), "CMA.*"), "collection_year")

# Check if any remaining variables are constant or linearly dependent
# If so, add to outlist
walk(relevant_years, function(year) {
    ini <- mice(data_years[[as.character(year)]], maxit = 0)
    print(ini$loggedEvents %>% filter(!out %in% outlist))
})

#Use quickpred extension that considers unordered factors correctly
source("https://raw.githubusercontent.com/LukasWallrich/rNuggets/0a15ae9c9fc163687eb9f0ad25f899ee370eb4d6/R/mice_quickpred_extension.R")

pred <- relevant_years %>% set_names() %>% map(function(year) {
    data_years[[as.character(year)]] %>% quickpred_ext(exclude = outlist)
})

library(furrr)
future::plan("multisession", workers = 6)

#impute with mice
data_imp <- future_map(.options = furrr_options(seed = 300688),
                        relevant_years %>% set_names(), function(year) {
    #message(glue::glue("Imputing year {year}"))
        data <- data_years[[as.character(year)]] %>% zap_labels() %>%
            mutate(across(where(is.factor), droplevels))
        preds <- pred[[as.character(year)]]
        mice(data, pred = preds, maxit = 50, m = 10) %>%
            complete(action = "long", include = TRUE)
})

##################################################################
##                   3. Create scales                           ##
##################################################################

data <- map(relevant_years %>% set_names(), function(year) { #(.options = furrr_options(seed = 300688)
        message(glue::glue("Scale creation for {year}"))
  
         data_imp[[as.character(year)]] %>%
            create_scales(variables_years[[as.character(year)]])
            })

# Reverse attitudes so that greater = more positive
# Done by reversing (_rev) in variables.csv
# Check through political_orientation (greater: right-wing)

map_dfr(relevant_years, function(year) {
    if (!"political_orientation" %in% names(data[[as.character(year)]])) return(tibble())
    data[[as.character(year)]] %>% select(starts_with("attitudes_"), political_orientation) %>% cor_matrix() %>% .[[1]] %>% .[, "political_orientation"] %>% t() %>% data.frame(
    ) %>% mutate(year = year)
})


##################################################################
##                   3. Recode vars                             ##
##################################################################

# Recode variables
# add more than gender
data <- map(data, ~mutate(.x,
  gender = fct_collapse(as_factor(gender), 
    male = "MALE", female = "FEMALE",
    other_level = "other/not reported")
))

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

walk(relevant_years, function(year) {


data[[as.character(year)]] %>% 
  filter(.imp == "0") %>%
  select(!starts_with("CMA_"), -weight, -.id, -.imp, , -collection_year) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix() %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_{year}_cor_table_generated_pairwise_del (N = {range_(.$n, 0, TRUE)}).html")))

data[[as.character(year)]] %>% 
  filter(.imp != "0") %>%
  select(!starts_with("CMA_"), -.id, -collection_year) %>%
  select(all_of(sort(colnames(.)))) %>%
  mutate(gender = (gender == "female") %>% as.numeric()) %>%
  cor_matrix_mi(weights = weight) %>%
  report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/{dataset_name}_{year}_cor_table_generated_MI (N = {.$n[1,2]}).html")))

})

##################################################################
##                 5. Create output files                       ##
##################################################################

# Create filter variable to remove outgroup members

data_both <- map(data, function(data) {

    data <- data %>% mutate(filter = case_when(german == "YES, EXCLUSIVELY" ~ "high_status",
                                           !is.na(german) ~ "not_outgroup"),
                                           CMA_year = collection_year) %>%
  filter(!is.na(filter)) %>%
  select(everything(), -collection_year)


# Convert to long data
   data_long <- data %>% make_long()

   list(long = data_long, wide = data)
}) %>% transpose()

data_both <- map(data_both, bind_rows)

# Save aligned data
save_data(data_both$wide, data_both$long, dataset_name)

message("Done processing ", dataset_name)

# Reset log - before sourcing completed file
# reset_log()