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

#remotes::install_github("kjhealy/gssr")
library(gssr)

##################################################################
##                   1. Read and prepare data                   ##
##################################################################

dataset_name <- "GSS_19722021"

data(gss_all)
data(gss_doc)

which_years <- function(...) {
    gss_which_years(gss_all, ...) %>%
    filter(if_any(2:ncol(.), ~.x == TRUE))
}

#In some gssr variables, 98 and 99 should be NA (https://github.com/kjhealy/gssr/issues/8) - removed here.
remove_missings <- function(x) {
    if (97 %in% x) warning("97 found in in variable - are you sure 98 and 99 should be treated as NA?")
    x[x == 98] <- NA
    x[x == 99] <- NA
}


variables <- read_csv(here("0_metadata/variables.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name) %>%
    mutate(name = tolower(name), var = tolower(var))

# Basic vars 
basic_vars <- variables %>% filter(note == "all") %>% 
    pull(name) %>%
    tolower() 
helper_vars <- variables %>% filter(note == "helper") %>% 
    pull(name) %>%
    tolower() 

# Set metadata

metadata <- read_csv(here("0_metadata/metadata.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

  gss_all <- gss_all %>%
    select(all_of(variables$name)) %>%
    mutate(
      CMA_dataset = metadata$dataset,
      CMA_year = NA,
      CMA_country = metadata$country,
      CMA_outgroup_category = NA,
      CMA_outgroup_specific = NA,
      CMA_design = metadata$design,
      CMA_open = metadata$open,
      CMA_partid = paste(CMA_dataset, row_number(), sep = "_")
    )

# Contact variables per year

contact_measures_targets <- variables %>%
    filter(str_detect(note, "contact")) %>%
    separate(note, c(NA, "target"), extra = "drop") %>%
    mutate(name = tolower(name)) %>%
    mutate(target = str_replace(target, "religion", "jews"))

attitude_measures_targets <- variables %>%
    filter(str_detect(note, "attitude")) %>%
    separate(note, c(NA, "target"), extra = "drop") %>%
    mutate(name = tolower(name))

contact_measures_years <- contact_measures_targets %>% 
    pull(name) %>% gss_which_years(gss_all, .) %>% 
    pivot_longer(-year) %>% filter(value) %>% 
    select(-value) %>% left_join(contact_measures_targets %>% select(name, target)) 

attitude_measures_years <- attitude_measures_targets %>%
    pull(name) %>%
    gss_which_years(gss_all, .) %>%
    pivot_longer(-year) %>%
    filter(value) %>%
    select(-value) %>%
    left_join(attitude_measures_targets %>% select(name, target))


contact_targets_years <- contact_measures_years %>%
    select(-name) %>%
    distinct()

attitude_targets_years <- attitude_measures_years %>%
    select(-name) %>%
    distinct()


race_years <- contact_targets_years %>%
    filter(target == "race") %>%
    pull(year)

contact_targets_years <- contact_targets_years %>%
    filter(target != "race") %>%
    bind_rows(map_dfr(race_years, ~ tibble(year = .x, target = c("black", "white", "asianam", "hispanic")))) %>% 
    distinct()

contact_targets_years <- contact_targets_years %>% mutate(
    target
)

combinations <- contact_targets_years %>% 
    semi_join(attitude_targets_years)

contact_targets_years %>%
    anti_join(attitude_targets_years) %>%
    filter(target != "AIDS") # AIDS attitudes only measured in 1988

target_categories <- tribble(
    ~specific,  ~category,
    "AIDS",    "health",
    "mentalhealth", "health",
    "jews",     "religion",
    "black",    "ethnicity",
    "white",    "ethnicity",
    "asianam",  "ethnicity",
    "hispanic",  "ethnicity",
    "gay",      "sexuality"
) %>% {set_names(pull(., category), .$specific)}

combinations <- combinations %>% mutate(
    filter_crit = case_when(
        target == "mentalhealth" ~ "mhtrtslf != 1", #Many NA dropped - 99.6% were not asked that question
        target == "black" ~ "race != 2",
        target == "hispanic" ~ "hispanic == 1",
        target == "asianam" ~ "!racecen1 %in% 4:10",
        target == "white" ~ "race != 1",
        target == "AIDS" ~ "TRUE", #HIV status not collected
        target == "gay" ~ "TRUE", # sex orient not collected until 2008 (sexornt)
        target == "jews" ~ "relig != 3"
    ),
    status_crit = case_when(
        target == "mentalhealth" ~ "TRUE",
        target == "black" ~ "race == 1 & (is.na(hispanic) | hispanic == 1)", #Includes hispanic for 1998
        target == "hispanic" ~ "race == 1", 
        target == "asianam" ~ "race == 1 & hispanic == 1", 
        target == "white" ~ "FALSE",
        target == "AIDS" ~ "TRUE", #Not collected
        target == "gay" ~ "TRUE", #Sex orient only collected from 2008 onwards
        target == "jews" ~ "relig %in% c(1, 2, 10, 11)"
    )
)

data_years <- pmap(combinations, function(...) {

    combination <- tibble(...)

    message("Processing ", combination$year, ": ",combination$target)

    contact_vars <- contact_measures_years %>%
        filter(year == combination$year, 
              target == combination$target) %>%
        pull(name)

    if (combination$target %in% c("black", "white", "asianam", "hispanic")) {
        contact_vars <- c(contact_vars, 
        contact_measures_years %>%
        filter(year == combination$year, target == "race") %>%
        pull(name)
        )
    }

    attitude_vars <- attitude_measures_years %>%
        filter(year == combination$year, target == combination$target) %>%
        pull(name)

    data <- gss_all %>%
        select(all_of(c(basic_vars, contact_vars, attitude_vars, helper_vars)), starts_with("CMA_")) %>%
        filter(year == combination$year)

    n <- nrow(data)

    data <- data %>%
        filter(!!rlang::parse_expr(combination$filter_crit)) %>%
        mutate(
            CMA_year = combination$year,
            CMA_outgroup_category = target_categories[combination$target],
            CMA_outgroup_specific = combination$target,
            status = if_else(!!rlang::parse_expr(combination$status_crit), "high_status", "not_outgroup")
        ) %>%
        filter(!is.na(status))
    message("For ", combination$target, " in ", combination$year, ": ", n - nrow(data), " ingroup members excluded")
    data
}) %>% set_names(paste(combinations$year, combinations$target, sep = "_"))



attitude_vars_used <- map2_dfr(data_years, names(data_years), ~ tibble(dataset = .y, variable = names(.x) %>% intersect(attitude_measures_targets$name))) %>%
    separate(dataset, c("year", "target")) %>%
    arrange(target)

attitude_vars_used %>% group_by(year, target) %>% summarise(vars = glue::glue_collapse(unique(variable), sep = ", ", last = " & ")) %>% ungroup() %>% distinct(vars
    , .keep_all = TRUE) %>% select(-year) %>% arrange(target) %>% filter(!target %in% c("AIDS", "mentalhealth"))

#Mark unused vars in variables metadata
all_vars_used <- map(data_years, colnames) %>% unlist() %>% unique()
setdiff(variables %>% filter(note != "out") %>% pull(name), all_vars_used)

## Code relative vars - intl, viol etc as difference out - white
rel_vars <- c("^intl.*", "^viol*", "^work.*", "^fare.*", "^patr.*", "^fair.*")

vars_to_recode <- all_vars_used %>% str_subset(paste(rel_vars, collapse = "|")) %>% str_subset(".*whts$", negate = TRUE)

data_years <- map(data_years, function(d) {
    walk(vars_to_recode, function(v) {
        if(v %in% colnames(d)) {
            white_var <- paste0(str_sub(v, 1, 4), "whts")
#            message(unique(d$year), "_", unique(d$CMA_outgroup_specific), ": ", v, " - ", white_var)
            d[[v]] <<- d[[v]] - d[[white_var]] 
        }
    })
    d
})

## Include only relative vars that show bias (vs whites)
## Otherwise, perceptions of violent jews and unintelligent asians would be presumed to be reduced by contact - unlikely
#"^intl.*" 7 best -> neg = bias
#"^work.*" 1 best -> pos = bias
#"^fare.*": 1 best -> pos = bias
#"^viol*": 7 best -> neg = bias
#"^patr.*" 1 best -> pos = bias
#"^fair.*" 1 best -> pos = bias

bias_signs <- c(-1, 1, 1, -1, 1, 1) %>% set_names(c("intl", "work", "fare", "viol", "patr", "fair"))


unbiased <- map_dfr(data_years, function(d) {
    x <- tibble()
    walk(vars_to_recode, function(v) {
        if(v %in% colnames(d)) {
            m <- mean(d[[v]], na.rm = TRUE)
            if (sign(m) != bias_signs[str_sub(v, 1, 4)]) 
                x <<- bind_rows(x, tibble(variable = v))

        }})
        if(nrow(x) > 0) {
            x$year <- as.character(d$year[1])
            x$target <- d$CMA_outgroup_specific[1]
        }
        x
        })

# Are any combinations empty without these -> no
attitude_vars_used %>% semi_join(unbiased, by = c("year", "target")) %>% anti_join(unbiased)

#Remove unbiased variables
pwalk(unbiased, function(...) {
    current <- tibble(...)
    data_years[[paste(current$year, current$target, sep = "_")]][current$variable] <<- NULL
})


##################################################################
##                   2. Check missing data                      ##
##################################################################

# Currently, missing data types are not distinguished (https://github.com/kjhealy/gssr/issues/3) - and split ballots should probably not be imputed. Therefore, missing data is retained. 

#Once that is fixed, consider taking imputation code from ALLBUS.

#However, some 98s and 99s represent missing data (https://github.com/kjhealy/gssr/issues/8) - but not in the data here
suspicious <- map_dfr(data_years, function(x) {
    year <- unique(x$year)
    x %>% 
        summarise(across(everything(), 
            ~(any(na.omit(.x == 98 | .x == 99)) & 
            !any(na.omit(.x == 97))))) %>% 
        pivot_longer(everything()) %>% 
        filter(value) %>% 
        select(name) %>% 
        mutate(year = year)
        })


##################################################################
##            3. Recode vars and create scales                  ##
##################################################################

data_years <- map(data_years, function(df) {
    year <- df$CMA_year[1]
    group <- df$CMA_outgroup_specific[1]

    message("Processing ", year, "_", group)

    vars <- names(df)
    if ("natracey" %in% vars) {
        df <- df %>% mutate(natrace = coalesce(natrace, natracey)) %>% select(-natracey)
    }
    if ("mhtrtoth" %in% vars) {
        df <- df %>% mutate(mhtrtoth = as.numeric(coalesce(mhtrtoth, mhtrtot2) == 1)) %>% select(-mhtrtot2)
    }
    if (any(str_detect(vars, "acqnh"))) {
        acqu_vars <- str_subset(vars, "^acq.*") %>% str_subset("^acqfm.*", negate = TRUE)

        scale <- make_scale(df, acqu_vars, "acquaintance counts", print_hist = FALSE, return_list = TRUE, print_desc = FALSE)

        df["contact_acquaintance_count"] <- scale$scores

        log(glue::glue("{df$year[1]}_{df$CMA_outgroup_specific[1]}: {scale$descriptives$text}"))
        
        df <- df %>% select(-all_of(acqu_vars))
    }

    if ("frndrac1" %in% vars) {
        cond <- case_when(
            df$CMA_outgroup_specific[1] == "asianam" ~ 1,
            df$CMA_outgroup_specific[1] == "black" ~ 2,
            df$CMA_outgroup_specific[1] == "hispanic" ~ 3,
            df$CMA_outgroup_specific[1] == "white" ~ 4,
            TRUE ~ NA_real_
         )

        df["contact_friend_count"] <- df %>% select(starts_with("frndrac")) %>% mutate(across(everything(), ~.x == cond)) %>% rowSums(na.rm = TRUE) #Missing dropped because many reported less than 5
      
        df["contact_friend_count"][is.na(df["frndrac1"])] <- NA # NA where no race is reported

        df <- df %>% select(-starts_with("frndrac"))
    }

        if ("race1" %in% vars) {
        cond <- case_when(
            df$CMA_outgroup_specific[1] == "asianam" ~ 1,
            df$CMA_outgroup_specific[1] == "black" ~ 2,
            df$CMA_outgroup_specific[1] == "hispanic" ~ 3,
            df$CMA_outgroup_specific[1] == "white" ~ 4,
            TRUE ~ NA_real_
         )

        df["contact_friend_count"] <- df %>% select(all_of(paste0("race", 1:5))) %>% mutate(across(everything(), ~.x == cond)) %>% rowSums(na.rm = TRUE) 
        df["contact_friend_count"][is.na(df["race1"])] <- NA 
        df <- df %>% select(-all_of(paste0("race", 1:5)))
    }

   if ("relig1" %in% vars) {

        df["contact_friend_count"] <- df %>% select(all_of(paste0("relig", 1:5))) %>% mutate(across(everything(), ~.x == 3)) %>% rowSums(na.rm = TRUE)
        df["contact_friend_count"][is.na(df["relig1"])] <- NA 
        df <- df %>% select(-all_of(paste0("relig", 1:5)))
    }

    if (any(str_detect(vars ,".*rel$"))) { #selected because cls not available for Whites
        group <- str_subset(vars, ".*rel$") %>% str_remove("rel$")
 
        df["contact_contexts_binary"] <- df %>% select(all_of(paste0(group, c("wrk", "rel", "com", "schl")))) %>% mutate(across(everything(), ~.x == 1)) %>% rowSums(na.rm = TRUE)

        know_var <- paste0("knw", group)
        if (know_var == "knwjews") know_var <- "knwjew" #Inconsistent naming

        df["contact_contexts_binary"][df[know_var] == 2] <- 0 
        
        if (any(str_detect(vars ,".*cls$"))) df["contact_close_any"] <- as.numeric(df[paste0(group, "cls")] == 1)

        df <- df %>% 
            select(-any_of(c(paste0(group, c("wrk", "rel", "com", "schl", "cls")), know_var)))    
    }

    #Remove "other" answer options
    if ("homosex" %in% vars) {
        df <- df %>% mutate(homosex = na_if(homosex, 5))
    }
    if ("racopen" %in% vars) {
        df <- df %>% mutate(racopen = na_if(racopen, 3))
    }

    renames <- variables %>% filter(name %in% names(df)) %>% {set_names(.$name, .$var)}
    df %>% rename(all_of(renames))
})

scales <- map(data_years, ~variables %>% 
    filter(!is.na(scale), var %in% names(.x)))

data_years <- map2(data_years, scales, function(df, scales) {
    if (nrow(scales)>0) { 
        log(glue::glue("\n\n{df$year[1]}_{df$CMA_outgroup_specific[1]}:"))
        create_scales(df, scales)
    } else {
        df
        }
        })

# Reverse attitudes so that greater = more positive

to_reverse <- tribble(
    ~target,        ~measure,
    "all",          "attitude_value",
    "all",          "attitude_stereotype_fairness",
    "black",      "attitude_civic",
    "all",          "attitude_approach_neighborhood",
    "all",          "attitude_approach_marriage",
    "all",          "attitude_stereotype_hard_working",
    "hispanic",     "attitude_stereotype_strong_families",
)

data_years <- map(data_years, function(df) {
    rev <- to_reverse %>% filter((target == df$CMA_outgroup_specific[1] | target == "all") & measure %in% names(df)) %>% pull(measure)
    walk(rev, function(r) {
      df[[r]] <<- (max(df[[r]], na.rm = TRUE) + min(df[[r]], na.rm = TRUE)) - df[[r]]
    })
    df
})

# Sanity check
# Check direction of attitude coding through political_orientation (greater: right-wing)

cors <- map_dfr(data_years, function(df) {
    if (!"political_orientation" %in% names(df)) stop("pol or missing")
    year <- df$year[1]
    group <- df$CMA_outgroup_specific[1]
    df <-  df %>% select(starts_with("attitude_"), political_orientation)
    df %>% cor_matrix() %>% .[[1]] %>% .[, "political_orientation"] %>% t() %>% data.frame(
    ) %>% mutate(year = year, group = group) 
})

# Deviations look plausible (and double checked coding there)
cors %>% pivot_longer(-c(year, group)) %>% filter(value > 0, value != 1)

# Recode gender and drop helper vars
data_years <- map(data_years, ~mutate(.x,
  gender = fct_collapse(as_factor(gender), 
    male = "male", female = "female",
    other_level = "other/not reported")
) %>% select(-all_of(helper_vars)))

# Remove 2004_jews ... contact only based on religion of 5 closest people, 
# and thus too rare (only 6 with any contact)

data_years[["2004_jews"]] <- NULL

##################################################################
##               4. Reproduce descriptives                      ##
##################################################################

walk(data_years, function(df) {
    year <- df$CMA_year[1]
    group <- df$CMA_outgroup_specific[1]

    message("Processing ", year, "_", group)

    library(srvyr)
    library(survey)

    last_cors <<- df %>%
        select(!starts_with("CMA_")) %>%
        select(all_of(sort(colnames(.)))) %>%
        mutate(gender = (gender == "female") %>% as.numeric()) %>%
        zap_labels() %>%
        as_survey_design(weights = weight) %>%
        svy_cor_matrix(setdiff(names(.$variables), "weight") %>% set_names(.)) 
        
    last_cors    %>%
        report_cor_table(filename = here(glue::glue("3_data_processed/cor_tables/GSS_{year}_{group}_cor_table_generated_pairwise_del_survey_weighted (unweighted N = {nrow(df)}).html")))
})

##################################################################
##                 5. Create output files                       ##
##################################################################

# Create filter variable to remove outgroup members

pairings <- tibble()

data_both <- map(data_years, function(df) {

   # df <- rowid_to_column(df)

    df_l <- make_long(df %>% zap_labels()) #To avoid wrong labels after reshaping

    pairings <<- df_l %>% group_by(contact_var, attitude_var) %>% 
        summarise(N = sum(!is.na(contact) & !is.na(attitude)), .groups = "drop") %>%
        mutate(year = df$CMA_year[1], group = df$CMA_outgroup_specific[1]) %>%
        bind_rows(pairings, .)

    df_l <- df_l %>% group_by(contact_var, attitude_var) %>% 
        filter(sum(!is.na(contact) & !is.na(attitude)) > 0) %>%
        ungroup()

    if (nrow(df_l) == 0) {
        message(glue::glue("{df$CMA_year[1]}_{df$CMA_outgroup_specific[1]}: no overlapping attitude & contact measures"))
        return (NULL)
    }

    df <- df_l %>% 
        select(-contact_type, -contact_type_detail, -attitude_type, -attitude_type_detail) %>%
        pivot_wider(names_from = "attitude_var", values_from = "attitude") %>%
        pivot_wider(names_from = "contact_var", values_from = "contact")

   list(long = df_l, wide = df)
}) 

data_both[sapply(data_both, is.null)] <- NULL

data_both <- data_both %>% transpose() %>% map(bind_rows)

# Save aligned data
save_data(data_both$wide, data_both$long, dataset_name)

write_csv(pairings, here("1_data_raw", dataset_name, "logs", "contact_attitude_pairs.csv"))

message("Done processing ", dataset_name)

# Reset log - before sourcing completed file
# reset_log()