
#' Logs text to a dataset_cleaning.log file in raw data folder

log <- function(..., dataset) {
  if (missing(dataset)) {
    if (!exists("dataset_name")) stop("To log, either dataset needs to be specified or global dataset_name variable needs to exist")
    dataset = dataset_name
  }

  message <- list(...) %>% str_flatten(collapse = "\n")

  message(message)
  file = here("data_raw", dataset, "logs", "dataset_cleaning.log")
  if (file.exists(file)) {
    cat("\n", message, file = file, append = TRUE)
  } else {
    if (!dir.exists(dirname(file))) dir.create(dirname(file))
    cat(message, file = file)
  }
}

#' Reads data, adds metadata and selects relevant variables

read_and_augment <- function(dataset_name) {

    variables <- read_csv(here("metadata/variables.csv"), show_col_types = FALSE) %>% 
      filter(dataset == dataset_name)

    metadata <- read_csv(here("metadata/metadata.csv"), show_col_types = FALSE) %>% 
      filter(dataset == dataset_name)

    file_type <- tools::file_ext(metadata$filename)

    if (file_type == "sav") {
      data <- read_sav(here("data_raw", metadata$dataset, metadata$filename))
    } else if (file_type == "dta") {
      data <- read_dta(here("data_raw", metadata$dataset, metadata$filename))
    } else {
      stop("Add code for filetype: ", file_type)
    }

    message(glue::glue("Read {dataset_name} with {nrow(data)} rows"))

    # Add metadata & select revelant variables
    data <- data %>% select(all_of(variables$name)) %>% 
      mutate(
        CMA_dataset = metadata$dataset,
        CMA_year =  metadata$year,
        CMA_country = metadata$country,
        CMA_outgroup_category = metadata$outgroup_category,
        CMA_outgroup_specific = metadata$outgroup_specific,
        CMA_design = metadata$design,
        CMA_open = metadata$open,
        CMA_partid = paste(CMA_dataset, row_number(), sep = "_")
    )

    #Rename variables
    data <- data %>% 
      rename(variables %>% {set_names(pull(., name), pull(., var))})

    #Add weight if not given
    if (!"weight" %in% names(data)) {
      data <- data %>% mutate(weight = 1)
    }

    data
}


#' Creates scales and logs descriptives

create_scales <- function(data, variables_tibble = NULL) {
  if (is.null(variables_tibble)) {
      variables_tibble <- read_csv(here("metadata/variables.csv"), show_col_types = FALSE) %>% 
      filter(dataset == dataset_name)
  }

  mi <- ".imp" %in% names(data)

  # Make scales
  data <- map_dfc(na.omit(unique(variables_tibble$scale)), function(scale_name) {
    if(mi) {
     out <- make_scale_mi(data, variables_tibble %>% filter(scale == scale_name) %>% pull(var),
      scale_name, print_desc = FALSE, return_list = TRUE)
    } else {
     out <- make_scale(data, variables_tibble %>% filter(scale == scale_name) %>% pull(var),
      scale_name, print_hist = FALSE, print_desc = FALSE, return_list = TRUE)
    }
    log(out$descriptives$text)
    tibble(!!scale_name := out$scores)
  }) %>% bind_cols(data, .)

  # Drop scale items and rename variables
  data <- data %>% 
    select(-all_of(variables_tibble %>% filter(!is.na(scale)) %>% pull(var)))

  data
}

#' Make df longer by gathering contact and attitudes - and authoritarianism later?

make_long <- function(data) {
  data %>%
    pivot_longer(starts_with("contact"), names_to = "contact_type", values_to = "contact") %>% 
    pivot_longer(starts_with("attitude"), names_to = "attitude_type", values_to = "attitude") %>%
    mutate(across(c(attitude_type, contact_type), ~str_remove(.x, "[0-9]*$"))) # remove numbers from end of type, if used to disambiguate
}

save_data <- function(data, data_long, dataset_name) {
  write_rds(data, here("data_processed", paste0(dataset_name, "_dataset_wide.RDS")))
  write_rds(data_long, here("data_processed", paste0(dataset_name, "_dataset_long.RDS")))
  invisible(TRUE)
}

range_ <- function(x, digits = 2, simplify = FALSE) {
  x <- c(x) #drop matrix dimensions
  if(simplify) {
    if(length(na.omit(unique(x))) == 1) return(round_(na.omit(unique(x)), digits))
  }
  glue::glue("{round_(min(x, na.rm = TRUE), digits)} - {round_(max(x, na.rm = TRUE), digits)}")
}

print_levels <- function(x) {
  glue::glue_collapse(levels(as_factor(x)), sep = ", ", last = " & ")
}
