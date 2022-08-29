
#' Logs text to a dataset_cleaning.log file in raw data folder

log <- function(..., dataset) {
  if (missing(dataset)) {
    if (!exists("dataset_name")) stop("To log, either dataset needs to be specified or global dataset_name variable needs to exist")
    dataset = dataset_name
  }

  message <- list(...) %>% str_flatten(collapse = "\n")

  message(message)
  file = here("1_data_raw", dataset, "logs", "dataset_cleaning.log")
  if (file.exists(file)) {
    cat("\n", message, file = file, append = TRUE)
  } else {
    if (!dir.exists(dirname(file))) dir.create(dirname(file))
    cat(message, file = file)
  }
}

#' Remove log (e.g., to remove clutter accumulated from interactive development)

reset_log <- function(dataset) {
  if (missing(dataset)) {
    if (!exists("dataset_name")) stop("To reset, either dataset needs to be specified or global dataset_name variable needs to exist")
    dataset = dataset_name
  }
  file = here("1_data_raw", dataset, "logs", "dataset_cleaning.log")
  if (file.exists(file)) {
    invisible(file.remove(file))
  } else {
    message("No log found.")
  }
}



#' Reads data, adds metadata and selects relevant variables

read_and_augment <- function(dataset_name, ...) {
  variables <- read_csv(here("0_metadata/variables.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

  metadata <- read_csv(here("0_metadata/metadata.csv"), show_col_types = FALSE) %>%
    filter(dataset == dataset_name)

  file_type <- tools::file_ext(metadata$filename)

  if(length(variables$name) == 0) stop("Variables to import need to be specified in variables.csv")

  if (file_type == "sav") {
    data <- read_sav(here("1_data_raw", metadata$dataset, metadata$filename), col_select = all_of(variables$name), ...)
  } else if (file_type == "dta") {
    data <- read_dta(here("1_data_raw", metadata$dataset, metadata$filename), col_select = all_of(variables$name), ...)
  } else if (file_type == "csv") {
    dots <- rlang::list2(...)
    if ("delim" %in% names(dots)) {
      data <- rlang::exec("read_delim", here("1_data_raw", metadata$dataset, metadata$filename), col_select = all_of(variables$name), !!!dots)
    } else {
      data <- rlang::exec("read_csv", here("1_data_raw", metadata$dataset, metadata$filename), col_select = all_of(variables$name), !!!dots)
    }
  } else {
    stop("Add code for filetype: ", file_type)
  }

  message(glue::glue("Read {dataset_name} with {nrow(data)} rows"))

  # Add metadata & select revelant variables
  data <- data %>%
    select(all_of(variables$name)) %>%
    mutate(
      CMA_dataset = metadata$dataset,
      CMA_year = metadata$year,
      CMA_country = metadata$country,
      CMA_outgroup_category = metadata$outgroup_category,
      CMA_outgroup_specific = metadata$outgroup_specific,
      CMA_design = metadata$design,
      CMA_open = metadata$open,
      CMA_partid = paste(CMA_dataset, row_number(), sep = "_")
    )

  # Rename variables
  data <- data %>%
    rename(variables %>%
      {
        set_names(pull(., name), pull(., var))
      })

  # Add weight if not given
  if (!"weight" %in% names(data)) {
    data <- data %>% mutate(weight = 1)
  }

  data
}


#' Creates scales and logs descriptives

create_scales <- function(data, variables_tibble = NULL) {
  if (is.null(variables_tibble)) {
      variables_tibble <- read_csv(here("0_metadata/variables.csv"), show_col_types = FALSE) %>% 
      filter(dataset == dataset_name)
  }

  mi <- ".imp" %in% names(data)

  # Make scales
  data <- map_dfc(na.omit(unique(variables_tibble$scale)), function(scale_name) {
    scale_items <- variables_tibble %>% filter(scale == scale_name) %>% pull(var)

  if(length(scale_items) == 1) {
    message(scale_name, " consists of single item. This is simply renamed (and reversed if _rev is included in the name).")
    if (str_detect(scale_items, "_rev")) {
      data[[scale_items]] <- (max(data[[scale_items]], na.rm = TRUE) + min(data[[scale_items]], na.rm = TRUE)) - data[[scale_items]] 
    }
    return(tibble(!!scale_name := data[[scale_items]]))
  }

    reverse <- "auto"
    reverse_items <- NULL
    if (any(str_detect(scale_items, "_rev$"))) {
      reverse <- "spec"
      reverse_items <- str_subset(scale_items, "_rev$")
    }
    if(mi) {
     out <- make_scale_mi(data,  scale_items,
      scale_name, print_desc = FALSE, return_list = TRUE, 
      reverse = reverse, reverse_items = reverse_items)
    } else {
     out <- make_scale(data, scale_items,
      scale_name, print_hist = FALSE, print_desc = FALSE, return_list = TRUE,
      reverse = reverse, reverse_items = reverse_items)
    }
    log(out$descriptives$text)
    tibble(!!scale_name := out$scores)
  }) %>% bind_cols(data %>% 
    select(-any_of(na.omit(unique(variables_tibble$scale)))), #Remove single-item "scales" (sometimes in multi-sample (social survey) datasets)
    .)

  # Drop scale items and rename variables

  data <- data %>% 
    select(-all_of(variables_tibble %>% 
                   filter(!is.na(scale)) %>% pull(var)))

  data
}

#' Make df longer by gathering contact and attitudes - and authoritarianism later?

make_long <- function(data) {
  data %>%
    pivot_longer(starts_with("contact"), names_to = "contact_var", values_to = "contact") %>% 
    pivot_longer(starts_with("attitude"), names_to = "attitude_var", values_to = "attitude") %>%
    separate(attitude_var, c(NA, "attitude_type", "attitude_type_detail"), sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    separate(contact_var, c(NA, "contact_type", "contact_type_detail"), sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    mutate(across(c(attitude_type, contact_type), ~str_remove(.x, "[0-9]*$"))) # remove numbers from end of type, if used to disambiguate
}

save_data <- function(data, data_long, dataset_name) {
  write_rds(data, here("3_data_processed", paste0(dataset_name, "_dataset_wide.RDS")))
  write_rds(data_long, here("3_data_processed", paste0(dataset_name, "_dataset_long.RDS")))
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
