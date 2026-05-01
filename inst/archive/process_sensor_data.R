#' Process and Summarize Sensor Data
#'
#' @description
#' Scans a directory, parses filenames, filters based on metadata.
#' Can return raw data (wide or long) or summarized data (wide or long).
#'
#' @param site_dir Path to the directory containing processed sensor CSV files.
#' @param summarize Logical. If `FALSE` (default), returns raw data.
#'                  If `TRUE`, performs a summary using the `fun` argument.
#' @param output_format Character string: `"wide"` (default) or `"long"`.
#'   - If `summarize = FALSE`: Controls shape of raw data.
#'   - If `summarize = TRUE`: Controls shape of summarized data.
#' @param fun A function (e.g., `mean`) or a **named list of functions**
#'            (e.g., `list(mean = mean, sd = sd)`) to use *only* if
#'            `summarize = TRUE`.
#' @param aggregate_interval Optional time interval (e.g. "1 hour", "1 day").
#'   Applies a `floor_date()` to the `DateTime` column *before* any
#'   other operations.
#' @param group_vars Character vector of grouping variables (default = c("DateTime", "Field", "Plot")).
#'                   Only used if `summarize = TRUE`.
#' @param pivot_groups_wider (Optional) Column name(s) from `group_vars` to
#'   pivot into column headers for a final, very wide format.
#'   Only used if `summarize = TRUE`.
#' @param ... (Other filters: `site_filter`, `field_filter`, etc.)
#'
#' @return A tibble, formatted as requested.
#'
process_sensor_data <- function(site_dir,
                                summarize = FALSE,
                                output_format = "wide",
                                fun = NULL,
                                site_filter = NULL,
                                field_filter = NULL,
                                plot_filter = NULL,
                                port_filter = NULL,
                                sensor_filter = NULL,
                                serial_filter = NULL,
                                group_vars = c("DateTime", "Field", "Plot"),
                                pivot_groups_wider = NULL,
                                timestamp_col = NULL,
                                aggregate_interval = NULL,
                                recursive = FALSE) {

  # Load required libraries
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(readr)
    library(stringr)
    library(purrr)
    library(lubridate)
  })

  # --- NEW: Guard Clause ---
  if (summarize == TRUE && is.null(fun)) {
    stop("If `summarize = TRUE`, you must provide a function to `fun`.")
  }
  if (summarize == FALSE && !is.null(fun)) {
    warning("`fun` was provided, but `summarize = FALSE`. Ignoring `fun`.")
    fun <- NULL
  }

  # ---- 1. Query and Filter File List (Same as before) ----
  all_files_db <- query_sensor_files(site_dir, recursive)
  if (nrow(all_files_db) == 0) stop("No valid sensor files found in: ", site_dir)

  filtered_files_db <- all_files_db %>%
    filter(
      is.null(site_filter) | site %in% site_filter,
      is.null(field_filter) | field %in% field_filter,
      is.null(plot_filter) | plot %in% plot_filter,
      is.null(port_filter) | port %in% port_filter,
      is.null(sensor_filter) | sensor %in% sensor_filter,
      is.null(serial_filter) | serial %in% serial_filter
    )

  if (nrow(filtered_files_db) == 0) stop("No files remaining after applying filters.")
  message(paste("Found", nrow(filtered_files_db), "files to process."))

  # ---- 2. Reader Helper Function (Same as before) ----
  # (read_sensor_file function is unchanged)
  read_sensor_file <- function(file_path, site, field, plot, port, sensor_type, serial) {
    dat <- read_csv(file_path, skip = 6, show_col_types = FALSE)
    ts_col_name <- "DateTime"
    if (!is.null(timestamp_col) && timestamp_col %in% names(dat)) {
      names(dat)[names(dat) == timestamp_col] <- ts_col_name
    } else if ("Timestamps" %in% names(dat)) {
      names(dat)[names(dat) == "Timestamps"] <- ts_col_name
    } else {
      datetime_guess <- names(dat)[sapply(dat, inherits, "POSIXct")][1]
      if (!is.na(datetime_guess)) {
        names(dat)[names(dat) == datetime_guess] <- ts_col_name
      } else {
        stop("No POSIXct timestamp column found in ", basename(file_path))
      }
    }
    dat %>%
      mutate(
        Site = site, Field = field, Plot = plot, Port = port,
        SensorType = sensor_type, Serial = serial, File = basename(file_path)
      )
  }

  # ---- 3. Read Data (Same as before) ----
  all_data <- pmap_dfr(
    list(
      file_path = filtered_files_db$file_path,
      site = filtered_files_db$site, field = filtered_files_db$field,
      plot = filtered_files_db$plot, port = filtered_files_db$port,
      sensor_type = filtered_files_db$sensor, serial = filtered_files_db$serial
    ),
    read_sensor_file
  )

  # ---- 4. Time Aggregation (Always runs first) ----
  if (!is.null(aggregate_interval)) {
    message(paste("Aggregating DateTime to", aggregate_interval))
    all_data <- all_data %>%
      mutate(DateTime = floor_date(DateTime, aggregate_interval))
  }

  # --- Identify metadata/measurement columns ---
  metadata_cols <- c("DateTime", "Site", "Field", "Plot", "Port", "SensorType", "Serial", "File")
  non_numeric_cols <- names(all_data)[!sapply(all_data, is.numeric)]
  all_metadata_cols <- unique(c(metadata_cols, non_numeric_cols))
  all_metadata_cols <- intersect(all_metadata_cols, names(all_data))
  measurement_cols <- setdiff(names(all_data), all_metadata_cols)

  # --- 5. Main Logic Branch: Summarize or Not? ---

  if (summarize == FALSE) {
    # --- BRANCH A: RAW DATA ---
    message("Returning raw data.")

    if (output_format == "long") {
      message("Pivoting raw data to 'long' format.")
      all_data <- all_data %>%
        tidyr::pivot_longer(
          cols = all_of(measurement_cols),
          names_to = "Measurement",
          values_to = "Value"
        )
    } else {
      message("Returning raw data in 'wide' format.")
    }

    # Common arranging
    arrange_vars <- c("DateTime", "Site", "Field", "Plot", "Port", "SensorType")
    valid_arrange_vars <- intersect(arrange_vars, names(all_data))
    return(all_data %>% arrange(across(all_of(valid_arrange_vars))))
  }

  # --- BRANCH B: SUMMARIZED DATA ---

  numeric_cols <- names(all_data)[sapply(all_data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c("Port", "Serial"))

  if (!"DateTime" %in% group_vars) group_vars <- c("DateTime", group_vars)
  valid_group_vars <- intersect(group_vars, names(all_data))
  if (length(valid_group_vars) == 0) stop("No valid group_vars found in the data.")

  summarized <- NULL # Initialize

  if (is.list(fun)) {
    # --- List of functions (e.g., min, max) ---
    message(paste("Summarizing data using multiple functions:", paste(names(fun), collapse = ", ")))

    summarized_wide <- all_data %>%
      group_by(across(all_of(valid_group_vars))) %>%
      summarise(
        across(
          all_of(numeric_cols),
          fun,
          na.rm = TRUE,
          .names = "{.col}___{.fn}"
        ),
        .groups = "drop"
      )

    summarized_long <- summarized_wide %>%
      tidyr::pivot_longer(
        cols = !all_of(valid_group_vars),
        names_to = c("Measurement", "Statistic"),
        names_sep = "___",
        values_to = "Value"
      )

    if (output_format == "wide") {
      message("Returning summarized data in 'wide' (by measurement) format.")
      summarized <- summarized_long %>%
        tidyr::pivot_wider(
          names_from = "Measurement",
          values_from = "Value"
        )
    } else {
      message("Returning summarized data in 'long' (tidy) format.")
      summarized <- summarized_long # Already long
    }

  } else if (is.function(fun)) {
    # --- Single function (e.g., mean) ---
    message(paste("Summarizing data using function:", deparse(substitute(fun))))

    summarized <- all_data %>%
      group_by(across(all_of(valid_group_vars))) %>%
      summarise(across(all_of(numeric_cols), ~ fun(.x, na.rm = TRUE)), .groups = "drop")

    if (output_format == "long") {
      message("Pivoting summarized data to 'long' format.")
      summarized <- summarized %>%
        tidyr::pivot_longer(
          cols = all_of(numeric_cols),
          names_to = "Measurement",
          values_to = "Value"
        )
    } else {
      message("Returning summarized data in 'wide' (by measurement) format.")
    }
  }

  # --- Final Arrange & Pivot ---
  arrange_cols <- intersect(valid_group_vars, names(summarized))
  if ("Statistic" %in% names(summarized)) {
    arrange_cols <- c(arrange_cols, "Statistic")
  }

  summarized <- summarized %>%
    arrange(across(all_of(arrange_cols)))

  if (!is.null(pivot_groups_wider)) {
    message(paste("Pivoting wider on group column(s):",
                  paste(pivot_groups_wider, collapse = ", ")))

    if (!all(pivot_groups_wider %in% names(summarized))) {
      stop("`pivot_groups_wider` contains column names not found in the summary output.")
    }

    id_cols <- setdiff(valid_group_vars, pivot_groups_wider)
    if ("Statistic" %in% names(summarized)) {
      id_cols <- c(id_cols, "Statistic")
    }
    id_cols <- intersect(id_cols, names(summarized))

    value_cols <- setdiff(names(summarized), c(id_cols, pivot_groups_wider))

    summarized <- summarized %>%
      tidyr::pivot_wider(
        names_from = all_of(pivot_groups_wider),
        values_from = all_of(value_cols)
      )
  }

  return(summarized)
}
