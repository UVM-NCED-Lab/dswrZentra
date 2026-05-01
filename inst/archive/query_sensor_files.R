library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

#' Query Sensor Files and Parse Metadata
#'
#' Scans a directory for sensor files matching the expected naming convention
#' ([site]_[field] [plot]_z6-[serial]_[port]_[sensor].csv) and returns a tibble of
#' all files with their parsed metadata.
#'
#' @param site_dir Path to the directory containing processed sensor CSV files.
#' @param recursive Logical; whether to read files recursively. Default = FALSE.
#'
#' @return A tibble with columns: file_path, file_name, site, field, plot,
#'         serial, port, and sensor.
#'
query_sensor_files <- function(site_dir, recursive = FALSE) {

  # List all .csv files
  files <- list.files(site_dir, pattern = "\\.csv$",
                      full.names = TRUE, recursive = recursive)

  if (length(files) == 0) {
    warning("No .csv files found in: ", site_dir)
    return(tibble(
      file_path = character(), file_name = character(),
      site = character(), field = character(), plot = character(),
      serial = integer(), port = integer(), sensor = character()
    ))
  }

  # Create a tibble and use tidyr::extract to parse names
  file_metadata <- tibble(file_path = files, file_name = basename(files)) %>%
    tidyr::extract(
      file_name,
      into = c("site", "combined_plot", "serial", "port", "sensor"),
      # Regex captures:
      # 1: Site (e.g., "VB")
      # 2: Combined Plot (e.g., "east high" or "mid peizometer")
      # 3: Serial (e.g., 14347)
      # 4: Port (e.g., 1)
      # 5: Sensor (e.g., "TEROS11")
      regex = "^([A-Z]{2})_(.*?)_z6-(\\d+)_Port(\\d+)_(.*?)\\.csv$",
      remove = FALSE, # Keep the original file_name column
      convert = TRUE  # Automatically converts serial and port to integer
    ) %>%
    # Filter out any files that didn't match the pattern
    filter(!is.na(site)) %>%

    # --- SEPARATE 'combined_plot' into 'Field' and 'Plot' ---
    tidyr::separate_wider_delim(
      combined_plot,
      delim = " ",
      names = c("field", "plot"),
      too_few = "align_start", # If no space, becomes field="name", plot=NA
      too_many = "merge"       # If "a b c", becomes field="a", plot="b c"
      # 'col_remove = TRUE' was removed here, as it's the default behavior
      # and not a valid argument.
    )

  return(file_metadata)
}
