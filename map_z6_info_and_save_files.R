# Adrian Wiegman
# 2025-08-12  (updated 2026-05)
#
# Load necessary libraries for data manipulation and file handling
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ---------------------------------------------------------------------------
# Helper: parse_zentra_metadata
# ---------------------------------------------------------------------------
#' Parse Zentra Cloud Metadata CSVs to extract Port -> Sensor mappings
#'
#' Searches zentra_dir recursively for files matching
#' z6-<serial>.*Metadata.*\.csv, parses each one, and returns a data
#' frame with columns Serial, Port, Sensor_zentra so callers can
#' cross-check against user-maintained z6_info_wide.csv.
#'
#' @param zentra_dir Path to the folder containing Zentra Cloud export
#'   sub-folders (i.e. data/usr/inputs/Zentra/).
#' @return A data frame with columns Serial, Port (integer),
#'   Sensor_zentra (character).
parse_zentra_metadata <- function(zentra_dir) {
  meta_files <- list.files(
    path = zentra_dir,
    pattern = ".*Metadata.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(meta_files) == 0) {
    message("[INFO] No Zentra metadata CSV files found in: ", zentra_dir)
    return(data.frame(Serial = character(), Port = integer(), Sensor_zentra = character()))
  }

  purrr::map_dfr(meta_files, function(f) {
    serial <- stringr::str_extract(basename(f), "z6-\\d+")

    raw <- tryCatch(
      readLines(f, warn = FALSE),
      error = function(e) {
        message("[WARN] Could not read metadata file: ", f)
        return(character(0))
      }
    )
    if (length(raw) == 0) return(NULL)

    port_rows <- which(stringr::str_detect(raw, "^,Port #,"))
    name_rows <- which(stringr::str_detect(raw, "^,Name,"))

    purrr::map_dfr(seq_along(port_rows), function(i) {
      pr <- port_rows[i]
      nr <- name_rows[name_rows > pr][1]
      if (is.na(nr)) return(NULL)
      port_num <- as.integer(trimws(strsplit(raw[pr], ",")[[1]][3]))
      sensor   <- trimws(strsplit(raw[nr], ",")[[1]][3])
      if (sensor %in% c("Battery", "Barometer")) return(NULL)
      data.frame(Serial = serial, Port = port_num, Sensor_zentra = sensor,
                 stringsAsFactors = FALSE)
    })
  })
}

# ---------------------------------------------------------------------------
# Helper: build_info_long
# ---------------------------------------------------------------------------
#' Pivot z6_info_wide.csv from wide to long format
#'
#' Reads the user-maintained z6_info_wide.csv (columns:
#' Site, Plot, Serial, Port_1 ... Port_6, Notes) and returns a long-form
#' data frame with columns Site, Plot, Serial, Port, Sensor.
#' Empty port cells are dropped.
#'
#' @param info_wide_file Path to z6_info_wide.csv.
#' @return A data frame with columns Site, Plot, Serial, Port (integer),
#'   Sensor (character).
build_info_long <- function(info_wide_file) {
  wide <- read_csv(info_wide_file, skip = 2,
                   col_names = c("Site", "Plot", "Serial",
                                 paste0("Port_", 1:6), "Notes"),
                   show_col_types = FALSE)

  wide %>%
    select(-Notes) %>%
    pivot_longer(
      cols      = starts_with("Port_"),
      names_to  = "Port_col",
      values_to = "Sensor"
    ) %>%
    filter(!is.na(Sensor), Sensor != "") %>%
    mutate(Port = as.integer(str_extract(Port_col, "\\d+"))) %>%
    select(Site, Plot, Serial, Port, Sensor)
}

# ---------------------------------------------------------------------------
# Main: process_z6_data
# ---------------------------------------------------------------------------
#' Process Z6 Logger Data
#'
#' Reads concatenated Z6 data files (output of concat_z6_data.R), maps each
#' sensor port to its site/plot context using z6_info_wide.csv, and writes
#' one CSV per sensor port.  Optionally verifies port-sensor assignments
#' against Zentra Cloud metadata CSVs when zentra_dir is supplied.
#'
#' @param input_dir       Directory containing <serial>_concat.csv files.
#' @param info_wide_file  Path to the user-maintained z6_info_wide.csv.
#'   Columns: Site, Plot, Serial, Port_1 ... Port_6, Notes.
#'   This is the ONLY file users need to maintain; port-sensor assignments
#'   can be verified (but not overridden) by Zentra metadata CSVs.
#' @param output_dir      Directory where per-sensor CSV files will be written.
#' @param zentra_dir      (Optional) Zentra Cloud export directory. When
#'   supplied, metadata CSVs are parsed and discrepancies with
#'   z6_info_wide.csv are printed as warnings.
#' @param debug           If TRUE, print detailed diagnostics.
#'
#' @return Invisibly returns NULL; side effect is writing files to output_dir.
process_z6_data <- function(
    input_dir      = "data/usr/outputs/concat_data",
    info_wide_file = "data/usr/inputs/z6_info_wide.csv",
    output_dir     = "data/usr/outputs/mapped_data",
    zentra_dir     = NULL,
    debug          = FALSE) {

  # --- 1. Setup and file checks ---
  if (!dir.exists(output_dir)) {
    message("Output directory not found. Creating: ",
            normalizePath(output_dir, mustWork = FALSE))
    dir.create(output_dir, recursive = TRUE)
  }
  data_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(data_files) == 0) stop("No CSV files found in input_dir: ", input_dir)
  if (!file.exists(info_wide_file)) stop("Info file not found: ", info_wide_file)

  # --- 2. Build long-form info from z6_info_wide ---
  info_df <- build_info_long(info_wide_file) %>%
    mutate(
      Port_name   = paste0("Port", Port),
      Sensor_name = str_replace_all(Sensor, "[^A-Za-z0-9]", "")
    )

  # --- 3. Optional: verify against Zentra metadata CSVs ---
  if (!is.null(zentra_dir) && dir.exists(zentra_dir)) {
    meta_df <- parse_zentra_metadata(zentra_dir)
    if (nrow(meta_df) > 0) {
      check <- info_df %>%
        left_join(meta_df, by = c("Serial", "Port")) %>%
        filter(!is.na(Sensor_zentra))

      mismatches <- check %>%
        filter(!str_detect(Sensor_zentra,
                           regex(str_replace_all(Sensor, "[^A-Za-z0-9]", ".*"),
                                 ignore_case = TRUE)))

      if (nrow(mismatches) > 0) {
        message("\n[WARNING] Sensor mismatches (z6_info_wide vs Zentra metadata):")
        print(mismatches %>% select(Serial, Port, Sensor, Sensor_zentra))
      } else {
        message("[OK] Sensor assignments in z6_info_wide.csv match Zentra metadata.")
      }
    }
  }

  # --- 4. Load all concatenated data files ---
  message("Loading concatenated data files...")
  all_data <- data_files %>%
    set_names(str_extract(basename(.), "z6-\\d+")) %>%
    map(~ read_csv(
      .x,
      col_types = cols(Timestamps = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")),
      show_col_types = FALSE
    ))
  message("Loaded ", length(all_data), " data file(s).")

  # --- 5. Pre-loop diagnostics ---
  if (debug) {
    info_serials <- unique(info_df$Serial)
    data_serials <- names(all_data)
    missing_from_data <- setdiff(info_serials, data_serials)
    if (length(missing_from_data) > 0)
      message("[WARNING] Serials in z6_info_wide but no data file: ",
              paste(missing_from_data, collapse = ", "))
    missing_from_info <- setdiff(data_serials, info_serials)
    if (length(missing_from_info) > 0)
      message("[INFO] Data files with no entry in z6_info_wide (ignored): ",
              paste(missing_from_info, collapse = ", "))
  }

  # --- 6. Iterate over info rows and write per-sensor files ---
  pwalk(info_df, function(Site, Plot, Serial, Port, Port_name, Sensor, Sensor_name, ...) {

    if (!Serial %in% names(all_data)) {
      if (debug) message("[INFO] No data for Serial: ", Serial, " - skipping.")
      return()
    }

    logger_data   <- all_data[[Serial]]
    column_prefix <- paste0(Port_name, "_", Sensor_name)
    sensor_data   <- logger_data %>% select(Timestamps, starts_with(column_prefix))

    if (ncol(sensor_data) <= 1) {
      if (debug)
        message("[INFO] No columns for ", column_prefix, " in ", Serial, " - skipping.")
      return()
    }

    final_df <- sensor_data %>%
      rename_with(~ str_remove(., paste0(column_prefix, "_")),
                  .cols = starts_with(column_prefix)) %>%
      arrange(Timestamps)

    # Replace characters not suitable for filenames
    plot_safe <- str_replace_all(Plot, "[^A-Za-z0-9_-]", "_")

    output_filename <- file.path(
      output_dir,
      paste0(Site, "_", plot_safe, "_", Serial, "_", Port_name, "_", Sensor_name, ".csv")
    )

    header_lines <- c(
      paste0("# Site: ",   Site),
      paste0("# Plot: ",   Plot),
      paste0("# Serial: ", Serial),
      paste0("# Port: ",   Port_name),
      paste0("# Sensor: ", Sensor)
    )
    data_start_line <- length(header_lines) + 2
    header_lines <- c(header_lines, paste0("# Data begins on line: ", data_start_line))

    write_lines(header_lines, file = output_filename)
    write_csv(final_df, file = output_filename, append = TRUE, col_names = TRUE, na = "")

    message("  -> Saved: ", basename(output_filename))
  })

  message("\nProcessing complete!")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
# USER INPUT: adjust paths if your directory layout differs from the defaults.

# Directory containing <serial>_concat.csv files (output of concat_z6_data.R)
input_dir      <- "data/usr/outputs/concat_data"

# User-maintained wide-format site/plot info file (ONE file to maintain).
# Columns: Site, Plot, Serial, Port_1, Port_2, ..., Port_6, Notes
# Location: data/usr/inputs/z6_info_wide.csv
info_wide_file <- "data/usr/inputs/z6_info_wide.csv"

# Directory for per-sensor output files
output_dir     <- "data/usr/outputs/mapped_data"

# (Optional) Zentra Cloud export directory for sensor-assignment verification.
# Set to NULL to skip. Does NOT change output — warnings only.
zentra_dir     <- "data/usr/inputs/Zentra"

process_z6_data(
  input_dir      = input_dir,
  info_wide_file = info_wide_file,
  output_dir     = output_dir,
  zentra_dir     = zentra_dir,
  debug          = TRUE
)
