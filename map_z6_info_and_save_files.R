# Adrian Wiegman
# 2025-08-12
#
# Load necessary libraries for data manipulation and file handling
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

#' Process Z6 Logger Data (Metadata-Driven)
#'
#' This function automates the processing of Z6 logger data. It is driven by the
#' metadata `info_file`, processing only the sensors explicitly listed there. It reads all
#' data files into memory once, then iterates through the metadata to extract,
#' reshape, and save each sensor's data into a separate CSV file.
#' Note if ports are changed then this file will need to be updated.
#'
#' @param input_dir A string path to the directory containing the raw CSV data files.
#' @param info_file A string path to the CSV file containing logger metadata. Treatment,Serial,Port,Sensor
#' @param output_dir A string path to the directory where processed files will be saved.
#' @param debug A logical value (TRUE or FALSE). If TRUE, detailed checkpoint messages
#'              and data previews will be printed. Defaults to FALSE.
#'
#' @return This function does not return a value but writes files to the output directory.
process_z6_data <- function(input_dir = "inputs", info_file = "z6_info_long.csv", output_dir = "processed", debug = FALSE) {

  # --- 1. Setup and File Checks ---
  if (!dir.exists(output_dir)) {
    message("Output directory not found. Creating it at: ", normalizePath(output_dir))
    dir.create(output_dir, recursive = TRUE)
  }
  data_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(data_files) == 0) stop("No CSV files found in the input directory.")
  if (!file.exists(info_file)) stop("Info file not found.")

  # --- 2. Load All Data and Metadata ---
  message("Loading all data files into memory. This may take a moment...")

  # Load metadata and clean it up
  info_df <- read_csv(info_file, skip = 1, show_col_types = FALSE) %>%
    mutate(
      Treatment = tolower(Treatment),
      Port_name = paste0("Port", Port), # Create a name to match column headers
      Sensor_name = str_replace_all(Sensor, "[^A-Za-z0-9]", "") # Clean sensor name
    )

  # Load all data files into a named list, with the serial number as the name
  all_data <- data_files %>%
    set_names(str_extract(basename(.), "z6-\\d+")) %>%
    map(~ read_csv(
      .x,
      col_types = cols(Timestamps = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")),
      show_col_types = FALSE
    ))

  message(paste("Successfully loaded", length(all_data), "data files."))

  # --- 3. Pre-Loop Debugging Checks ---
  if (debug) {
    message("\n--- Pre-Loop Debugging ---")
    info_serials <- unique(info_df$Serial)
    data_serials <- names(all_data)

    missing_from_data <- setdiff(info_serials, data_serials)
    if (length(missing_from_data) > 0) {
      message("[WARNING] The following serial numbers are in your info file but have no matching data file:")
      print(missing_from_data)
    }

    missing_from_info <- setdiff(data_serials, info_serials)
    if (length(missing_from_info) > 0) {
      message("[INFO] The following data files were found but have no matching serial number in the info file (they will be ignored):")
      print(missing_from_info)
    }
    message("--- End Debugging ---\n")
  }

  # --- 4. Process Data by Iterating Through Metadata ---

  # Use pwalk to iterate over each row of the info_df data frame
  pwalk(info_df, function(Site, Treatment, Serial, Port_name, Sensor_name, ...) {

    # Check if the data for the current serial number exists
    if (!Serial %in% names(all_data)) {
      if(debug) message(paste("[INFO] No data file found for Serial:", Serial, "- Skipping this metadata entry."))
      return() # Skip this row if no data file was found for this serial
    }

    # Get the specific logger's data from the list
    logger_data <- all_data[[Serial]]

    # Define the specific column prefix for the sensor we're looking for
    column_prefix <- paste0(Port_name, "_", Sensor_name)

    # Select the Timestamps and all columns that start with our prefix
    sensor_data <- logger_data %>%
      select(Timestamps, starts_with(column_prefix))

    # If no columns were found for this sensor, skip to the next one
    if (ncol(sensor_data) <= 1) {
      if(debug) message(paste("[INFO] No data columns found for", column_prefix, "in file", Serial, "- Skipping."))
      return()
    }

    # Select the matching columns and rename them by dropping the prefix.
    final_df <- sensor_data %>%
      rename_with(~ str_remove(., paste0(column_prefix, "_")), .cols = starts_with(column_prefix)) %>%
      arrange(Timestamps)

    # --- 5. Construct Header and Save File ---
    output_filename <- file.path(
      output_dir,
      paste0(Site, "_", Treatment, "_", Serial, "_", Port_name, "_", Sensor_name, ".csv")
    )

    header_lines <- c(
      paste0("# Site: ", Site),
      paste0("# Treatment: ", Treatment),
      paste0("# Serial: ", Serial),
      paste0("# Port: ", Port_name),
      paste0("# Sensor: ", Sensor_name)
    )
    data_start_line <- length(header_lines) + 2
    header_lines_with_start <- c(
      header_lines,
      paste0("# Data begins on line: ", data_start_line)
    )

    write_lines(header_lines_with_start, file = output_filename)
    write_csv(final_df, file = output_filename, append = TRUE, col_names = TRUE, na = "")

    message(paste("  -> Saved:", basename(output_filename)))
  })

  message("\nProcessing complete!")
}

# --- Example Usage ---
# To run this script, place your data files (e.g., "z6-19406_concat.csv")
# and the "z6_info_long.csv" file in a directory named "inputs".
# The processed files will be saved in a new directory named "processed".
#
# Example usage
input_dir <- "data/usr/outputs/concat_data"
info_file <- "data/usr/inputs/METER/z6_info_long.csv"
output_dir <- "data/usr/outputs/mapped_data"
process_z6_data(input_dir,
                info_file,
                output_dir,
                debug=T)
# To run with the uploaded files, create an "inputs" folder and place them inside,
# then uncomment and run the line below.
# process_z6_data()

