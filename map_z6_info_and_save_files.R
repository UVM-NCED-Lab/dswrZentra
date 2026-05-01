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
#' Process Z6 Logger Data (data-driven)
#'
#' For every concatenated Z6 data file in \code{input_dir}, this function:
#'  \enumerate{
#'    \item Parses the column names to find every \code{Port{N}_{Sensor}}
#'          group present in the data (so it adapts automatically when ports
#'          are re-wired or sensors are swapped).
#'    \item Looks up the Site and Plot for each (Serial, Port) from the
#'          user-maintained \code{z6_info_wide.csv}. The lookup tries an
#'          exact \code{(Serial, Port)} match first; if the port isn't
#'          listed for that serial, it falls back to the Serial-only
#'          Site/Plot so the port still inherits the logger's location.
#'    \item Writes one CSV per (Serial, Port, Sensor) group, named
#'          \code{Site_Plot_Serial_Port_Sensor.csv}, with a 6-line metadata
#'          header. If a port carries multiple sensors, multiple files are
#'          produced for that port.
#'  }
#' Battery and Barometer housekeeping channels are skipped.
#'
#' Sensor identity is taken from the data columns themselves (which were
#' named by \code{concat_z6_data.R} from the Zentra Cloud headers), NOT from
#' \code{z6_info_wide.csv}. The user only declares Site/Plot per (Serial, Port).
#'
#' @param input_dir       Directory containing \code{<serial>_concat.csv}
#'   files (output of \code{concat_z6_data.R}).
#' @param info_wide_file  Path to the user-maintained \code{z6_info_wide.csv}.
#'   Columns: \code{Site, Plot, Serial, Port_1 … Port_6, Notes}.
#'   The (Serial, Port) → (Site, Plot) mapping is preferred; rows without an
#'   explicit per-port entry inherit Site/Plot from any other row matching
#'   the same Serial. The per-port sensor labels in this file are
#'   descriptive only (used for the optional Zentra metadata cross-check).
#' @param output_dir      Directory where per-sensor CSV files will be written.
#' @param zentra_dir      (Optional) Zentra Cloud export directory. When
#'   supplied, the user's sensor labels in \code{z6_info_wide.csv} are
#'   compared with the loggers' Metadata CSVs and discrepancies are printed
#'   as warnings. Output files are unaffected.
#' @param debug           If \code{TRUE}, print detailed diagnostics.
#'
#' @return Invisibly returns \code{NULL}; side effect is writing files to
#'   \code{output_dir}.
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

  # --- 2. Build Site/Plot lookups from z6_info_wide ---
  # The Sensor column in z6_info_wide is treated as descriptive only; actual
  # sensor identity per port comes from the concatenated data columns (which
  # were named by concat_z6_data.R from the Zentra Cloud CSV headers).
  #
  # Two lookups are built so we can do a (Serial, Port) → (Site, Plot) match
  # first and fall back to Serial-only if the port isn't listed:
  #   * site_plot_port_lookup: keyed on (Serial, Port). Used when a logger
  #     spans multiple plots across its ports (e.g. z6-18472 with rain gauge
  #     on ports 1/2, west tile on port 3, east tile on port 4).
  #   * site_plot_lookup:      keyed on Serial only. Used as a fallback for
  #     ports that appear in the data but aren't explicitly listed in
  #     z6_info_wide, so they inherit the logger's Site/Plot rather than
  #     being written as "UnknownSite"/"UnknownPlot". If the Serial itself
  #     has multiple distinct Site/Plot values, the unique values are joined
  #     with "-".
  info_df <- build_info_long(info_wide_file)
  site_plot_port_lookup <- info_df %>%
    select(Serial, Port, Site, Plot) %>%
    distinct()
  site_plot_lookup <- info_df %>%
    select(Serial, Site, Plot) %>%
    distinct()

  # --- 3. Optional: verify z6_info_wide against Zentra metadata CSVs ---
  # This is purely informational. It compares the user's declared sensor
  # (in z6_info_wide) to what the logger metadata reports. Output files are
  # NOT affected — actual sensor identity is taken from the data columns.
  if (!is.null(zentra_dir) && dir.exists(zentra_dir)) {
    meta_df <- parse_zentra_metadata(zentra_dir)
    if (nrow(meta_df) > 0) {
      check <- info_df %>%
        left_join(meta_df, by = c("Serial", "Port")) %>%
        filter(!is.na(Sensor_zentra), !is.na(Sensor), Sensor != "")

      mismatches <- check %>%
        filter(!str_detect(Sensor_zentra,
                           regex(str_replace_all(Sensor, "[^A-Za-z0-9]", ".*"),
                                 ignore_case = TRUE)))

      if (nrow(mismatches) > 0) {
        message("\n[WARNING] Sensor mismatches (z6_info_wide vs Zentra metadata):")
        print(mismatches %>% select(Serial, Port, Sensor, Sensor_zentra))
      } else {
        message("[OK] z6_info_wide sensor labels match Zentra metadata.")
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

  # --- 6. Data-driven split: for each Serial, iterate Port/Sensor groups
  #         actually present in the concatenated data, write one file each. ---
  for (serial in names(all_data)) {

    logger_data <- all_data[[serial]]
    cols <- setdiff(colnames(logger_data), "Timestamps")

    # Parse "Port{N}_{SensorName}_{Param}" -> ("Port{N}_{SensorName}", N)
    port_sensor <- str_match(cols, "^(Port\\d+_[A-Za-z0-9]+)_")
    valid       <- !is.na(port_sensor[, 2])
    if (!any(valid)) {
      if (debug) message("[INFO] No Port_*_Sensor columns in ", serial, " - skipping.")
      next
    }

    # Unique (Port_name, Sensor_name) groups present in this logger's data
    groups <- unique(data.frame(
      prefix      = port_sensor[valid, 2],
      Port_name   = str_extract(port_sensor[valid, 2], "^Port\\d+"),
      Sensor_name = str_remove(port_sensor[valid, 2], "^Port\\d+_"),
      stringsAsFactors = FALSE
    ))
    groups$Port <- as.integer(str_extract(groups$Port_name, "\\d+"))

    # Skip Battery / Baro housekeeping channels; these aren't field sensors
    groups <- groups[!groups$Sensor_name %in% c("Battery", "Baro"), , drop = FALSE]
    if (nrow(groups) == 0) next

    for (i in seq_len(nrow(groups))) {
      column_prefix <- groups$prefix[i]
      Port_name     <- groups$Port_name[i]
      Sensor_name   <- groups$Sensor_name[i]
      Port_num      <- groups$Port[i]

      # Look up Site/Plot. Prefer an exact (Serial, Port) match from
      # z6_info_wide so loggers whose ports span multiple plots
      # (e.g. z6-18472: rain gauge on ports 1/2, west tile on 3, east
      # tile on 4) get the correct per-port Site/Plot. If the port isn't
      # listed for this Serial, fall back to the Serial-only lookup so the
      # port still inherits the logger's Site/Plot. If the Serial isn't
      # listed at all, warn and use placeholders.
      lk_port <- site_plot_port_lookup %>%
        filter(Serial == serial, Port == Port_num)
      if (nrow(lk_port) >= 1) {
        Site <- paste(unique(lk_port$Site), collapse = "-")
        Plot <- paste(unique(lk_port$Plot), collapse = "-")
      } else {
        lk <- site_plot_lookup %>% filter(Serial == serial)
        if (nrow(lk) == 0) {
          message("[WARNING] No Site/Plot entry in z6_info_wide for serial ",
                  serial,
                  " — using 'UnknownSite'/'UnknownPlot'. Add a row to z6_info_wide.csv to fix.")
          Site <- "UnknownSite"
          Plot <- "UnknownPlot"
        } else {
          # Serial is listed but this specific Port isn't. Fall back to the
          # logger-level Site/Plot. If the Serial spans multiple Site/Plot
          # rows, keep them all by joining the unique values.
          Site <- paste(unique(lk$Site), collapse = "-")
          Plot <- paste(unique(lk$Plot), collapse = "-")
        }
      }

      sensor_data <- logger_data %>% select(Timestamps, starts_with(column_prefix))

      final_df <- sensor_data %>%
        rename_with(~ str_remove(., paste0(column_prefix, "_")),
                    .cols = starts_with(column_prefix)) %>%
        arrange(Timestamps)

      # Preserve a single space between Field and Plot so downstream parsers
      # (inst/archive/query_sensor_files.R) can split the combined "field plot"
      # token on " " to recover Field and Plot from the filename.
      plot_safe <- str_replace_all(Plot, "[^A-Za-z0-9 _-]", "_")

      output_filename <- file.path(
        output_dir,
        paste0(Site, "_", plot_safe, "_", serial, "_", Port_name, "_", Sensor_name, ".csv")
      )

      header_lines <- c(
        paste0("# Site: ",      Site),
        paste0("# Treatment: ", Plot),
        paste0("# Serial: ",    serial),
        paste0("# Port: ",      Port_name),
        paste0("# Sensor: ",    Sensor_name)
      )
      data_start_line <- length(header_lines) + 2
      header_lines <- c(header_lines, paste0("# Data begins on line: ", data_start_line))

      write_lines(header_lines, file = output_filename)
      write_csv(final_df, file = output_filename, append = TRUE, col_names = TRUE, na = "")

      message("  -> Saved: ", basename(output_filename))
    }
  }

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
