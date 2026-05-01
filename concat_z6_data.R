#' Concatenate Z6 Sensor Data
#'
#' This concatenates Z6 sensor data files
#' using the utility functions defined in `R/batch_concat.R`. It performs the
#' following steps:
#'
#' - Reads a list of serial numbers from a text file.
#' - Recursively searches for matching files in the specified folder.
#' - Filters and groups files by serial numbers.
#' - Reads and renames columns based on lookup tables.
#' - Combines data frames for each serial number.
#' - Saves the combined data as CSV and RData files.
#'
#'
#' @note Ensure that the `R/batch_concat.R` script is sourced before running this script.
#' @author Adrian Wiegman, Molly Ratliff
#' @date 2026-04-30
#' @owner University of Vermont
#'
#' @examples
#' # Example usage:
#' folder_path <- "data/usr/inputs/Zentra/"
#' serial_numbers <- readr::read_lines("data/usr/inputs/serials.txt")
#' source("R/batch_concat.R")
#' # Run the script to process and save data.
source("R/batch_concat.R")

# USER INPUT =========
# Folder where Zentra Cloud exported data is located.
# User downloads data from Zentra Cloud and copies the export folders here.
folder_path <- "data/usr/inputs/Zentra/"
# Serial numbers of z6 data loggers (one per line, e.g. z6-14354).
serial_numbers <- readr::read_lines("data/usr/inputs/serials.txt")
# Output folder.
output_dir <- "data/usr/outputs/concat_data"

# Zentra Cloud parameter name → standardized column name mapping.
# Left-hand side: exact string from Zentra Cloud CSV header (row 3).
# Right-hand side: standardized column name used in output files.
#
# NOTE: Zentra Cloud changed from ASCII (pre-2025) to Unicode (2025+) unit symbols.
# Both variants are included here so old and new exports are handled automatically.
param_lookup <- list(
  # --- Shared (unit symbols unchanged between formats) ---
  "% Oxygen Concentration"      = "Soil_O2_pct",
  "% Battery Percent"           = "Battery_pct",
  "mV Battery Voltage"          = "Battery_mV",
  "kPa Reference Pressure"      = "Ref_Pressure_kPa",
  "mS/cm EC"                    = "EC_mScm",
  "mS/cm Weighted EC"           = "Weighted_EC_mScm",
  "mm Precipitation"            = "Precip_mm",
  "mm/h Max Precip Rate"        = "PrecipMaxRate_mmh",
  "kPa Vapor Pressure"          = "Vap_Pressure_kPa",
  "kPa Atmospheric Pressure"    = "Atm_Pressure_kPa",
  "kPa VPD"                     = "VPD_kPa",          # ATMOS 14; present in 2025+ exports

  # --- ASCII format (pre-2025 Zentra Cloud exports) ---
  "m3/m3 Water Content"         = "Soil_VWC_m3m3",
  "degree_C Soil Temperature"   = "Soil_Temp_degC",
  "degree_C Internal Temperature" = "Sensor_Temp_degC",
  "degree_C Logger Temperature" = "Ref_Temp_degC",
  "degree_C Water Temperature"  = "Water_Temp_degC",
  "degree_C Air Temperature"    = "Air_Temp_degC",

  # --- Unicode format (2025+ Zentra Cloud exports) ---
  "m\u00b3/m\u00b3 Water Content"      = "Soil_VWC_m3m3",     # m³/m³
  "\u00b0C Soil Temperature"           = "Soil_Temp_degC",     # °C
  "\u00b0C Internal Temperature"       = "Sensor_Temp_degC",
  "\u00b0C Logger Temperature"         = "Ref_Temp_degC",
  "\u00b0C Water Temperature"          = "Water_Temp_degC",
  "\u00b0C Air Temperature"            = "Air_Temp_degC"
)

sensor_lookup <- list(
  "TEROS 11 Moisture/Temp"                  = "TEROS11",
  "SO-411 Soil Oxygen Concentration"        = "SO411",
  "SO-411/431 Soil Oxygen Concentration"    = "SO411",  # 2025+ sensor name; kept as SO411 to avoid downstream breakage
  "Battery"                                 = "Battery",
  "Barometer"                               = "Baro",
  "ES-2 Conductivity/Temp"                  = "ES2",
  "ECRN-100 Precipitation"                  = "ECRN100",
  "ATMOS 14 Humidity/Temp/Barometer"        = "ATMOS14"
)


# Step 1: Search for all files in the folder
all_files <- search_files_limited_depth(dir=folder_path,max_depth = 4)

# Step 2: Filter files to include only relevant ones
match_patterns <- serial_numbers  # Example: Match serial numbers starting with "z6"
omit_patterns <- c("Raw","Metadata")  # Example: Exclude raw data files
filtered_files <- filter_strings_match_omit(all_files,
                                            match_patterns,
                                            omit_patterns,
                                            unique_basename=T)

# Step 3: Group files by serial number (or other identifiers)
grouped_files <- group_strings_by_identifier(filtered_files, serial_numbers)
print(grouped_files)



# Step 4: Read data for each serial number
rename_cols <- function(df, new_names) {
  if (!is.null(new_names) && length(new_names) == ncol(df)) {
    names(df) <- new_names
  } else {
    warning("Column name count does not match data frame columns.")
  }
  df
}

data_list <- lapply(grouped_files, function(files) {
  message(files)
  # Read CSV files into a list of data frames
  df_list <- fread_delimited_list(files, fread_args=list(sep=",",skip=2), use_parallel = TRUE)
  # Generate list of new column names.
  colnames_list <- lapply(files, function(file){
    print(file)
    rename_z6_columns(file,sensor_lookup,param_lookup)
  })
  # assign new column names to appropriate data frames
  df_list <- mapply(rename_cols, df_list, colnames_list, SIMPLIFY = FALSE)
})

# Combine the data frames within each list group using concatenate_data_frames
data_list_combined <- lapply(data_list,function(df_list){
  concatenate_data_frames(df_list)
})

# Step 5: Save the combined data for future analysis
dir.create(output_dir, showWarnings = FALSE)
for (name in names(data_list_combined)){
  print(name)
  df <- data_list_combined[[name]]
  output_csv <- file.path(output_dir, paste0(name, "_concat.csv"))
  readr::write_csv(df, output_csv)
}
