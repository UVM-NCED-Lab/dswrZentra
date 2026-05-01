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
#' folder_path <- "data/usr/inputs/METER/"
#' serial_numbers <- readr::read_lines("data/usr/inputs/serials.txt")
#' source("R/batch_concat.R")
#' # Run the script to process and save data.
source("R/batch_concat.R")

# USER INPUT =========
# folder where z6 data is located.
folder_path <- "data/usr/inputs/METER/"
# Serial numbers of z6 data loggers.
serial_numbers <- readr::read_lines("inputs/serials.txt")

# The left hand args are zentra cloud metric names,
# The right hand args are new metric names,
param_lookup <- list(
  # Zentra_name = New_name,
  # Older (>2025 zentra cloud output)
  "m3/m3 Water Content" = "Soil_VWC_m3m3",
  "degree_C Soil Temperature" = "Soil_Temp_degC",
  "% Oxygen Concentration" = "Soil_O2_pct",
  "degree_C Internal Temperature" = "Sensor_Temp_degC",
  "% Battery Percent" = "Battery_pct",
  "mV Battery Voltage" = "Battery_mV",
  "kPa Reference Pressure" = "Ref_Pressure_kPa",
  "degree_C Logger Temperature" = "Ref_Temp_degC",
  "mS/cm EC" = "EC_mScm",
  "mS/cm Weighted EC" = "Weighted_EC_mScm",
  "degree_C Water Temperature" = "Water_Temp_degC",
  "mm Precipitation" = "Precip_mm",
  "mm/h Max Precip Rate"="PrecipMaxRate_mmh",
  "degree_C Air Temperature"="Air_Temp_degC",
  "kPa Vapor Pressure"="Vap_Pressure_kPa",
  "kPa Atmospheric Pressure"="Atm_Pressure_kPa",

  # Current udpated (zentra cloud format)
  "m3/m3 Water Content" = "Soil_VWC_m3m3",
  "degree_C Soil Temperature" = "Soil_Temp_degC",
  "m3/m3 Water Content" = "Soil_VWC_m3m3",
  "degree_C Soil Temperature" = "Soil_Temp_degC",
  "% Oxygen Concentration" = "Soil_O2_pct",
  "degree_C Internal Temperature" = "Sensor_Temp_degC",
  "% Oxygen Concentration" = "Soil_O2_pct",
  "degree_C Internal Temperature" = "Sensor_Temp_degC",
  "% Battery Percent" = "Battery_pct",
  "mV Battery Voltage" = "Battery_mV",
  "kPa Reference Pressure" = "Ref_Pressure_kPa",
  "degree_C Logger Temperature" = "Ref_Temp_degC"
  )

sensor_lookup <- list(
  "TEROS 11 Moisture/Temp" = "TEROS11",
  "SO-411 Soil Oxygen Concentration" = "SO411",
  "Battery" = "Battery",
  "Barometer" = "Baro",
  "ES-2 Conductivity/Temp"="ES2",
  "ECRN-100 Precipitation" = "ECRN100",
  "ATMOS 14 Humidity/Temp/Barometer" = "ATMOS14",
  # updated names.
  "SO-411/431 Soil Oxygen Concentration" = "SO411" # keeping as SO411 so not to break the code downstream
)


# Step 1: Search for all files in the folder
all_files <- search_files_limited_depth(dir=folder_path,max_depth = 4)

# Step 2: Filter files to include only relevant ones
match_patterns <- serial_numbers  # Example: Match serial numbers starting with "z6"
omit_patterns <- c("Raw")  # Example: Exclude raw data files
filtered_files <- filter_strings_match_omit(all_files,
                                            match_patterns,
                                            omit_patterns,
                                            unique_basename=T)

# Step 3: Group files by serial number (or other identifiers)
grouped_files <- group_strings_by_identifier(filtered_files, serial_numbers)
print(grouped_files)

# Step 4: Read data for each serial number
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
output_dir <- "data/usr/outputs/concat_data"
dir.create(output_dir, showWarnings = FALSE)
lapply(names(data_list_combined), function(name) {
  output_csv <- file.path(output_dir, paste0(name, "_concat.csv"))
  readr::write_csv(data_list_combined[[name]], output_csv)
  output_Rdata <- file.path(output_dir, paste0(name, "_concat.Rdata"))
  save(data_list_combined[[name]],file=output_Rdata)
})
