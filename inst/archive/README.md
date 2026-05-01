# dswrEMMA Sensor Data Pipeline

Tools for processing, exploring, and visualizing environmental sensor data from METER Z6 loggers (TEROS11, SO411, etc.) deployed at the VB field site.

**Developer:** Adrian Wiegman  
**Location in repo:** `dswr/dswrEMMA/data/`

---

## Quick Start

1. **Open the R project** — double-click `dswrEMMA-data.Rproj` in this directory.
2. **Install dependencies** (one-time):
   ```r
   install.packages(c(
     "shiny", "ggplot2", "plotly", "tidyr", "dplyr", "readr",
     "stringr", "purrr", "lubridate", "shinycssloaders", "DT",
     "shinyFiles", "fs"
   ))
   ```
3. **Process raw data** (if starting from raw Z6 exports):
   ```r
   source("map_z6_info_and_save_files.R")
   ```
4. **Launch the interactive explorer**:
   ```r
   shiny::runApp("explore_sensors_app.R")
   ```

---

## Directory Structure

```
dswrEMMA/data/
├── README.md
├── dswrEMMA-data.Rproj
├── query_sensor_files.R          # Scans directories, parses file metadata
├── process_sensor_data.R         # Loads, filters, aggregates, summarizes sensor data
├── plot_sensor_data.R            # Plotting helper (ggplot2 + plotly)
├── map_z6_info_and_save_files.R  # Raw Z6 data → standardized per-sensor CSVs
├── explore_sensors_app.R         # Shiny app for interactive exploration
├── calculate_soil_site_averages.R # Example scripted analysis
└── VB/
    └── Meter/
        ├── inputs/               # Raw concatenated Z6 CSV exports
        │   └── z6-XXXXX_concat.csv
        ├── z6_info_long.csv      # Logger metadata (site, treatment, port, sensor)
        └── processed/            # Output: one CSV per sensor per logger port
            └── VB_east high_z6-14347_Port1_TEROS11.csv
```

---

## File Naming Convention (Processed Files)

Processed CSVs follow this pattern:

```
{Site}_{Field} {Plot}_z6-{Serial}_Port{N}_{SensorType}.csv
```

Example: `VB_east high_z6-14347_Port1_TEROS11.csv`

| Component    | Example       | Description                        |
|-------------|---------------|------------------------------------|
| Site        | `VB`          | Site code (2 uppercase letters)    |
| Field       | `east`        | Field name                         |
| Plot        | `high`        | Treatment/plot identifier          |
| Serial      | `14347`       | Z6 logger serial number            |
| Port        | `1`           | Logger port number                 |
| SensorType  | `TEROS11`     | Sensor model                       |

Each file includes a 6-line metadata header (lines starting with `#`), followed by CSV data with a `Timestamps` column.

---

## Pipeline Overview

### Step 1: `map_z6_info_and_save_files.R` — Raw → Processed

Reads raw Z6 concatenated exports and the `z6_info_long.csv` metadata file. Splits data into one CSV per sensor/port/logger combination and writes them to `processed/`.

**Inputs:**
- `VB/Meter/inputs/*.csv` — raw Z6 data files
- `VB/Meter/z6_info_long.csv` — metadata mapping (Site, Treatment, Serial, Port, Sensor)

**Output:** `VB/Meter/processed/*.csv`

```r
process_z6_data(
  input_dir = "VB/Meter/inputs",
  info_file = "VB/Meter/z6_info_long.csv",
  output_dir = "VB/Meter/processed",
  debug = TRUE
)
```

### Step 2: `query_sensor_files.R` — File Discovery

Scans a directory of processed CSVs and returns a metadata index (tibble) by parsing filenames.

```r
index <- query_sensor_files("VB/Meter/processed", recursive = TRUE)
```

### Step 3: `process_sensor_data.R` — Load, Filter, Summarize

The main workhorse. Uses the file index to load data, apply filters, aggregate over time, and optionally summarize with statistics.

```r
# Raw data, filtered
data <- process_sensor_data(
  site_dir = "VB/Meter/processed",
  site_filter = "VB",
  field_filter = "east",
  sensor_filter = "TEROS11"
)

# Daily summaries
summary_funs <- list(mean = mean, min = min, max = max)
data_summary <- process_sensor_data(
  site_dir = "VB/Meter/processed",
  summarize = TRUE,
  fun = summary_funs,
  aggregate_interval = "1 day",
  group_vars = c("DateTime", "Field", "Plot")
)
```

**Key parameters:**

| Parameter             | Description                                              |
|-----------------------|----------------------------------------------------------|
| `site_filter`, `field_filter`, `plot_filter`, `sensor_filter`, `serial_filter`, `port_filter` | Filter files by metadata |
| `summarize`           | `TRUE` to compute statistics, `FALSE` for raw data       |
| `fun`                 | A function or named list of functions (used with `summarize = TRUE`) |
| `aggregate_interval`  | Time rounding interval (e.g., `"1 hour"`, `"1 day"`)    |
| `group_vars`          | Columns to group by when summarizing                     |
| `output_format`       | `"wide"` (default) or `"long"`                           |
| `pivot_groups_wider`  | Additional columns to pivot into wide column headers     |

### Step 4: `plot_sensor_data.R` — Visualization

Accepts wide or long data, auto-pivots to long, and generates faceted line plots.

```r
plot_sensor_data(
  data = data_summary,
  x = "DateTime",
  color = "Plot",
  facet_row = "Measurement",
  interactive = TRUE   # TRUE = plotly, FALSE = ggplot2
)
```

### Step 5: `explore_sensors_app.R` — Shiny App

Interactive UI that wraps all the above into a point-and-click workflow. Includes:
- Directory browser
- Filter controls (site, field, plot, sensor, serial, port, date range)
- Data preview, column selection, time aggregation, summarization
- Interactive plotly plots
- Exportable data tables
- Auto-generated reproducible R code

---

## Example Script Usage

See `calculate_soil_site_averages.R` for a scripted (non-Shiny) example that produces daily summaries for the VB east field.

---

## Notes

- The Shiny app's `DEFAULT_SITE_DIR` is set relative to `getwd()`. If the app doesn't find data on startup, use the directory picker.
- All timestamps are expected as `POSIXct`. The raw Z6 format is `%m/%d/%Y %I:%M:%S %p`.
- Processed files skip the first 6 header lines when read by `process_sensor_data`.