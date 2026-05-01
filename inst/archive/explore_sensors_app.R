# --- 0. Load Libraries ---
# Make sure these are all installed:
# install.packages(c("shiny", "ggplot2", "plotly", "tidyr", "dplyr", "readr",
#                    "stringr", "purrr", "lubridate", "shinycssloaders", "DT",
#                    "shinyFiles", "fs"))

library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(shinycssloaders) # For loading spinners
library(DT)              # For interactive tables
library(shinyFiles)      # For directory selection
library(fs)              # For finding home directory

# --- 1. Load Helper Functions ---
# These MUST be in the same directory as app.R
# Check if files exist before sourcing
if (file.exists("query_sensor_files.R")) {
  source("query_sensor_files.R")
} else {
  stop("Helper file 'query_sensor_files.R' not found.")
}
if (file.exists("process_sensor_data.R")) {
  source("process_sensor_data.R")
} else {
  stop("Helper file 'process_sensor_data.R' not found.")
}


# --- 2. Define Global Variables ---
DEFAULT_MIN_DATE <- as.Date("2022-01-01")
DEFAULT_MAX_DATE <- Sys.Date()
# --- NEW: Define a default path relative to the app ---
# @#@OJEWFJNF #$)F#$ J#$*
# NOTE CHANGE THIS BEFORE SHARING !!!!!!!!!!!!!!!!!
DEFAULT_SITE_DIR <- file.path(getwd(),"VB", "Meter", "processed")
# DEFAULT_SITE_DIR <- file.path(getwd(), "data", "VB", "Meter", "processed")

# --- 3. The Plotting Function ---
# --- FIXED: Source the correct plotting script ---
if (file.exists("plot_sensor_data.R")) {
  source("plot_sensor_data.R")
} else {
  stop("Helper file 'plot_sensor_data.R' not found.")
}

# --- 5. Shiny UI (User Interface) ---
ui <- fluidPage(
  titlePanel("Sensor Data Explorer (explore_sensors_app.R)"),

  # --- METADATA HEADER ---
  fluidRow(
    column(
      width = 12,
      tags$p(
        tags$i(paste("App query time:", format(Sys.time(), "%A, %B %d, %Y at %I:%M %p %Z"))),
        style = "text-align: right; font-size: 0.9em; color: #666;"
      )
    )
  ),
  # --- END HEADER ---

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Load Data Directory"),

      shinyFiles::shinyDirButton("site_dir_button", "Select Directory",
                                 "Please select the data directory",
                                 class = "btn-info", icon = icon("folder-open")),

      tags$p(tags$b("Selected Directory:")),
      verbatimTextOutput("selected_dir_path", placeholder = TRUE),

      hr(),

      # --- REFACTORED: All steps are now dynamic ---
      uiOutput("sidebar_steps")

    ),

    mainPanel(
      width = 9,
      # --- UPDATED: New Tab Structure ---
      tabsetPanel(
        id = "main_tabs", # Give the tabset an ID
        # --- NEW: About tab is now first ---
        tabPanel(
          "About",
          value = "about_tab", # Give it a value for selection
          uiOutput("about_page")
        ),
        tabPanel(
          "Directory Contents",
          tags$p(tags$i("This table shows all .csv files found and parsed in the selected directory.")),
          DT::dataTableOutput("directory_table") %>%
            withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          "Plot",
          plotlyOutput("interactive_plot", height = "700px") %>%
            withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          "Data Table",
          # Add save button
          downloadButton("save_table", "Save as CSV", icon = icon("save"), class = "btn-sm"),
          hr(style = "margin-top: 5px; margin-bottom: 10px;"),
          DT::dataTableOutput("data_table") %>%
            withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          "Code",
          verbatimTextOutput("code_output")
        ),
        tabPanel(
          "Console",
          verbatimTextOutput("log_output")
        )
      )
    )
  )
)

# --- 6. Shiny Server (The Logic) ---
server <- function(input, output, session) {

  # --- 6.0. Define File System Volumes ---
  volumes <- c(Home = fs::path_home(), "Working Dir" = getwd())
  if (.Platform$OS.type == "windows") {
    volumes <- c(volumes, shinyFiles::getVolumes()())
  } else {
    volumes <- c(volumes, "Root" = "/")
  }

  # --- 6.1. shinyDirChoose Logic ---
  shinyFiles::shinyDirChoose(
    input,
    "site_dir_button",
    roots = volumes,
    session = session
  )

  # --- 6.2. Reactive for Selected Path ---
  reactive_site_dir <- reactiveVal(DEFAULT_SITE_DIR)

  user_selected_dir <- reactive({
    req(input$site_dir_button)
    path <- shinyFiles::parseDirPath(volumes, input$site_dir_button)
    if (length(path) == 0) return(NULL)
    return(path)
  })

  observeEvent(user_selected_dir(), {
    path <- user_selected_dir()
    if (!is.null(path)) {
      reactive_site_dir(path) # Overwrite the default
    }
  })

  output$selected_dir_path <- renderPrint({
    path <- reactive_site_dir()
    if (is.null(path)) cat("No directory selected.") else cat(path)
  })

  # --- 6.3. Reactive Master Index ---
  reactive_master_index <- reactiveVal(NULL)

  # --- 6.4. "Load Directory" Event ---
  observeEvent(reactive_site_dir(), {

    selected_path <- reactive_site_dir()
    req(selected_path)

    if (!dir.exists(selected_path)) {
      showModal(modalDialog(
        title = "Directory Not Found",
        paste("Could not find default or selected directory:", selected_path),
        "Please select a valid directory to continue.",
        footer = modalButton("Close"), easyClose = TRUE
      ))
      reactive_master_index(NULL) # Clear the index
      return() # Stop execution
    }

    showModal(modalDialog(paste("Scanning directory:", selected_path),
                          "Building file index...",
                          footer = NULL, easyClose = FALSE))

    index <- tryCatch({
      query_sensor_files(selected_path, recursive = TRUE)
    }, error = function(e) {
      showModal(modalDialog(title = "Error", paste("Failed to read directory:", e.message),
                            footer = modalButton("Close"), easyClose = TRUE))
      return(NULL)
    })

    if (!is.null(index) && nrow(index) > 0) {
      reactive_master_index(index)
      removeModal()

    } else if (!is.null(index) && nrow(index) == 0) {
      showModal(modalDialog(
        title = "No Files Found",
        "Directory scanned successfully, but no valid sensor .csv files were found.",
        footer = modalButton("Close"), easyClose = TRUE
      ))
      reactive_master_index(NULL) # --- Clear index if no files found
    }
  }, ignoreNULL = FALSE)

  # --- 6.5. REFACTORED: Sequential Sidebar UI ---
  output$sidebar_steps <- renderUI({

    # Step 1: Wait for index
    index <- reactive_master_index()
    if (is.null(index)) {
      return(tags$p(tags$i("Please load a valid data directory index to continue..."),
                    style = "color: #888;"))
    }

    # Step 2: Show Query Filters
    # Populate choices from the index
    ALL_SITES <- sort(unique(index$site))
    ALL_FIELDS <- sort(unique(index$field))
    ALL_PLOTS <- sort(unique(index$plot))
    ALL_SENSORS <- sort(unique(index$sensor))
    ALL_SERIALS <- sort(unique(index$serial))
    ALL_PORTS <- sort(unique(index$port))

    tagList(
      h4("2. Query Data"),
      selectInput("site", "Site", choices = c("All" = "*", ALL_SITES), multiple = TRUE, selectize = TRUE),
      selectInput("field", "Field", choices = c("All" = "*", ALL_FIELDS), multiple = TRUE, selectize = TRUE),
      selectInput("plot", "Plot", choices = c("All" = "*", ALL_PLOTS), multiple = TRUE, selectize = TRUE),
      selectInput("sensor", "Sensor Type", choices = c("All" = "*", ALL_SENSORS), multiple = TRUE, selectize = TRUE),
      selectInput("serial", "Serial", choices = c("All" = "*", ALL_SERIALS), multiple = TRUE, selectize = TRUE),
      selectInput("port", "Port", choices = c("All" = "*", ALL_PORTS), multiple = TRUE, selectize = TRUE),
      dateRangeInput("date_range", "Date Range",
                     start = DEFAULT_MIN_DATE, end = DEFAULT_MAX_DATE,
                     min = DEFAULT_MIN_DATE, max = DEFAULT_MAX_DATE),

      actionButton("preview_data", "Preview Data", icon = icon("search"), class = "btn-info"),

      hr(),
      # Step 3: Column Selection (will appear after preview)
      uiOutput("column_select_ui"),

      # Step 4: Process & Plot (will appear after preview)
      uiOutput("process_plot_ui")
    )
  })

  # --- 6.6. Reactive for Raw Data Preview ---
  reactive_raw_data <- eventReactive(input$preview_data, {

    args <- build_args(step = "preview")
    if (is.null(args$site_dir)) return(NULL)

    withProgress(message = 'Loading Raw Data Preview...', value = 0, {

      incProgress(0.4, detail = "Calling process_sensor_data()...")
      data_out <- process_sensor_data(
        site_dir = args$site_dir,
        summarize = FALSE,
        output_format = "wide", # Always get wide for preview
        fun = NULL,
        site_filter = args$site_filter,
        field_filter = args$field_filter,
        plot_filter = args$plot_filter,
        sensor_filter = args$sensor_filter,
        serial_filter = args$serial_filter,
        port_filter = args$port_filter,
        group_vars = NULL,
        aggregate_interval = NULL, # No aggregation yet
        recursive = TRUE
      )

      incProgress(0.9, detail = "Filtering by date...")
      data_out <- data_out %>%
        filter(DateTime >= args$min_date, DateTime <= args$max_date)

      incProgress(1, detail = "Done.")

      if (nrow(data_out) == 0) {
        showModal(modalDialog("Query returned 0 rows.", "Please check your filters and try again.",
                              footer = modalButton("Close"), easyClose = TRUE))
        return(NULL)
      }

      # --- NEW: Update Data Table tab when preview is clicked ---
      updateTabsetPanel(session, "main_tabs", selected = "Data Table")

      return(data_out)
    })
  })

  # --- 6.7. UI for Column Selection (Step 3) ---
  output$column_select_ui <- renderUI({

    data <- reactive_raw_data()
    req(data) # Wait for preview data

    # Find all numeric measurement columns
    metadata_cols <- c("DateTime", "Site", "Field", "Plot", "Port", "SensorType", "Serial", "File")
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    measurement_cols <- setdiff(numeric_cols, metadata_cols)

    if (length(measurement_cols) == 0) {
      return(
        tagList(
          h4("3. Select Measurements"),
          tags$p(tags$i("No numeric measurement columns found in the preview data."),
                 style = "color: #888;"),
          hr()
        )
      )
    }

    tagList(
      h4("3. Select Measurements"),
      selectInput("measure_cols", "Select Columns to Analyze:",
                  choices = measurement_cols,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = measurement_cols), # <-- Changed default to all
      hr()
    )
  })

  # --- 6.8. UI for Process & Plot (Step 4) ---
  output$process_plot_ui <- renderUI({

    data <- reactive_raw_data()
    req(data) # Wait for preview data

    # --- Check for measurement columns ---
    metadata_cols <- c("DateTime", "Site", "Field", "Plot", "Port", "SensorType", "Serial", "File")
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    measurement_cols <- setdiff(numeric_cols, metadata_cols)

    if (length(measurement_cols) == 0) {
      return(NULL) # Don't show the rest of the UI
    }

    # --- This is the key: require measure_cols to be populated ---
    req(input$measure_cols)

    # --- STATIC PLOT CHOICES ---
    plot_group_choices <- c("Site", "Field", "Plot", "SensorType", "Port", "Serial", "Statistic", "Measurement")

    tagList(
      h4("4. Process Data"),
      selectInput("agg_interval", "Time Aggregation",
                  choices = c("None" = "none", "15 minute" = "15 mins",
                              "1 hour" = "1 hour", "1 day" = "1 day",
                              "1 week" = "1 week"),
                  selected = "none"),

      checkboxInput("summarize", "Summarize Data?", FALSE),

      conditionalPanel(
        condition = "input$summarize == true",
        selectInput("summary_funs", "Summary Functions",
                    choices = c("mean", "median", "min", "max", "sum", "sd"),
                    multiple = TRUE, selected = "mean"),

        selectInput("group_vars", "Group By",
                    choices = c("DateTime", "Site", "Field", "Plot", "SensorType", "Port", "Serial"),
                    multiple = TRUE,
                    selected = c("DateTime", "Field", "Plot"))
      ),

      hr(),
      h4("5. Plot Options"),

      # --- REVERTED TO STATIC ---
      selectInput("plot_color", "Color By:",
                  choices = c("None" = "NULL", plot_group_choices),
                  selected = "Plot"),

      selectInput("facet_row", "Facet Rows:",
                  choices = c("None" = "NULL", plot_group_choices),
                  selected = "NULL"),

      selectInput("facet_col", "Facet Columns:",
                  choices = c("None" = "NULL", plot_group_choices),
                  selected = "NULL"),

      hr(),
      h4("6. Run Analysis"),
      actionButton("run_analysis", "Process & Plot",
                   class = "btn-primary", icon = icon("cogs"))
    )
  })

  # --- 6.9. Helper function to build reactive arguments ---
  # This stores *all* user choices in one place
  build_args <- function(step = "analysis") {

    req(reactive_site_dir()) # Require a directory

    site_filter <- if(is.null(input$site) || "*" %in% input$site) NULL else input$site
    field_filter <- if(is.null(input$field) || "*" %in% input$field) NULL else input$field
    plot_filter <- if(is.null(input$plot) || "*" %in% input$plot) NULL else input$plot
    sensor_filter <- if(is.null(input$sensor) || "*" %in% input$sensor) NULL else input$sensor
    serial_filter <- if(is.null(input$serial) || "*" %in% input$serial) NULL else input$serial
    port_filter <- if(is.null(input$port) || "*" %in% input$port) NULL else input$port

    # Base list
    arg_list <- list(
      site_dir = reactive_site_dir(),
      site_filter = site_filter,
      field_filter = field_filter,
      plot_filter = plot_filter,
      sensor_filter = sensor_filter,
      serial_filter = serial_filter,
      port_filter = port_filter,
      min_date = input$date_range[1],
      max_date = input$date_range[2],
      recursive = TRUE
    )

    if (step == "preview") {
      return(arg_list)
    }

    # --- Add Analysis args ---
    agg_int <- if(input$agg_interval == "none") NULL else input$agg_interval
    summarize <- input$summarize

    fun_list <- NULL
    if (summarize && !is.null(input$summary_funs)) {
      fun_list <- setNames(lapply(input$summary_funs, get), input$summary_funs)
    }

    analysis_args <- list(
      summarize = summarize,
      output_format = "wide", # Always get wide for table
      fun = fun_list,
      group_vars = input$group_vars,
      aggregate_interval = agg_int,

      # Column selection
      measure_cols = input$measure_cols,

      # Plot args
      plot_color = if(input$plot_color == "NULL") NULL else input$plot_color,
      facet_row = if(input$facet_row == "NULL") NULL else input$facet_row,
      facet_col = if(input$facet_col == "NULL") NULL else input$facet_col
    )

    return(c(arg_list, analysis_args))
  }

  # --- 6.10. The Core Data Reactive ---
  # This reactive holds the *final* data for plotting/tabling
  processed_data <- eventReactive(input$run_analysis, {

    args <- build_args(step = "analysis")
    raw_data <- reactive_raw_data()
    req(raw_data)

    withProgress(message = 'Processing Data...', value = 0, {

      incProgress(0.1, detail = "Selecting columns...")
      # --- NEW: Select only the chosen columns ---
      metadata_cols <- c("DateTime", "Site", "Field", "Plot", "Port", "SensorType", "Serial", "File")
      # Ensure metadata columns exist in the data
      metadata_cols_present <- intersect(metadata_cols, names(raw_data))

      data_to_process <- raw_data %>%
        select(all_of(metadata_cols_present), all_of(args$measure_cols))

      incProgress(0.3, detail = "Applying time aggregation...")
      if (!is.null(args$aggregate_interval)) {
        data_to_process <- data_to_process %>%
          mutate(DateTime = floor_date(DateTime, args$aggregate_interval))
      }

      # --- Summarization (if requested) ---
      if (args$summarize == TRUE) {
        incProgress(0.6, detail = "Summarizing data...")

        numeric_cols <- args$measure_cols
        valid_group_vars <- intersect(args$group_vars, names(data_to_process))
        if (length(valid_group_vars) == 0) stop("No valid group_vars found.")

        if (is.list(args$fun)) {
          # --- List of functions ---
          summarized_wide <- data_to_process %>%
            group_by(across(all_of(valid_group_vars))) %>%
            summarise(
              across(all_of(numeric_cols), args$fun, na.rm = TRUE, .names = "{.col}___{.fn}"),
              .groups = "drop"
            )

          summarized_long <- summarized_wide %>%
            tidyr::pivot_longer(
              cols = !all_of(valid_group_vars),
              names_to = c("Measurement", "Statistic"),
              names_sep = "___",
              values_to = "Value"
            )

          data_out <- summarized_long %>%
            tidyr::pivot_wider(
              names_from = "Measurement",
              values_from = "Value"
            )

        } else if (is.function(args$fun)) {
          # --- Single function ---
          data_out <- data_to_process %>%
            group_by(across(all_of(valid_group_vars))) %>%
            summarise(across(all_of(numeric_cols), ~ args$fun(.x, na.rm = TRUE)), .groups = "drop")
        }
      } else {
        # No summary, just return the column-selected, time-aggregated data
        data_out <- data_to_process
      }

      incProgress(1, detail = "Done.")
      return(data_out)
    })
  })

  # --- 6.11. Reactive for Dynamic Plot Choices (REMOVED) ---

  # --- 6.12. Render Dynamic Plot UIs (REMOVED) ---

  # --- 6.13. Render Final Outputs ---

  # Render the plot
  output$interactive_plot <- renderPlotly({
    data_to_plot <- processed_data()
    req(data_to_plot, nrow(data_to_plot) > 0)

    args <- build_args("analysis")

    plot_sensor_data(
      data = data_to_plot,
      x = "DateTime",
      color = args$plot_color,
      facet_row = args$facet_row,
      facet_col = args$facet_col,
      interactive = TRUE
    )
  })

  # Render the data table
  output$data_table <- DT::renderDataTable({
    # --- UPDATED: Also show preview data ---
    data_to_show <- if (input$preview_data == 0) {
      NULL # Don't show anything on startup
    } else {
      # Show the raw preview *until* processed data is ready
      if (input$run_analysis == 0) {
        reactive_raw_data()
      } else {
        processed_data()
      }
    }

    req(data_to_show, nrow(data_to_show) > 0)

    DT::datatable(
      data_to_show,
      options = list(scrollX = TRUE, pageLength = 25, dom = 'lfrtip'),
      rownames = FALSE,
      filter = "top"
    )
  })

  # Render the Directory Contents Table
  output$directory_table <- DT::renderDataTable({
    index_data <- reactive_master_index()
    req(index_data)

    data_to_show <- index_data %>%
      select(file_name, site, field, plot, serial, port, sensor)

    DT::datatable(
      data_to_show,
      options = list(scrollX = TRUE, pageLength = 25, dom = 'lfrtip'),
      rownames = FALSE,
      filter = "top"
    )
  })

  # Render the log
  output$log_output <- renderPrint({
    req(input$run_analysis) # Only show after run
    args <- build_args("analysis")

    paste(
      "--- Query Log ---",
      paste("\nTimestamp:", Sys.time()),
      paste("\nSite Dir:", args$site_dir),
      paste("\nFilters:"),
      paste("  Site:", paste(args$site_filter, collapse = ", ")),
      paste("  Field:", paste(args$field_filter, collapse = ", ")),
      paste("  Plot:", paste(args$plot_filter, collapse = ", ")),
      paste("  Sensor:", paste(args$sensor_filter, collapse = ", ")),
      paste("  Serial:", paste(args$serial_filter, collapse = ", ")),
      paste("  Port:", paste(args$port_filter, collapse = ", ")),
      paste("\nProcessing:"),
      paste("  Selected Columns:", paste(args$measure_cols, collapse = ", ")),
      paste("  Aggregate Interval:", args$aggregate_interval),
      paste("  Summarize:", args$summarize),
      paste("  Functions:", paste(names(args$fun), collapse = ", ")),
      paste("  Group By:", paste(args$group_vars, collapse = ", ")),
      paste("\nPlot Options:"),
      paste("  Color By:", args$plot_color),
      paste("  Facet Row:", args$facet_row),
      paste("  Facet Col:", args$facet_col)
    )
  })

  # Render the code
  output$code_output <- renderPrint({
    req(input$run_analysis)
    args <- build_args("analysis")

    format_arg <- function(arg) {
      if (is.null(arg)) return("NULL")
      if (is.character(arg) && length(arg) > 1) {
        return(paste0("c('", paste(arg, collapse = "', '"), "')"))
      }
      if (is.character(arg)) return(paste0("'", arg, "'"))
      if (is.list(arg)) return(paste0("list(", paste(names(arg), "=", names(arg), collapse = ", "), ")"))
      return(as.character(arg))
    }

    # Get the *actual* metadata columns present in the raw data
    raw_data <- reactive_raw_data()
    req(raw_data)
    metadata_cols_present <- intersect(c("DateTime", "Site", "Field", "Plot", "Port", "SensorType", "Serial", "File"), names(raw_data))


    code_string <- paste(
      "# --- Code to Replicate Query ---",

      if (is.list(args$fun)) {
        paste(names(args$fun), "=", names(args$fun), collapse = "\n")
        paste("fun_list <-", format_arg(args$fun), "\n")
      } else { "fun_list <- NULL" },

      "\n# Step 1: Get Raw, Filtered Data",
      "raw_data <- process_sensor_data(",
      paste0("  site_dir = '", args$site_dir, "',"),
      paste0("  summarize = FALSE,"),
      paste0("  output_format = 'wide',"),
      paste0("  fun = NULL,"),
      paste0("  site_filter = ", format_arg(args$site_filter), ","),
      paste0("  field_filter = ", format_arg(args$field_filter), ","),
      paste0("  plot_filter = ", format_arg(args$plot_filter), ","),
      paste0("  sensor_filter = ", format_arg(args$sensor_filter), ","),
      paste0("  serial_filter = ", format_arg(args$serial_filter), ","),
      paste0("  port_filter = ", format_arg(args$port_filter), ","),
      paste0("  aggregate_interval = NULL,"),
      paste0("  recursive = TRUE"),
      ")",

      "\n\n# Step 2: Filter by Date",
      "data_filtered <- raw_data %>%",
      paste0("  filter(DateTime >= as.Date('", args$min_date, "'),"),
      paste0("         DateTime <= as.Date('", args$max_date, "'))"),

      "\n\n# Step 3: Select, Aggregate, and Summarize",
      paste0("metadata_cols <- ", format_arg(metadata_cols_present)),
      paste0("measure_cols <- ", format_arg(args$measure_cols)),

      "\ndata_processed <- data_filtered %>%",
      "  select(all_of(metadata_cols), all_of(measure_cols))",

      if (!is.null(args$aggregate_interval)) {
        paste0("\n  mutate(DateTime = floor_date(DateTime, '", args$aggregate_interval, "'))")
      } else { "" },

      if (args$summarize == TRUE) {
        paste(
          "\n  group_by(across(all_of(", format_arg(args$group_vars), ")))",
          paste0("\n  summarise("),
          if (is.list(args$fun)) {
            paste0("    across(all_of(measure_cols), fun_list, na.rm = TRUE, .names = '{.col}___{.fn}'),")
          } else {
            # Attempt to deparse the function
            fun_text <- deparse(args$fun)
            if (length(fun_text) > 1 || grepl("function", fun_text)) {
              fun_text <- "args$fun" # Fallback for complex functions
            }
            paste0("    across(all_of(measure_cols), ~ ", fun_text ,"(.x, na.rm = TRUE)),")
          },
          "    .groups = 'drop'",
          "\n  )"
        )
      } else { "" },

      # Handle pivoting
      if (args$summarize == TRUE && is.list(args$fun)) {
        paste(
          "\ndata_processed <- data_processed %>%",
          "  tidyr::pivot_longer(",
          "    cols = !all_of(", format_arg(args$group_vars), "),",
          "    names_to = c('Measurement', 'Statistic'),",
          "    names_sep = '___',",
          "    values_to = 'Value'",
          "  ) %>%",
          "  tidyr::pivot_wider(",
          "    names_from = 'Measurement',",
          "    values_from = 'Value'",
          "  )"
        )
      } else { "" },

      "\n\n# --- Code to Replicate Plot ---",
      "# (Note: plot_sensor_data function must be defined in your environment)",
      "plot_object <- plot_sensor_data(",
      "  data = data_processed,",
      "  x = 'DateTime',",
      paste0("  color = ", format_arg(args$plot_color), ","),
      paste0("  facet_row = ", format_arg(args$facet_row), ","),
      paste0("  facet_col = ", format_arg(args$facet_col), ","),
      "  interactive = FALSE",
      ")",
      "\n\nprint(plot_object)",
      sep = "\n"
    )

    cat(code_string)
  })

  # Render the About page
  output$about_page <- renderUI({
    tagList(
      h3("About This Application"),
      tags$p("This application (explore_sensors_app.R) is designed to facilitate the exploration of processed sensor data."),
      tags$p("It provides a user interface for dynamically querying, summarizing, and plotting data based on file metadata."),

      # --- NEW: Instructions ---
      h4("Instructions for Use"),
      tags$ol(
        tags$li(tags$b("Step 1: Load Directory"), " - Use the 'Select Directory' button to navigate to the folder containing your processed .csv sensor files (e.g., '.../processed/'). The app will scan this folder and populate the filters."),
        tags$li(tags$b("Step 2: Query Data"), " - Use the filters to select the subset of data you wish to analyze. You can filter by site, plot, sensor, etc. Select a date range."),
        tags$li(tags$b("Step 3: Preview Data"), " - Click the 'Preview Data' button. This loads the raw data for your query, which populates the 'Directory Contents' and 'Data Table' tabs and enables the next steps."),
        tags$li(tags$b("Step 4: Select Measurements"), " - Choose the specific measurement columns (e.g., 'Soil_Temp_degC') you want to analyze or plot."),
        tags$li(tags$b("Step 5: Process Data"), " - (Optional) Choose a time aggregation (e.g., '1 day') and/or select 'Summarize Data' to calculate statistics (e.g., mean, max)."),
        tags$li(tags$b("Step 6: Plot Options"), " - Choose which data columns to use for plot aesthetics like 'Color By', 'Facet Rows', and 'Facet Columns'. These dropdowns update based on your processing choices."),
        tags$li(tags$b("Step 7: Run Analysis"), " - Click 'Process & Plot' to apply your processing steps and generate the final plot and data table.")
      ),

      h4("Data Workflow"),
      tags$ol(
        tags$li(tags$b("map_z6_info_and_save_files.R:"), " Pre-processes raw meter data and saves standardized CSV files."),
        tags$li(tags$b("query_sensor_files.R:"), " Scans the directory of processed files and builds a metadata index based on the file naming convention."),
        tags$li(tags$b("process_sensor_data.R:"), " Uses the metadata index to load, filter, aggregate, and/or summarize the data on demand."),
        tags$li(tags$b("explore_sensors_app.R:"), " This Shiny app, which provides a UI to control the other scripts and visualize the results.")
      ),

      h4("Data Source"),
      tags$p("The application reads data from the processed data directory, which is assumed to be in the following location relative to the scripts:"),
      tags$code("~/Workspace/uvmgitlab/dswr/dswrEMMA/data/"),
      tags$p("This path is selected by the user upon starting the app.")
    )
  })

  # --- 6.14. Download Handlers ---

  # Save the data table
  output$save_table <- downloadHandler(
    filename = function() {
      paste0("sensor_data_summary_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      data_to_save <- processed_data() # This depends on the *final* data
      req(data_to_save)
      readr::write_csv(data_to_save, file)
    }
  )

}

# --- 7. Run the App ---
shinyApp(ui = ui, server = server)
