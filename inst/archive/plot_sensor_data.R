# --- 3. The Plotting Function ---
# (This function is unchanged from the previous version)
plot_sensor_data <- function(data,
                             x = "DateTime",
                             color = "Plot",
                             facet_row = NULL,
                             facet_col = NULL,
                             warn_over_rows = 100000,
                             downsample_n = 2000, # Downsample by default
                             interactive = TRUE,
                             ...) {

  # --- 4.1. Auto-Pivot to Tidy (Long) Format ---
  METADATA_COLS <- c("DateTime", "Site", "Field", "Plot", "Port",
                     "SensorType", "Serial", "File", "Statistic")

  data_long <- data

  if (!"Value" %in% names(data_long)) {
    id_vars <- intersect(METADATA_COLS, names(data_long))
    if (length(id_vars) == 0) stop("Input data is wide, but no metadata columns found.")
    value_vars <- setdiff(names(data_long), id_vars)
    if (length(value_vars) == 0) stop("Input data is wide, but no measurement columns found.")

    message("Pivoting 'wide' data to 'long' format for plotting.")
    data_long <- data_long %>%
      tidyr::pivot_longer(
        cols = all_of(value_vars),
        names_to = "Measurement",
        values_to = "Value"
      )
  }

  # --- 4.2. Set Smart Facet Defaults ---
  if (is.null(facet_row)) {
    if ("Statistic" %in% names(data_long)) {
      facet_row <- "Statistic"
      message("Defaulting to `facet_row = \"Statistic\"`.")
    } else if ("Measurement" %in% names(data_long)) {
      facet_row <- "Measurement"
      message("Defaulting to `facet_row = \"Measurement\"`.")
    }
  }

  # --- 4.3. Warn and Down-sample ---
  if (nrow(data_long) > warn_over_rows) {
    warning(paste("Dataset has", nrow(data_long), "rows. Plotting may be slow."))
  }

  if (!is.null(downsample_n) && nrow(data_long) > downsample_n) {
    message(paste("Down-sampling data to", downsample_n, "points per group..."))

    grouping_vars <- c(color, facet_row, facet_col, "Measurement") # Ensure Measurement is a group
    grouping_vars <- unique(grouping_vars[!is.null(grouping_vars)])

    if (length(grouping_vars) == 0) {
      n_rows <- nrow(data_long)
      if (n_rows > downsample_n) {
        data_long <- data_long %>%
          slice(round(seq(1, n(), length.out = min(n(), downsample_n))))
      }
    } else {
      # Ensure all grouping vars exist in the data
      grouping_vars <- intersect(grouping_vars, names(data_long))

      data_long <- data_long %>%
        group_by(across(all_of(grouping_vars))) %>%
        slice(round(seq(1, n(), length.out = min(n(), downsample_n)))) %>%
        ungroup()
    }

    message(paste("Data reduced to", nrow(data_long), "total rows for plotting."))
  }

  # --- 4.4. Build the ggplot ---
  plot_vars <- c(x, "Value", color, facet_row, facet_col)
  missing_vars <- setdiff(plot_vars, c(names(data_long), NULL))
  if (length(missing_vars) > 0) {
    stop(paste("Missing plot columns:", paste(missing_vars, collapse = ", ")))
  }

  p <- ggplot(data_long, aes(x = .data[[x]], y = .data[["Value"]]))

  # Create a unique group interaction for line connection
  group_aes <- setdiff(c(color, facet_row, facet_col, "Measurement"), NULL)

  # Ensure all group_aes are valid column names
  group_aes <- intersect(group_aes, names(data_long))

  if (!is.null(color) && color %in% names(data_long)) {
    p <- p + geom_line(aes(color = .data[[color]],
                           group = interaction(!!!syms(group_aes))), ...)
  } else {
    p <- p + geom_line(aes(group = interaction(!!!syms(group_aes))), ...)
  }

  # --- 4.5. Add Facets ---
  row_var <- if (!is.null(facet_row)) facet_row else "."
  col_var <- if (!is.null(facet_col)) facet_col else "."

  if (row_var != "." || col_var != ".") {
    facet_formula_str <- paste(row_var, "~", col_var)
    p <- p + facet_grid(as.formula(facet_formula_str), scales = "free_y")
  }

  # --- 4.6. Theming and Labels ---
  p <- p +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = x, y = "Value", color = color)

  # --- 4.7. Return Plotly or ggplot ---
  if (interactive) {
    tooltip_vars <- c(x, "Value")
    if (!is.null(color)) tooltip_vars <- c(tooltip_vars, color)
    return(plotly::ggplotly(p, tooltip = tooltip_vars))
  } else {
    return(p)
  }
}
