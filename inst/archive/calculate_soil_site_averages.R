source("~/Workspace/uvmgitlab/dswr/dswrEMMA/data/query_sensor_files.R")
source("~/Workspace/uvmgitlab/dswr/dswrEMMA/data/process_sensor_data.R")

my_dir <- "~/Workspace/uvmgitlab/dswr/dswrEMMA/data/VB/Meter/processed"

# Define the summary functions
summary_funs <- list(
  mean = mean,
  min = min,
  max = max
)

# Run the query
vb_east_summary_wide <- process_sensor_data(
  site_dir = my_dir,
  summarize = TRUE,
  fun = summary_funs,
  aggregate_interval = "1 day",
  site_filter = "VB",
  field_filter = "east",
  plot_filter = c("high", "low"),
  sensor_filter = c("SO411", "TEROS11"), # Filter for only these sensors
  group_vars = c("DateTime", "Plot"),    # Group by Day and Plot
  pivot_groups_wider = c("Plot", "Statistic") # Create the wide column names
)

# View the result
print(head(vb_east_summary))

# Run the query
vb_east_summary_long <- process_sensor_data(
  site_dir = my_dir,
  summarize = TRUE,
  fun = summary_funs,
  aggregate_interval = "1 day",
  site_filter = "VB",
  field_filter = "east",
  plot_filter = c("high", "low"),
  sensor_filter = c("SO411", "TEROS11"), # Filter for only these sensors
  group_vars = c("DateTime", "Plot")    # Group by Day and Plot
  #pivot_groups_wider = c("Plot", "Statistic") # Create the wide column names
)

# Plot the summarized data
plot_sensor_data(
  vb_east_summary_long,
  x = "DateTime",
  color = "Statistic",       # Will create "high" and "low" lines
  facet_row = "Measurement",  # Will create "mean", "min", and "max" rows
  facet_col = "Plot", # The function finds this automatically
  interactive=F
  )
