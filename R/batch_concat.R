#' Batch Concatenation Utility for Sensor Data
#'
#' This script provides a collection of functions to facilitate the processing,
#' filtering, grouping, and concatenation of sensor data files. It is designed
#' to handle large datasets with multiple files, enabling efficient data
#' aggregation and preparation for analysis. Key functionalities include:
#'
#' - Searching for files in a directory with a specific pattern and depth.
#' - Filtering files based on inclusion and exclusion patterns.
#' - Grouping files by identifiers (e.g., serial numbers).
#' - Reading and concatenating data from multiple files into a single dataset.
#' - Saving the combined data in multiple formats (e.g., CSV, Parquet).
#'
#' This script is intended to be part of a standalone package for sensor data
#' quality control and preprocessing. It supports parallel processing for
#' improved performance on large datasets.
#'
#' @note Ensure that required libraries (`data.table`, `stringr`, `arrow`, etc.)
#' are installed before using these functions.
#'
#' @author Adrian Wiegman
#' @date 2026
#' @license MIT
#'
#' @examples
#' # Example usage:
#' input_dir <- "data/usr/inputs/METER/Data_archive"
#' output_dir <- "data/usr/outputs/concat_data"
#' concatenate_files_by_serial(input_dir, output_dir, use_parallel = TRUE, save_parquet = TRUE)

#' Search for files with a specific extension within a directory up to a given depth
#'
#' @param dir The directory to search in (default is the current directory).
#' @param pattern The file name pattern to match (e.g., ".csv").
#' @param max_depth The maximum depth to search in subdirectories (default is 2).
#' @return A character vector of file paths matching the given extension.
#' @examples
#' search_files_by_extension("data", "csv", max_depth = 3)
#' @export
search_files_limited_depth <- function(dir = ".", pattern = ".csv", max_depth = 2) {
    # Normalize the directory path
    dir <- normalizePath(dir, winslash = "/")
    message("Normalized directory path: ", dir)

    # Initialize an empty vector to store matching files
    matching_files <- character()

    # Iteratively update the glob pattern to go deeper
    for (depth in seq_len(max_depth+1)) {
        glob_pattern <- paste0(dir, strrep("/*", depth), pattern)
        message("Generated glob pattern for depth ", depth, ": ", glob_pattern)

        # Append the found files to the list
        matching_files <- c(matching_files, Sys.glob(glob_pattern))
    }

    # Remove duplicates from the file list
    matching_files <- unique(matching_files)
    # Print the total number of files
    message("Total number of files: ", length(matching_files))

    # Print the first 10 files, each on a new line
    message("First 10 files:\n", paste(head(matching_files, 10), collapse = "\n"))

    return(matching_files)
}

fread_delimited_list <- function(file_paths, fread_args = list(), use_parallel = FALSE) {
    library(data.table)  # Ensure data.table is loaded

    # Read all files into a named list
    data_list <- if (use_parallel) {
        library(parallel)
        # Use mclapply for parallel reading
        parallel::mclapply(file_paths, function(file) {
            do.call(fread, c(list(input = file), fread_args))
        }, mc.cores = detectCores())
    } else {
        # Use lapply for sequential reading
        lapply(file_paths, function(file) {
            do.call(fread, c(list(input = file), fread_args))
        })
    }

    # Assign names to the list based on file names without extensions
    names(data_list) <- sapply(file_paths, function(file) tools::file_path_sans_ext(basename(file)))
    return(data_list)
}

read_csv_list_base <- function(csv_files, read_csv_args = list(), use_parallel = FALSE) {
  # Helper function to read a single file
  read_file <- function(file) {
    do.call(read.csv, c(list(file = file), read_csv_args))
  }

  if (use_parallel) {
    # Use parallel processing if requested
    library(parallel)
    data_list <- mclapply(csv_files, read_file, mc.cores = detectCores())
  } else {
    # Sequentially read files
    data_list <- lapply(csv_files, read_file)
  }

  # Assign names to the list based on file names without extensions
  names(data_list) <- sapply(csv_files, function(file) tools::file_path_sans_ext(basename(file)))
  return(data_list)
}

rename_z6_columns <- function(file_path, sensor_lookup = list(), param_lookup = list()) {
  lookup <- function(x,table){
    ifelse(x %in% names(table),table[[x]],gsub(" ", "_", x))
  }
    # Read the header rows to extract port, sensor, and parameter information
    lines <- readLines(file_path,6)
    print(lines)
    port <- trimws(strsplit(lines[1], ",")[[1]])
    sensor <- trimws(strsplit(lines[2], ",")[[1]])
    param <- trimws(strsplit(lines[3], ",")[[1]])
    print(port)
    print(sensor)
    print(param)
    new_names <- param # set param as default column name
    #print(new_names)
    # Assign new column names
    for (i in seq_along(new_names)){
      j <- port[i]
      s <- sensor[i]
      p <- param[i]
      if (str_detect(j, "Port")) {
        # Use the lookup table to find the column name
        s <- lookup(s,sensor_lookup)
        #print(s)
        p <- lookup(p,param_lookup)
        new_names[i] <- paste(j,s,p,sep="_")
        #print(p)
      }
    }
    print(new_names)
    return(new_names)
}


assign_column_names_df <- function(df, prefix = "Port", lookup_table = list()) {
  column_names = colnames(df)
  # Assign new column names
  new_column_names <- sapply(seq_along(column_names), function(i) {
    if (i == 1) return(column_names[i])
    else {
      # Use the lookup table to find the column name
      lookup_name <- lookup_table[[column_names[i]]]
      # If a lookup name exists, use it; otherwise, default to the original name
      if (!is.null(lookup_name)) {
        return(paste0(prefix, i - 1, "_", lookup_name))
      } else {
        # Default to the original column name with spaces replaced by underscores
        return(paste0(prefix, i - 1, "_", gsub(" ", "_", original_name)))
      }
    }
  })

  # Assign the new column names to the data frame
  colnames(df) <- new_column_names
  return(df)
}

#' Filter Strings with Match and Omit Patterns
#'
#' This function filters a vector or list of strings based on match patterns
#' and omit patterns.
#'
#' @param strings A character vector or list of strings to filter.
#' @param match_patterns A character vector of patterns to match in the strings.
#'   Only strings containing any of these patterns will be included. Default is `NULL`.
#' @param omit_patterns A character vector of patterns to omit from the strings.
#'   Strings containing any of these patterns will be excluded. Default is `NULL`.
#'
#' @return A character vector of strings that match the specified criteria.
#'
#' @import stringr
#'
#' @examples
#' # Filter strings that contain "data" but exclude those containing "temp".
#' filter_strings_match_omit(
#'   strings = c("data_file.csv", "temp_file.csv", "other_data.csv"),
#'   match_patterns = c("data"),
#'   omit_patterns = c("temp")
#' )
#'
#' @export
filter_strings_match_omit <- function(strings, match_patterns = NULL, omit_patterns = NULL,unique_basename=FALSE) {
    # Load necessary library
    library(stringr)

    # Filter strings by match_patterns if provided
    if (!is.null(match_patterns)) {
        match_regex <- paste(match_patterns, collapse = "|")
        strings <- strings[sapply(strings, function(string) {
            any(str_detect(string, match_regex))
        })]
    }

    # Exclude strings containing omit_patterns if provided
    if (!is.null(omit_patterns)) {
        omit_regex <- paste(omit_patterns, collapse = "|")
        strings <- strings[!sapply(strings, function(string) {
            any(str_detect(string, omit_regex))
        })]
    }

    # Remove duplicate base names
    if (unique_basename) {
        strings <- unique(strings[!duplicated(sapply(strings, function(file) {
            tools::file_path_sans_ext(basename(file))
        }))])
    }
    return(strings)
}

#' Group Strings by Identifier
#'
#' This function groups a vector of strings based on a set of match identifiers.
#' Each identifier is used to find matching strings, and the results are grouped
#' into a named list where the names correspond to the identifiers.
#'
#' @param strings A character vector containing the strings to be grouped.
#' @param match_identifiers A character vector of identifiers used to match and group the strings.
#'
#' @return A named list where each element is a character vector of strings
#'         that match the corresponding identifier.
#'
#' @examples
#' strings <- c("apple_pie", "banana_bread", "cherry_tart", "apple_crisp")
#' match_identifiers <- c("apple", "banana", "cherry")
#' group_strings_by_identifier(strings, match_identifiers)
#' # Returns:
#' # $apple
#' # [1] "apple_pie" "apple_crisp"
#' #
#' # $banana
#' # [1] "banana_bread"
#' #
#' # $cherry
#' # [1] "cherry_tart"
#'
#' @importFrom stringr str_detect
#' @export
group_strings_by_identifier <- function(strings, identifiers) {
    # Create a list of lists where each sublist contains strings matching a specific identifier
    grouped_strings <- lapply(identifiers, function(identifier) {
        matching_strings <- strings[sapply(strings, function(string) {
            str_detect(string, identifier)
        })]
        return(matching_strings)
    })
    names(grouped_strings) <- identifiers
    return(grouped_strings)
}

#' Concatenate or Join a List of Data Frames
#'
#' This function processes a list of data frames, ensuring they meet specific requirements,
#' and either concatenates them into a single data frame or provides options for joining
#' them based on common columns.
#'
#' @param data_list A named list of data frames to be concatenated or joined.
#'   - All elements in the list must be data frames.
#'   - Each element in the list must have a unique name.
#'
#' @return A single data frame or a list containing joined and unjoined elements:
#'   - If all data frames have matching column names and types, they are concatenated row-wise,
#'     with an additional column (`source_name`) indicating the original list element name.
#'   - If column names do not match, the function checks for common columns and provides
#'     options to perform a "left" or "full" join.
#'   - If no common columns are found or the user skips the join operation, the original list
#'     of data frames is returned.
#'
#' @details
#' The function performs the following steps:
#'   1. Validates that all elements in `data_list` are data frames and are named.
#'   2. Checks if column names and types match across all data frames.
#'   3. If columns match, concatenates the data frames row-wise.
#'   4. If columns do not match, identifies common columns and provides options for joining:
#'      - "Left join": Keeps all rows from the first data frame and matches rows from others.
#'      - "Full join": Combines all rows from all data frames, matching on common columns.
#'      - "Select join": Allows the user to select specific elements to join.
#'   5. If no common columns are found, returns the original list of data frames.
#'
#' @examples
#' # Example usage:
#' df1 <- data.frame(a = 1:3, b = letters[1:3])
#' df2 <- data.frame(a = 4:6, b = letters[4:6])
#' df_list <- list(df1 = df1, df2 = df2)
#' result <- concatenate_data_frames(df_list)
#'
#' @note If the user opts for a join operation, a preview of the resulting data frame
#' is displayed before returning the result.
#'
#' @importFrom utils head
#' @export
concatenate_data_frames <- function(data_list) {
  # Validate input
  if (!all(sapply(data_list, is.data.frame))) {
    stop("All elements in 'data_list' must be data frames.")
  }
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    stop("All elements in 'data_list' must be named.")
  }

  # Attempt to bind rows safely
  try_bind_rows <- function(df_list) {
    tryCatch({
      return(do.call(dplyr::bind_rows, lapply(names(df_list), function(name) {
        df <- df_list[[name]]
        df$source_name <- name
        return(df)
      })))
    }, error = function(e) {
      message("Error in bind_rows: ", e$message, "\nFalling back to alternative methods.")
      return(NULL)
    })
  }

  # Try to bind rows first
  combined_data <- try_bind_rows(data_list)
  if (!is.null(combined_data)) return(combined_data)

  # Column name and type analysis
  column_info <- lapply(data_list, function(df) vapply(df, class, character(1)))
  first_col_info <- column_info[[1]]

  column_match <- all(sapply(column_info, function(cols) {
    identical(sort(names(cols)), sort(names(first_col_info))) &&
      all(sapply(names(cols), function(name) cols[name] == first_col_info[name], USE.NAMES = FALSE))
  }))

  if (column_match) {
    message("Column names match but bind_rows failed. Returning original list.")
    return(data_list)
  }

  # Identify common columns
  common_columns <- Reduce(intersect, lapply(data_list, colnames))
  if (length(common_columns) == 0) {
    message("No common columns found. Returning the original list.")
    return(data_list)
  }

  # Join options
  message("Common columns found: ", paste(common_columns, collapse = ", "))
  user_choice <- readline(prompt = "Would you like a 'left', 'full', or 'select' join? (Enter 'left', 'full', 'select', or 'skip'): ")

  perform_join <- function(df_list, join_type) {
    join_fun <- switch(join_type,
                       "left" = function(x, y) merge(x, y, by = common_columns, all.x = TRUE),
                       "full" = function(x, y) merge(x, y, by = common_columns, all = TRUE))
    Reduce(join_fun, df_list)
  }

  if (tolower(user_choice) %in% c("left", "full")) {
    joined_data <- perform_join(data_list, tolower(user_choice))
    message("Preview of joined data:")
    print(head(joined_data, 5))
    return(joined_data)
  }

  if (tolower(user_choice) == "select") {
    message("Available elements: ", paste(names(data_list), collapse = ", "))
    selected_elements <- strsplit(readline(prompt = "Enter names of elements to join, separated by commas: "), ",")[[1]]
    selected_elements <- trimws(selected_elements)

    if (all(selected_elements %in% names(data_list))) {
      selected_data <- data_list[selected_elements]
      joined_data <- perform_join(selected_data, "full")
      unjoined_data <- data_list[setdiff(names(data_list), selected_elements)]
      return(list(joined = joined_data, unjoined = unjoined_data))
    } else {
      message("Invalid selection. Returning the original list.")
    }
  }

  message("Skipping join operation. Returning the original list.")
  return(data_list)
}

#' Save Combined Data
#'
#' This helper function saves combined data frames to CSV files and optionally as RData.
#' It can optionally use `mclapply` for parallel processing.
#'
#' @param combined_data A named list of data frames to save.
#' @param output_dir Character. The directory where the files will be saved.
#' @param save_Rdata Logical. Whether to save the combined data as an RData file.
#' @param use_parallel Logical. Whether to use `mclapply` from the `parallel` package for parallel processing.
#'
#' @return None. The function saves the data to the specified directory.
#'
#' @export
save_combined_data <- function(combined_data, output_dir, save_Rdata = FALSE, use_parallel = FALSE) {
    # Create the output directory if it doesn't exist
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Save each data frame in the list as a CSV file
    if (use_parallel) {
        library(parallel)
        mclapply(names(combined_data), function(group_name) {
            output_file <- file.path(output_dir, paste0(group_name, "_combined.csv"))
            write.csv(combined_data[[group_name]], output_file, row.names = FALSE)
        }, mc.cores = detectCores())
    } else {
        lapply(names(combined_data), function(group_name) {
            output_file <- file.path(output_dir, paste0(group_name, "_combined.csv"))
            write.csv(combined_data[[group_name]], output_file, row.names = FALSE)
        })
    }

    # Optionally save the entire list as an RData file
    if (save_Rdata) {
        save(combined_data, file = file.path(output_dir, "combined_data.RData"))
    }
}


#' Read and Concatenate Data
#'
#' This function searches for files in a specified directory, filters them based on match and omit patterns,
#' groups them by identifiers, and concatenates the data into combined data frames for further analysis.
#'
#' @param input_dir Character. The input directory containing the files to process.
#' @param match_patterns Character vector. Patterns to match in file names for inclusion.
#' @param omit_patterns Character vector. Patterns to omit in file names for exclusion.
#' @param output_dir Character. The directory where the combined data files will be saved.
#' @param save_Rdata Logical. Whether to save the combined data as RData files.
#' @param group_ids Character vector. Identifiers used to group files.
#' @param group_by_match_pattern Logical. Whether to group files by match patterns.
#' @param read_csv_args List. Additional arguments to pass to `read.csv`.
#' @param use_parallel Logical. Whether to use `mclapply` from the `parallel` package for parallel processing.
#'
#' @return None. The function saves the combined data files in the specified output directory.
#'
#' @examples
#' \dontrun{
#' read_and_concatenate_data(
#'   input_dir = "data/input",
#'   match_patterns = c("z6"),
#'   omit_patterns = c("Raw"),
#'   output_dir = "data/output",
#'   save_Rdata = TRUE,
#'   group_ids = c("z6", "z7"),
#'   group_by_match_pattern = TRUE,
#'   use_parallel = TRUE
#' )
#' }
#'
#' @export
read_and_concatenate_data <- function(input_dir, match_patterns, omit_patterns, output_dir, save_Rdata = FALSE, group_ids, group_by_match_pattern = FALSE, read_csv_args = list(), use_parallel = FALSE) {
    # Step 1: Search for all files in the folder
    all_files <- search_files_limited_depth(input_dir, pattern = ".*", max_depth = 2)

    # Step 2: Filter files to include only relevant ones
    filtered_files <- filter_strings_match_omit(all_files, match_patterns, omit_patterns)

    # Step 3: Group files by identifiers if required
    if (group_by_match_pattern) {
        grouped_files <- group_strings_by_identifier(filtered_files, group_ids)
        combined_data <- lapply(grouped_files, function(files) {
            # Read CSV files into a list of data frames
            data_list <- read_csv_list(files, read_csv_args, use_parallel)
            # Combine the data frames into one
            do.call(rbind, data_list)
        })
    } else {
        # If no grouping, read all files into a single combined data frame
        combined_data <- list(all = {
            data_list <- read_csv_list(filtered_files, read_csv_args, use_parallel)
            do.call(rbind, data_list)
        })
    }

    # Step 4: Save the combined data for future analysis
    save_combined_data(combined_data, output_dir, save_Rdata, use_parallel)
}
