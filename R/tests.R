# Provide paths to Excel files
files <- dir("~/rawdata/abluft2/labdata/xlsx", "xlsx$", full.names = TRUE)

# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  # 1. Source main.R first!
  # 2. Source this script
  
  file_database <- kwb.file::to_file_database(files)

  # Get all tables from one file
  tables <- kwb.readxl:::get_text_tables_from_xlsx(
    file = files[1L], 
    ids_as_names = TRUE # to name elements according to table_ids (below)!
  )

  # Get table metadata (just read an attribute)
  table_info <- kwb.readxl:::get_table_info(tables)

  # Create column metadata from the table headers
  column_info <- kwb.readxl:::create_column_metadata(tables)

  # Add column "skip": If the user puts an "x" into this column, the
  # corresponding table will not be imported.
  table_info$skip <- ""

  # Set "file" attribute in table_info
  attr(table_info, "file") <- kwb.utils::getAttribute(tables, "file")
  
  # Write table metadata to "<basename>_META.csv"
  kwb.readxl:::export_table_metadata(table_info, col_types = FALSE)
  
  # import_table_metadata returns NULL if no metadata file exists
  kwb.readxl:::import_table_metadata(files[5])

  # Select all file indices
  indices <- seq_along(files)
  indices <- 1:2
  
  # Change indices to test with less files
  #indices <- 16
  
  # Clear the screen
  kwb.utils::clearConsole()

  # Get all tables from all files
  system.time(all_tables <- lapply(indices, function(index) {
    cat("File index:", index)
    kwb.readxl:::get_text_tables_from_xlsx(files[index], ids_as_names = TRUE)
  }))

  #   user  system elapsed 
  # 93.604   2.936  97.305   
  
  names(all_tables) <- file_database$files$file_id[indices]
  
  # Create column metadata for all tables
  column_info_list <- lapply(all_tables, kwb.readxl:::create_column_metadata)

  column_info <- kwb.utils::rbindAll(
    column_info_list,
    nameColumn = "file_id", 
    namesAsFactor = FALSE
  )
  
  x <- kwb.readxl:::compact_column_info(column_info)

  nrow(x)
  # 6141
  
  column_info <- kwb.readxl:::suggest_column_name(column_info)
  
  column_info <- merge(column_info, file_database$files)
  
  column_info <- merge(column_info, file_database$folders)
  
  base_dir <- kwb.utils::getAttribute(file_database$folders, "base_dir")

  file_metadata <- file.path(base_dir, "METADATA_columns_tmp.csv")

  kwb.readxl:::write_csv(column_info, file_metadata)

  # TODO: Rename METADATA_columns_tmp.csv to METADATA_columns.csv, let the user
  # modify the file and read back into column_info
  #
  #column_info <- kwb.readxl:::read_column_info(safePath(base_dir, "METADATA_columns.csv"))
  column_info <- kwb.readxl:::read_csv(
    kwb.utils::safePath(base_dir, "METADATA_columns.csv")
  )
  
  # Use column info to convert the text tables into data frames
  all_data <- kwb.readxl:::text_matrices_to_data_frames(all_tables, column_info)
  
  lapply(all_data, function(all_tables) lapply(all_tables, utils::head))
    
  file_database$files$file_id
  
  # Get the path to a log file  
  logfile_summary <- tempfile("table_summary_", fileext = ".txt")
  logfile_headers <- tempfile("table_headers_", fileext = ".txt")

  # Write a summary of the read structure to the log file
  utils::capture.output(file = logfile_summary, {
    for (tables in all_tables) {
      kwb.readxl:::print_table_summary(tables)
    }
  })

  #kwb.utils::hsOpenWindowsExplorer(dirname(logfile_summary))
  
  # Let's have a look at the tables in one Excel file only
  tables <- all_tables[[1L]]

  # Get a description of the sheets in that file
  kwb.readxl:::get_sheet_info(tables)
  
  # Get a description of tables in that file
  kwb.readxl:::get_table_info(tables)

  # Get the name of the file that was read
  kwb.utils::getAttribute(tables, "file")

  # The tables are named by sheet number and table number within the sheet
  # The numbers are hexadecimal, i.e a = 10, f = 15, 10 = 16, ff = 255,
  names(tables)
  # "table_01_01" "table_02_01"

  # Try to guess the header rows for each table...
  n_headers <- sapply(names(tables), function(table_id) {
    kwb.readxl:::guess_number_of_headers_from_text_matrix(
      tables[[table_id]],
      table_id = table_id
    )
  })

  n_max = 10L
  
  print_guess <- function(x) {
    kwb.readxl:::print_logical_matrix(x = guess_header_matrix(x, n_max))
  }   

  print_guess(x = tables$table_01_01)
  print_guess(x = tables$table_02_01)

  print_header_guesses(tables, n_max, file = logfile_headers)

  lapply(all_tables[[2L]], guess_header_matrix, 4L)

  utils::head(x <- tables$table_02_01)

  is_empty <- (is.na(x) | x == "")

  kwb.readxl:::print_logical_matrix(utils::head(is_empty))
  kwb.readxl:::print_logical_matrix(utils::head(is_empty), invert = TRUE)
}

# Text Matrices to data frames -------------------------------------------------
if (FALSE)
{
  # Convert text matrices of known format
  tables <- kwb.readxl:::get_text_tables_from_xlsx(file = files[1L])

  selected <- grepl("^table_02_", names(tables))

  tables_year_well <- lapply(tables[selected], text_matrix_to_numeric_matrix)

  data_frames_year_well <- lapply(tables_year_well, as.data.frame)

  str(data_frames_year_well$table_02_01)
  str(data_frames_year_well$table_02_02)
}

# print_header_guesses ---------------------------------------------------------
print_header_guesses <- function(
  text_matrices, n_max = 5L, file = NULL, dbg = TRUE
)
{
  if (! is.null(file)) {
    
    kwb.readxl:::debug_formatted(dbg, "Writing output to '%s'... ", file)

    utils::capture.output(file = file, print_header_guesses(text_matrices, n_max))

    kwb.readxl:::debug_ok(dbg)
    
  } else {

    # matrix_name <- "table_03_01"

    for (matrix_name in names(text_matrices)) {
      
      header <- guess_header_matrix(x = text_matrices[[matrix_name]], n_max)

      kwb.readxl:::debug_formatted(dbg, "\n%s:\n", matrix_name)

      kwb.readxl:::print_logical_matrix(header)
    }
  }
}

# guess_header_matrix ----------------------------------------------------------
guess_header_matrix <- function(x, n_max = 10L) 
{
  stopifnot(is.character(x))

  kwb.utils::stopIfNotMatrix(x)

  x_head <- as.data.frame(utils::head(x, n_max))

  do.call(cbind, lapply(x_head, function(column_values) {
    sapply(seq_along(column_values), function(i) {
      as.integer(!(column_values[i] %in% column_values[-(1:i)]))
    })
  }))
}

# text_matrix_to_numeric_matrix ------------------------------------------------
text_matrix_to_numeric_matrix <- function(x) {
  print(x)

  matrix(
    as.numeric(x[-1, -1]),
    nrow = nrow(x) - 1,
    dimnames = list(x[-1, 1], x[1, -1])
  )
}
