# get_raw_text_from_xlsx -------------------------------------------------------
get_raw_text_from_xlsx <- function(file, sheets = NULL, dbg = TRUE)
{
  stopifnot(is.character(file), length(file) == 1)

  # Get the names of available sheets in the file
  available_sheets <- readxl::excel_sheets(file)
  
  # If no sheet name is given, call this function for all sheets in the file
  sheets <- kwb.utils::defaultIfNULL(sheets, available_sheets)
  
  # All given sheet names must be available
  stopifnot(is.character(sheets), all(sheets %in% available_sheets))
  
  # Print the file name and file path to the console
  debug_file(dbg, file)

  # Read all sheets into a list
  result <- lapply(sheets, read_sheet_as_text, file = file, dbg = dbg)

  # Create sheet metadata
  sheet_table <- kwb.utils::noFactorDataFrame(
    sheet_id = kwb.utils::createIdAlong(sheets),
    sheet_name = sheets
  )
    
  # Name the list entries according to the sheet ids
  names(result) <- kwb.utils::selectColumns(sheet_table, "sheet_id")

  # Set the sheet metadata as an attribute
  structure(result, sheet_info = sheet_table)
}

# read_sheet_as_text -----------------------------------------------------------
read_sheet_as_text <- function(file, sheet, dbg = TRUE)
{
  # Explicitly select all rows starting from the first row. Otherwise empty
  # rows at the beginning are automatically skipped. I want to keep everything
  # so that the original row numbers can be used as a reference
  range <- cellranger::cell_rows(c(1, NA))
  
  debug_formatted(dbg, "Reading sheet '%s' as raw text ... ", sheet)
  
  result <- as.matrix(readxl::read_xlsx(
    file, sheet, range = range, col_names = FALSE, col_types = "text"
  ))
  
  debug_ok(dbg)
  
  mode(result) <- "character"
  
  structure(result, file = file, sheet = sheet)
}
