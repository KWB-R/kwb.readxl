# cat_green_bold_0 -------------------------------------------------------------
cat_green_bold_0 <- function(...)
{
  cat(crayon::green(crayon::bold(paste0(...))))
}

# debug_file -------------------------------------------------------------------
debug_file <- function(dbg, file)
{
  if (dbg) {
    
    cat_green_bold_0(sprintf("\n  File: '%s'\n", basename(file)))
    
    cat(sprintf("Folder: '%s'\n", dirname(file)))
  }
}

# debug_formatted --------------------------------------------------------------
debug_formatted <- function(dbg, fmt, ...)
{
  kwb.utils::catIf(dbg, sprintf(fmt, ...))
}

# debug_ok ---------------------------------------------------------------------
debug_ok <- function(dbg)
{
  kwb.utils::catIf(dbg, "ok.\n")
}

# delete_empty_columns_right ---------------------------------------------------
delete_empty_columns_right <- function(x)
{
  stopifnot(length(dim(x)) == 2)
  
  last_column_index <- max(which(! kwb.utils::isNaInAllRows(x)))
  
  x[, seq_len(last_column_index), drop = FALSE]
}

# get_col ----------------------------------------------------------------------
get_col <- kwb.utils::selectColumns

# get_ele ----------------------------------------------------------------------
get_ele <- kwb.utils::selectElements

# print_logical_matrix ---------------------------------------------------------
print_logical_matrix <- function(
  x, invert = FALSE, do_print = TRUE, chars = c("x", " ")
)
{
  stopifnot(is.matrix(x))
  
  if (! is.logical(x)) {
    mode(x) <- "logical"
  }

  if (invert) {
    x <- ! x
  }
  
  y <- matrix(chars[2], nrow = nrow(x), ncol = ncol(x))
  
  y[x] <- chars[1]
  
  result <- kwb.utils::pasteColumns(as.data.frame(y), sep = "|")
  
  if (do_print) {
    
    writeLines(result)
    invisible(result)
    
  } else {
    
    result
  }
}

# read_csv ---------------------------------------------------------------------
read_csv <- function(file, lng = "de")
{
  kwb.utils::callWith(
    utils::read.table, 
    file = file, 
    kwb.utils::argsCsv(lng), 
    header = TRUE, 
    stringsAsFactors = FALSE
  )
}

# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(fmt, ...)
{
  stop(sprintf(fmt, ...), call. = FALSE)
}

# write_csv --------------------------------------------------------------------
write_csv <- function(x, file, lng = "de")
{
  kwb.utils::callWith(
    FUN = utils::write.table, 
    kwb.utils::argsCsv(lng),
    x = x, 
    file = file, 
    row.names = FALSE, 
    na = ""
  )
}
