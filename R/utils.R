# cat_green_bold_0 -------------------------------------------------------------
cat_green_bold_0 <- function(...)
{
  cat(crayon::green(crayon::bold(paste0(...))))
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

# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(fmt, ...)
{
  stop(sprintf(fmt, ...), call. = FALSE)
}
