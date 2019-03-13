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

# print_logical_matrix ---------------------------------------------------------
print_logical_matrix <- function(
  x, invert = FALSE, do_print = TRUE, chars = c("x", " ")
)
{
  stopifnot(is.matrix(x))
  
  if (! is.logical(x)) {
    x <- as.logical(x)
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

# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(fmt, ...)
{
  stop(sprintf(fmt, ...), call. = FALSE)
}
