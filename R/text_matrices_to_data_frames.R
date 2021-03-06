# text_matrices_to_data_frames -------------------------------------------------
text_matrices_to_data_frames <- function(all_tables, column_info, dbg = TRUE)
{
  for (file_id in names(all_tables)) {
    
    print(file_id)

    # file_id <- names(all_tables)[1]

    tables <- all_tables[[file_id]]
    
    table_info <- get_table_info(tables)

    this_file_column_info <- column_info[column_info$file_id == file_id, ]

    table_ids <- kwb.utils::toNamedList(names(tables))

    all_tables[[file_id]] <- lapply(table_ids, function(table_id) {
      
      print(table_id)

      # table_id <- names(tables)[1]

      text_matrix <- tables[[table_id]]

      n_headers <- table_info$n_headers[table_info$table_id == table_id]

      text_matrix <- text_matrix[-seq_len(n_headers), , drop = FALSE]

      selected <- this_file_column_info$table_id == table_id

      this_column_info <- this_file_column_info[selected, ]

      this_column_info <- this_column_info[order(this_column_info$column_no), ]

      stopifnot(nrow(this_column_info) == ncol(text_matrix))

      stopifnot(this_column_info$column_no == seq_len(nrow(this_column_info)))

      col_names <- this_column_info$column_name

      colnames(text_matrix) <- col_names

      result <- kwb.utils::asNoFactorDataFrame(text_matrix)

      indices_numeric <- which(this_column_info$column_type == "N")

      result[indices_numeric] <- lapply(indices_numeric, function(i) {
        
        text_to_numeric(txt_values = result[[i]], column = col_names[i], dbg)
      })

      result
    })
  }

  all_tables
}

# text_to_numeric --------------------------------------------------------------
text_to_numeric <- function(txt_values, column = "name?", dbg = TRUE)
{
  num_values <- suppressWarnings(as.numeric(txt_values))

  # The conversion is valid if the text was NA or if the numeric is not NA  
  valid <- is.na(txt_values) | ! is.na(num_values)

  # Return numeric values if there were no errors
  if (all(valid)) {
    return(num_values)
  }    
    
  debug_formatted(
    dbg, paste0(
      "Cannot convert column '%s' to numeric.\n",
      "(First) non-numeric text values: %s\n"
    ),
    column, 
    kwb.utils::stringList(utils::head(txt_values[! valid]))
  )
  
  txt_values
}
