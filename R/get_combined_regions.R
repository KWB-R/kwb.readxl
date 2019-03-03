# get_combined_regions ---------------------------------------------------------

#' Get Regions of Combined Cells
#' 
#' Get regions of combined cells, i.e. cells that are not empty and that
#' are neighbours to each other
#' 
#' @param content matrix of character
#' 
get_combined_regions <- function(content)
{
  stopifnot(is.matrix(content), is.character(content))
  
  # Logical matrix indicating whether a cell is not empty
  is_set <- kwb.utils::defaultIfNA(content, "") != ""
  
  # Initialise a matrix of IDs to be given to the cells
  cell_ids <- is_set

  # Convert matrix of character to matrix of integer
  mode(cell_ids) <- "integer"
  
  # List of IDs of regions that are connected to each other
  connected <- list()
  
  # Current maximum ID
  max_id <- 0

  # Blob colouring algorithm: Give same ID to cells belonging to the same 
  # region, i.e. to the same set of combined cells
  
  # Loop through each row
  for (i in seq_len(nrow(is_set))) {
    
    # Loop through each column
    for (j in seq_len(ncol(is_set))) {
      
      # Nothing to do if cell is empty
      if (! is_set[i, j]) {
        
        next
      }
      
      # Is the left neighbour cell set?      
      left_set <- (j > 1) && is_set[i, j - 1]
      
      # Is the above neighbour cell set?
      above_set <- (i > 1) && is_set[i - 1, j]
      
      # Set the ID
      cell_ids[i, j] <- if (left_set && above_set) {
        
        # Left and above set...
        id_left <- cell_ids[i, j - 1]
        
        id_above <- cell_ids[i - 1, j]
        
        if (id_left == id_above) {
          
          # Left ID is equal to above ID -> get ID from the left
          id_left
          
        } else {
          
          # Left ID is not equal to above ID -> get smallest ID and store the
          # information that left ID and above ID are connected to each other
          ids <- c(id_left, id_above)
          
          min_id <- min(ids)
          
          other_id <- setdiff(ids, min_id)
          
          connected[[min_id]] <- if (length(connected) < min_id) {
            
            other_id
            
          } else {
            
            unique(c(connected[[min_id]], other_id))
          }
          
          min_id
        }
        
      } else if (left_set) {
        
        # Left only set -> get ID from the left        
        cell_ids[i, j - 1]
        
      } else if (above_set) {
        
        # Above only set -> get ID from above
        cell_ids[i - 1, j]
        
      } else {
        
        # Neither neighbour set -> get a new ID
        (max_id <- max_id + 1)
      }
    }
  }
  
  # Unify IDs of connected cells
  for (index in which(! sapply(connected, is.null))) {
    
    cell_ids[cell_ids %in% connected[[index]]] <- index
  }
  
  cell_ids
}

# extract_cell_regions ---------------------------------------------------------
extract_cell_regions <- function(content, bounding_boxes)
{
  apply(bounding_boxes, 1, function(box) {
  
    region <- content[box["top"]:box["bottom"], box["left"]:box["right"]]
    
    kwb.utils::asNoFactorDataFrame(region)
  })
}

# get_bounding_boxes -----------------------------------------------------------
get_bounding_boxes <- function(cell_ids)
{
  used_ids <- setdiff(sort(unique(c(cell_ids))), 0)
  
  t(sapply(used_ids, function(id) {
    
    indices <- which(cell_ids == id, arr.ind = TRUE)
    
    bounding_box <- c(range(indices[, 1]), range(indices[, 2]))
    
    stats::setNames(bounding_box, c("top", "bottom", "left", "right"))
  }))
}

# bounding_boxes_to_table_info -------------------------------------------------
bounding_boxes_to_table_info <- function(bounding_boxes)
{
  colnames(bounding_boxes) <- c("first_row", "last_row", "first_col", "last_col")
  
  table_names <- sprintf("table_%02d", seq_len(nrow(bounding_boxes)))
  
  table_info <- kwb.utils::noFactorDataFrame(table_name = table_names)
  
  cbind(table_info, bounding_boxes)
}

# get_named_regions_by_pattern -------------------------------------------------
get_named_regions_by_pattern <- function(regions, patterns)
{
  region_names <- character(length(regions))
  
  for (key in names(patterns)) {
    
    pattern <- patterns[[key]]
    
    is_matching <- sapply(regions, function(x) {
      
      any(grepl(pattern, as.matrix(x)))
    })
    
    if (any(is_matching)) {
      
      matching_names <- region_names[is_matching] 
      
      matching_names <- sapply(matching_names, set_or_append, key)
      
      region_names[is_matching] <- matching_names
    }
  }
  
  # Return only labelled tables
  
  labelled <- region_names != ""
  
  region_names <- kwb.utils::makeUnique(region_names[labelled], warn = FALSE)
  
  stats::setNames(regions[labelled], region_names)
}

# set_or_append ----------------------------------------------------------------
set_or_append <- function(x, value, sep = "_")
{
  if (x == "") value else paste(x, value, sep = sep)
}
