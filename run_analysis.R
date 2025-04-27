library(data.table)
library(dplyr)


#I created a general function that can be used to any data.table and will always merge two datasets (here named "train" and "test"
#and will give the mean and sd of the columns according to the row_labels.


#train_data: data.table to be used as training_data. Must have same column names as test_data
#test_data: data.table to be used as test_data. Must have same column names as training_data
#group_cols: columns to group by (e.g, "subject")
#measure_cols: columns to analyze (NULL = all numeric). Optional.
#NewNames: vector with the new names for the final dataset. Optional.



run_analysis <- function(
    train_data, 
    test_data, 
    id_col = "subject",        # Column for individuals
    variable_col = "activity", # Column for conditions
    measure_cols = NULL,       # Columns to analyze (NULL = all numeric)
    variable_labels = NULL,    # New names for condition categories
    measure_labels = NULL      # New names for measurement columns
) {
  require(data.table)
  
  # 1. Merge datasets
  merged_data <- rbindlist(list(train_data, test_data), fill = TRUE)
  
  # 2. Validate columns
  if (!id_col %in% names(merged_data)) stop(id_col, " column not found")
  if (!variable_col %in% names(merged_data)) stop(variable_col, " column not found")
  
  # 3. Auto-detect measure columns if not specified
  if (is.null(measure_cols)) {
    measure_cols <- setdiff(
      names(merged_data)[sapply(merged_data, is.numeric)],
      c(id_col, variable_col)
    )
    if (length(measure_cols) == 0) stop("No numeric measurement columns found")
  }
  
  # 4. Apply condition labels if provided
  if (!is.null(variable_labels)) {
    unique_vars <- unique(merged_data[[variable_col]])
    if (length(variable_labels) == length(unique_vars)) {
      merged_data[, (variable_col) := factor(get(variable_col), labels = variable_labels)]
    } else {
      warning("variable_labels length (", length(variable_labels), 
              ") ≠ unique ", variable_col, " values (", 
              length(unique_vars), ")")
    }
  }
  
  # 5. Calculate statistics
  mean_data <- merged_data[, lapply(.SD, mean), 
                           by = c(id_col, variable_col),
                           .SDcols = measure_cols]
  
  sd_data <- merged_data[, lapply(.SD, sd), 
                         by = c(id_col, variable_col),
                         .SDcols = measure_cols]
  
  # 6. Merge results
  result <- merge(mean_data, sd_data, 
                  by = c(id_col, variable_col),
                  suffixes = c("_mean", "_sd"))
  
  # 7. Apply measurement labels if provided
  if (!is.null(measure_labels)) {
    if (length(measure_labels) == length(measure_cols)) {
      # Create mapping for all measurement columns
      old_names <- c(
        paste0(measure_cols, "_mean"),
        paste0(measure_cols, "_sd")
      )
      new_names <- c(
        paste0(measure_labels, "_mean"),
        paste0(measure_labels, "_sd")
      )
      setnames(result, old = old_names, new = new_names)
    } else {
      warning("measure_labels length (", length(measure_labels), 
              ") ≠ measure_cols length (", length(measure_cols), ")")
    }
  }
  
  # 8. Order results
  setorderv(result, cols = c(id_col, variable_col))
  
  return(result)
}