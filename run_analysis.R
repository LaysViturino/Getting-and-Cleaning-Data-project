library(data.table)
library(dplyr)

# Read datasets
subject_train <- fread('UCI_HAR_Dataset/train/subject_train.txt', col.names = "subject")
x_train <- fread('UCI_HAR_Dataset/train/X_train.txt')  # Features
y_train <- fread('UCI_HAR_Dataset/train/y_train.txt', col.names = "activity")  # Labels

subject_test <- fread('UCI_HAR_Dataset/test/subject_test.txt', col.names = "subject")
x_test <- fread('UCI_HAR_Dataset/test/X_test.txt')    # Features
y_test <- fread('UCI_HAR_Dataset/test/y_test.txt', col.names = "activity")    # Labels

# Combine columns
train_data <- cbind(subject_train, y_train, x_train)
test_data <- cbind(subject_test, y_test, x_test)

activityLabels <- fread(file.path("UCI_HAR_Dataset/activity_labels.txt"), col.names = c("classLabels", "activityName"))

NewNames <- c("Walk", "Walk_up", "Walk_down", "Sit", "Stand", "Lay")


#train_data: data.table to be used as training_data. Must have same column names as test_data
#test_data: data.table to be used as test_data. Must have same column names as training_data
#group_cols: columns to group by (e.g, "subject")
#measure_cols: columns to analyze (NULL = all numeric). Optional.
#NewNames: vector with the new names for the final dataset. Optional.

#I created a general function that can be used to any data.table and will always merge two datasets (here named "train" and "test"
#and will give the mean and sd of the columns according to the row_labels.


run_analysis <- function(train_data, test_data, group_cols = NULL, measure_cols = NULL, NewNames = NULL) {
  require(data.table)
  
  # 1. Merge datasets
  merged_data <- rbindlist(list(train_data, test_data), fill = TRUE)
  
  # 2. Auto-detect columns if not specified
  if (is.null(group_cols)) {
    group_cols <- names(merged_data)[!sapply(merged_data, is.numeric)]
    message("Grouping by: ", paste(group_cols, collapse = ", "))
  }
  
  if (is.null(measure_cols)) {
    measure_cols <- setdiff(
      names(merged_data)[sapply(merged_data, is.numeric)],
      group_cols
    )
    message("Analyzing columns: ", paste(measure_cols, collapse = ", "))
  }
  
  # 3. Apply group renaming (to first group column only)
  if (!is.null(NewNames) && length(group_cols) > 0) {
    unique_vals <- unique(merged_data[[group_cols[1]]])
    if (length(NewNames) == length(unique_vals)) {
      merged_data[, (group_cols[1]) := factor(get(group_cols[1]), labels = NewNames)]
    } else {
      warning("NewNames length (", length(NewNames), 
              ") ≠ unique values in '", group_cols[1], "' (", 
              length(unique_vals), ")")
    }
  }
  
  # 4. Calculate stats
  result <- merged_data[,
                        c(lapply(.SD, mean), lapply(.SD, sd)),
                        by = group_cols,
                        .SDcols = measure_cols
  ]
  
  # 5. Clean output names
  stat_names <- c(
    paste0(measure_cols, "_mean"),
    paste0(measure_cols, "_sd")
  )
  setnames(result, 
           old = c(measure_cols, paste0(measure_cols, ".1")), 
           new = stat_names)
  
  return(result[order(get(group_cols[1]))])
}