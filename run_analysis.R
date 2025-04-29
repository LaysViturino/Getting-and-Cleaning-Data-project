# I created a general function that can be used to any data.table and will 
# always merge two datasets (here named "train" and "test")
# and will give the mean and sd of the columns (measurement columns) according to the row_labels.


# train_data: data.table to be used as training_data. Must have same column names as test_data
# test_data: data.table to be used as test_data. Must have same column names as training_data
# id_cols: columns to group by (e.g, "subject")
# variable_col: column for variables
# measure_cols: columns to analyze (NULL = all numeric). 
# activity_labels: new names for the variables. Optional
# measure_labels: new names for the measurement columns. Optional.





library(data.table)
library(dplyr)

run_analysis <- function(
    train_data, 
    test_data, 
    id_col,        
    variable_col, 
    measure_cols = NULL,      
    activity_labels = NULL,   
    measure_labels = NULL      
) {
  
  # 1. Merge datasets
  merged_data <- rbindlist(list(train_data, test_data), fill = TRUE)
  
  # 2. Validate required columns
  if (!id_col %in% names(merged_data)) stop(id_col, " column not found")
  if (!variable_col %in% names(merged_data)) stop(variable_col, " column not found")
  
  # 3. Auto-detect measure columns if not specified
  if (is.null(measure_cols)) {
    measure_cols <- setdiff(
      names(merged_data)[sapply(merged_data, is.numeric)],
      c(id_col, variable_col)
    )
    if (length(measure_cols) == 0) stop("No numeric measurement columns found")
    message("Auto-detected measure columns: ", paste(measure_cols, collapse = ", "))
  }
  
  # 4. Apply activity labels if provided
  if (!is.null(activity_labels)) {
    unique_acts <- unique(merged_data[[variable_col]])
    if (length(activity_labels) == length(unique_acts)) {
      merged_data[, (variable_col) := factor(get(variable_col), labels = activity_labels)]
    } else {
      warning("activity_labels length (", length(activity_labels), 
              ") ≠ unique ", variable_col, " values (", 
              length(unique_acts), ")")
    }
  }
  
  # 5. Calculate statistics (mean + SD)
  mean_data <- merged_data[, lapply(.SD, mean), 
                           by = c(id_col, variable_col),
                           .SDcols = measure_cols]
  
  sd_data <- merged_data[, lapply(.SD, sd), 
                         by = c(id_col, variable_col),
                         .SDcols = measure_cols]
  
  # 6. Merge results with proper suffixes
  result <- merge(mean_data, sd_data, 
                  by = c(id_col, variable_col),
                  suffixes = c("_mean", "_sd"))
  
  # 7. Apply measurement column renaming if provided
  if (!is.null(measure_labels)) {
    if (length(measure_labels) == length(measure_cols)) {
      # Create mapping for all measurement columns (both mean and SD)
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
  
  # 8. Order results by ID and condition
  setorderv(result, cols = c(id_col, variable_col))
  
  return(result)
}



################### Example of dataset #######################

#### Read dataset. 
# If you are planning to specify the columns to run the analysis, 
# I recommend making sure they are already named according to the parameter "measure_cols" to avoid errors. 
# It's also possible to run the analysis without specifying the measurement columns. 
# In that case, the code will include all numeric columns except id_col and variable_col
# below is an example of how to run the function while determining the measurement columns. 

# 1. Load and rename measurement data 
x_train <- fread('UCI_HAR_Dataset/train/X_train.txt')  
x_test <- fread('UCI_HAR_Dataset/test/X_test.txt')

# Get variable names
activity <- fread(file.path("UCI_HAR_Dataset/activity_labels.txt"), col.names = c("classLabels", "activityName"))
activityLabels <- activity$activityName #Extract names

# Get measurement names
features <- fread("UCI HAR Dataset/features.txt", col.names = c("index", "featureNames"))
feature_vector <- gsub('[()]', '', features$featureNames)

# Rename measurement columns
setnames(x_train, names(x_train), feature_vector)
setnames(x_test, names(x_test), feature_vector)

# 2. Combine with subject/activity data
subject_train <- fread('UCI_HAR_Dataset/train/subject_train.txt', col.names = "subject")
y_train <- fread('UCI_HAR_Dataset/train/y_train.txt', col.names = "activity")
train_data <- cbind(subject_train, y_train, x_train)

subject_test <- fread('UCI_HAR_Dataset/test/subject_test.txt', col.names = "subject") 
y_test <- fread('UCI_HAR_Dataset/test/y_test.txt', col.names = "activity")
test_data <- cbind(subject_test, y_test, x_test)

# 3. Define labels
measure_cols <- feature_vector[grep("mean|std", feature_vector)]
NewNames <- c("Walk", "Walk_up", "Walk_down", "Sit", "Stand", "Lay")

# 4. Run analysis
result <- run_analysis(train_data = train_data, test_data = test_data,
  id_col = "subject", variable_col = "activity", activity_labels = NewNames,
  measure_cols = measure_cols)
