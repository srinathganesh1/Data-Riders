# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders/LinearRegression"
setwd(project_dir)

# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("psych", "dummies", "dplyr", "car")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

# Get the Raw Data from the file
read_file <- function (file) {
  data <- read.csv(file)
  return (data)
}

# Replace NA values in factor column
handle_factor_col_na <- function (factor_column, default_value) {
  if (!default_value %in% levels(factor_column)) {
    # Add new value to factor levels, if not already present
    factor_column <- factor(factor_column, levels = c(levels(factor_column), default_value))
  }

  # Set default value
  factor_column <- handlle_col_na(factor_column, default_value)
  return (factor_column)
}

# Replace NA values in a column
handlle_col_na <- function (column, default_value) {
  column[is.na(column)] <- default_value
  return (column)
}

# Handle missing values
handle_missing_values <- function (input) {
  output <- input

  # Garage (Garage location) - NA means No Garage
  output$Garage <- handle_factor_col_na(output$Garage, "NoGarage")
  output$Garage_Quality <- handle_factor_col_na(output$Garage_Quality, "NoGarage")

  # Lane_Type: Type of alley access to property - NA means No alley access
  output$Lane_Type <- handle_factor_col_na(output$Lane_Type, "NoAlleyAccess")

  # Brick_Veneer_Type: Masonry veneer type - NA means None
  output$Brick_Veneer_Type <- handle_factor_col_na(output$Brick_Veneer_Type, "None")
  output$Brick_Veneer_Area <- handlle_col_na(output$Brick_Veneer_Area, 0)

  # Fence_Quality: quality of fence - NA means No Fence
  output$Fence_Quality <- handle_factor_col_na(output$Fence_Quality, "NoFence")

  # Miscellaneous_Feature: Miscellaneous feature not covered in other categories - NA means None
  output$Miscellaneous_Feature <- handle_factor_col_na(output$Miscellaneous_Feature, "None")

  # Garage_Condition: Garage condition - NA means No Garage
  output$Garage_Condition <- handle_factor_col_na(output$Garage_Condition, "NoGarage")

  # Pool_Quality Pool quality - NA means No Pool
  output$Pool_Quality <- handle_factor_col_na(output$Pool_Quality, "NoPool")

  # BsmtFinType1: Rating of basement finished area - NA mean No Basement
  output$BsmtFinType1 <- handle_factor_col_na(output$BsmtFinType1, "NoBasement")

  # BsmtFinType2: Rating of basement finished area (if multiple types) - NA mean No Basement
  output$BsmtFinType2 <- handle_factor_col_na(output$BsmtFinType2, "NoBasement")

  # Exposure_Level: Refers to walkout or garden level walls - NA means No Basement
  output$Exposure_Level <- handle_factor_col_na(output$Exposure_Level, "NoBasement")

  # Basement_Condition: Evaluates the general condition of the basement - NA means No Basement
  output$Basement_Condition <- handle_factor_col_na(output$Basement_Condition, "NoBasement")

  # Basement_Height: Evaluates the height of the basement - NA means No Basement
  output$Basement_Height <- handle_factor_col_na(output$Basement_Height, "NoBasement")

  # Fireplace_Quality: quality of fireplaces - NA means No Fireplace
  output$Fireplace_Quality <- handle_factor_col_na(output$Fireplace_Quality, "NoFirePlace")

  # TODO think
  # - Lot_Extent
  # - Garage_Finish_Year
  # - Garage_Built_Year
  # - Electrical_System

  return (output)
}

# Inverse of is.factor function
not_factor <- function (obj) {
  return (!is.factor(obj))
}

# Do the data processing for categorical data
process_categorical_data <- function (data) {
  return (dummy.data.frame(data, sep = "#"))
}

# Clearn the data to make it good for processing
# eg. delete unwanted rows, change data types etc
data_processing <- function (input) {
  pre_processed_data <- input
  pre_processed_data <- handle_missing_values(pre_processed_data)

  # Continous Data
  continous_data <- pre_processed_data %>% select_if(not_factor)

  # Categorical Data
  categorical_data <- pre_processed_data %>% select_if(is.factor)
  categorical_data <- process_categorical_data(categorical_data)

  result <- NA
  result$raw <- pre_processed_data
  result$categorical <- categorical_data
  result$continous <- continous_data
  result$data <- data.frame(categorical_data, continous_data)

  return (result)
}

# Get the Data Frame for the data
get_data <- function (file) {
  raw <- read_file(file)
  data <- data_processing(raw)
  return (data)
}

# Optimise the linear model
do_liner_model <- function (dependent_var_name, data, vif_threashold, max_iterations) {
  independent_vars <- names(data)[names(data) != dependent_var_name]
  model <- NA

  for (i in 1:max_iterations) {
    # Variables for linear model
    model_formula <- paste0(paste0(dependent_var_name, "~"), paste(independent_vars, collapse = "+"))
    model <- lm(model_formula, data=data)
    summary_model <- summary(model)

    # Fetch Records to Scan P values
    coeff_mode <- data.frame(summary_model$coefficients)
    coeff_mode <- cbind(rownames(coeff_mode), coeff_mode)

    # Good Predictors based on P Value
    good_predictors <- coeff_mode[, 1][coeff_mode[, 5] < 0.05]
    good_predictors <- good_predictors[good_predictors != "(Intercept)"]

    # Next Filter: Based on vif threashold
    # TODO

    # For use in next iteration
    independent_vars <- good_predictors
  }

  return (model)
}

super_data <- get_data("data/Property_Price_Train.csv")
data <- super_data$data

options(scipen=999)  # skip e values # https://stackoverflow.com/a/25947542/1897935

final_linear_model <- do_liner_model("Sale_Price", data, 7, 4)
summary(final_linear_model)
#sqrt(mean(residuals(final_linear_model)^2))
