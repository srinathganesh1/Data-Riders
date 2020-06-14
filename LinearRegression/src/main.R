# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders/LinearRegression"
setwd(project_dir)

# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("dplyr", "psych")
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

# Clearn the data to make it good for processing
# eg. delete unwanted rows, change data types etc
data_processing <- function (input) {
  output <- input
  output <- handle_missing_values(output)

  # some data processing steps that would be required
  return (output)
}

# Get the Data Frame for the data
get_data <- function (file) {
  raw <- read_file(file)
  data <- data_processing(raw)
  return (data)
}

data <- get_data("data/Property_Price_Train.csv")
summary(data)