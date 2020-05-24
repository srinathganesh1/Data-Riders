# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "your_directory_path/Data-Riders"
setwd(project_dir)

# Get the Raw Data from the file
read_file <- function () {
  data <- read.csv("data/cereals_data.csv")
  return (data)
}

# Handle missing values
handle_missing_values <- function (input) {
  output <- input
  # any handling of missing values
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
get_data <- function () {
  raw <- read_file()
  data <- data_processing(raw)
}

data <- get_data()
View(data)