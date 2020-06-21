# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("psych", "dummies", "dplyr", "car", "ggplot2")
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

  # TODO review
  # - Lot_Extent: Linear feet of street connected to property
  output$Lot_Extent <- handlle_col_na(output$Lot_Extent, 0)

  # TODO think
  # - Garage_Finish_Year # TODO rename variable
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
  data <- dummy.data.frame(data, sep = "#")
  data <- optimise_categorical_variables(data)
  return (data)
}

# Optimize Categorical Variables
optimise_categorical_variables <- function (data) {
  return (data)
}

# Do the processing for continous data
process_continous_data <- function (data) {
  # Normalise the data
  #data <- as.data.frame(scale(data))
  data$Lot_Size <- log(data$Lot_Size)
  data$Sale_Price <- log(data$Sale_Price)

  # Consolidate Variables
  #data$HArea <- data$First_Floor_Area + data$Second_Floor_Area
  #data <- select(data, -c(First_Floor_Area, Second_Floor_Area))
  #data$BsmtFinSF <- data$BsmtFinSF1 + data$BsmtFinSF2
  #data <- select(data, -c(BsmtFinSF1, BsmtFinSF2))

  return (data)
}

# Clearn the data to make it good for processing
# eg. delete unwanted rows, change data types etc
data_processing <- function (input) {
  pre_processed_data <- input
  pre_processed_data <- handle_missing_values(pre_processed_data)

  # Continous Data
  continous_data <- pre_processed_data %>% select_if(not_factor)
  continous_data <- process_continous_data(continous_data)

  # Categorical Data
  categorical_data <- pre_processed_data %>% select_if(is.factor)
  categorical_data <- process_categorical_data(categorical_data)

  result <- NA
  result$raw <- pre_processed_data
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
do_liner_model <- function (dependent_var_name, data, vif_threashold, max_iterations, pvalue_threashold) {
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
    good_predictors <- coeff_mode[, 1][coeff_mode[, 5] < pvalue_threashold]
    good_predictors <- good_predictors[good_predictors != "(Intercept)"]

    # Find Coefficents with NA
    #na_coeff <- names(coef(model))[is.na(coef(model))]
    #print("---------------")
    #print(good_predictors %in% na_coeff)

    # Skip High VIF for next Ieration
    if (i > 1) {
      model_vif <- vif(model)
      low_vif_vars <- names(model_vif)[model_vif < vif_threashold]
      good_predictors <- good_predictors[good_predictors %in% low_vif_vars]
    }

    # For use in next iteration
    independent_vars <- good_predictors
  }

  return (model)
}

training_super_data <- get_data("data/Property_Price_Train.csv")
training_data <- training_super_data$data

options(scipen=999)  # skip e values # https://stackoverflow.com/a/25947542/1897935

#f <- 100000
#plot(training_super_data$raw$Sale_Price, exp(predict(model, training_data)), xlim = c(1, 9*f), ylim = c(1, 9*f))
#abline(v=2*f, h=2*f, col="blue")
#abline(v=4*f, h=4*f, col="brown")
#abline(v=6*f, h=6*f, col="green")
#abline(v=8*f, h=8*f, col="red")

#saveRDS(model, file="a.rda")
#model <- readRDS("a.rda")

#plot(training_data$Sale_Price, type="l", col = "red")
#par(new=TRUE)
#plot(predict(model, training_data), type="l", col = "blue")

#library(plotly)
#d <- data.frame(x=seq_along(training_data$Sale_Price), trace1=training_data$Sale_Price, trace2=predict(model, training_data))
#fig <- plot_ly(head(d, 25), x = ~x)
#fig <- fig %>% add_trace(y = ~trace1, name = 'Real', mode = 'lines', fill = 'tozeroy', fillcolor="rgba(255,0,0,0.5)")
#fig <- fig %>% add_trace(y = ~trace2, name = 'Predicted', mode = 'lines', fill = 'tozeroy', fillcolor="rgba(0,0,255,0.5)")
#fig

# ----------------------------------
#t <- FALSE
#if (t) {
#  test_data <- get_data("data/Property_Price_Test.csv")$data
#  missing_col_in_test_data <- names(training_data)[!names(training_data) %in% names(test_data)]
#  missing_col_in_test_data <- missing_col_in_test_data[missing_col_in_test_data != "Sale_Price"]
#  # set missing col (dummy var) to test data
#  for (col in missing_col_in_test_data) {
#    test_data[col] <- 0
#  }
#  predict(model, training_data)
#}
# ----------------------------------

# area is negative
# - Enclosed_Lobby_Area
# - Open_Lobby_Area
# - W_Deck_Area
# - Garage_Area
#plot(final_linear_model)

#plot(model)
#sqrt(mean(residuals(model)^2))

#if (FALSE) {
#  m1 <- "Zoning_ClassCommer+Zoning_ClassFVR+Zoning_ClassRHD+Zoning_ClassRLD+Lot_ConfigurationCulDSac+Condition2PosN+House_TypeTwnhs+Overall_Material1+Overall_Material2+Overall_Material3+Overall_Material4+Overall_Material5+Overall_Material6+Overall_Material7+House_Condition3+House_Condition4+House_Condition5+House_Condition6+Roof_QualityCT+BsmtFinType1GLQ+Heating_TypeGrav+Heating_QualityEx+Air_ConditioningN+Kitchen_QualityEx+Functional_RateMajD2+Functional_RateSD+Garage2Types+Lot_Size+Remodel_Year+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area"
#  m1_v <- as.vector(strsplit(m1, "\\+"))
#  m1_v
#
#  m2 <- "Neighborhood.StoneBr+Condition2.PosN+Roof_Quality.CT+Kitchen_Quality.Ex+Lot_Size+Overall_Material+House_Condition+Construction_Year+Brick_Veneer_Area+BsmtUnfSF+Total_Basement_Area+Grade_Living_Area+Garage_Size"
#  m2_v <- as.vector(strsplit(m2, "\\+"))
#  m2_v
#}

#a <- readRDS("one.rds")
#b <- readRDS("two.rds")
#c <- readRDS("three.rds")
#
#setdiff(a, b)
#setdiff(b, a)
#
#setdiff(b, c)
#setdiff(c, b)
#
#setdiff(c, a)
#setdiff(a, c)
#
#setdiff(setdiff(a, b), setdiff(b, c))