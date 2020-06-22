data <- read.csv("/home/admin-12/Documents/IMARTICUS/Data-Riders/LinearRegression/data/Property_Price_Train.csv")
library(car)
library(dplyr)
library(ggplot2)

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

data <- handle_missing_values(data)

# one
summary(lm(Sale_Price ~ ., data=data))

# two
model <- lm(Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + Lot_Configuration + Property_Slope + Neighborhood + Condition1 + Condition2 + House_Type + Overall_Material + House_Condition + Construction_Year + Roof_Quality + Brick_Veneer_Area + Exterior_Material + Basement_Height + Exposure_Level + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating_Quality + First_Floor_Area + Second_Floor_Area + Bedroom_Above_Grade + Kitchen_Above_Grade + Kitchen_Quality + Rooms_Above_Grade + Functional_Rate + Fireplaces + Garage_Size + Garage_Quality + Screen_Lobby_Area + Pool_Area + Pool_Quality + Fence_Quality + Sale_Type + Sale_Condition, data = data)
summary(model)

# three
model <- lm(Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + Lot_Configuration + Property_Slope + Neighborhood + Condition1 + Condition2 + House_Type + Overall_Material + House_Condition + Construction_Year + Roof_Quality + Brick_Veneer_Area + Exterior_Material + Basement_Height + Exposure_Level + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + First_Floor_Area + Second_Floor_Area + Bedroom_Above_Grade + Kitchen_Above_Grade + Kitchen_Quality + Rooms_Above_Grade + Fireplaces + Garage_Size + Garage_Quality + Screen_Lobby_Area + Pool_Quality + Sale_Condition, data = data)
summary(model)

# four
model <- lm(Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + Lot_Configuration + Property_Slope + Condition1 + Condition2 + House_Type + Overall_Material + House_Condition + Construction_Year + Roof_Quality + Brick_Veneer_Area + Exterior_Material + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + First_Floor_Area + Second_Floor_Area + Bedroom_Above_Grade + Kitchen_Above_Grade + Kitchen_Quality + Rooms_Above_Grade + Fireplaces + Garage_Size + Garage_Quality + Screen_Lobby_Area + Pool_Quality + Sale_Condition, data = data)
summary(model)
