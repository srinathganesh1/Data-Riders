# Load Liner Regression Project
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders-GITHUB/LinearRegression"
setwd(project_dir)
source("src/main.R")

# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("tree", "class", "FNN", "MASS", "randomForest")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

evalute_prediction <- function (actual, predicted) {
  actual <- exp(actual)
  predicted <- exp(predicted)
  diff <- abs(actual - predicted)

  res <- data.frame(
    actual=actual,
    predicted=predicted,
    diff=diff,
    diff_pc=diff/actual * 100
  )

  plot(predicted, diff, ylab = "Residual", xlab = "Fitted")

  hist(res$diff_pc, main="Residual Relative Percentage (actual-predicted)/actual * 100",
       xlab = "Residual Percentage")

  print(paste("RSME: ", round(sqrt(mean(diff^2)))))
  print(paste("MSE: ", round(mean(diff^2))))
  print(paste("MAE (Mean Absolute Error): ", round(mean(abs(diff)))))
}

validate_tree <- function (model) {
  print(summary(model))

  plot(model)
  text(model, pretty = 0)

  actual <- training_data$Sale_Price
  predicted <- predict(model, training_data)
  evalute_prediction(actual, predicted)
}

# Decision Tree
print("########### Decision Tree ############")
tree1 <- tree(Sale_Price ~ ., data = training_data)
validate_tree(tree1)
# MAPE

# KNN
print("########### KNN ############")
var_filter <- c('Zoning_Class.Commer', 'Condition2.PosN', 'Roof_Quality.CT', 'Heating_Quality.Ex', 'Air_Conditioning.N', 'Kitchen_Quality.Ex', 'Functional_Rate.MajD2', 'Lot_Size', 'Overall_Material', 'House_Condition', 'Construction_Year', 'Remodel_Year', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'First_Floor_Area', 'Second_Floor_Area', 'Garage_Size', 'Screen_Lobby_Area')
knn_dataset <- subset(training_data, select = var_filter)
predicted <- knn.reg(train = knn_dataset, test = knn_dataset, y = training_data["Sale_Price"], k = sqrt(round(length(var_filter))))
evalute_prediction(training_data$Sale_Price, predicted$pred)

#library(class)
#knn(train = knn_dataset[1:1021, ], test = knn_dataset[1022:1459, ], cl=training_data[1:1021, ]["Sale_Price"], k=3)

print("########### Random Forest ############")
#model_formula <- paste0(paste0("Sale_Price", "~"), paste(var_filter, collapse = "+")) # not working
rf_model <- randomForest(Sale_Price~Zoning_Class.Commer+Condition2.PosN+Roof_Quality.CT+Heating_Quality.Ex+Air_Conditioning.N+Kitchen_Quality.Ex+Functional_Rate.MajD2+Lot_Size+Overall_Material+House_Condition+Construction_Year+Remodel_Year+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+First_Floor_Area+Second_Floor_Area+Garage_Size+Screen_Lobby_Area,
                         training_data, mtry = 5, importance = TRUE)
rf_model <- randomForest(Sale_Price~.,
                         training_data, mtry = 5, importance = TRUE)
importance(rf_model)
predicted <- predict(rf_model, newdata = training_data)
evalute_prediction(training_data$Sale_Price, as.numeric(predicted))
