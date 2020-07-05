# Load Liner Regression Project
project_dir <- "/home/admin-12/Documents/IMARTICUS/Team/Data-Riders/LinearRegression"
setwd(project_dir)
source("src/main.R")

# Start with Decision Tree Project
setwd("/home/admin-12/Documents/IMARTICUS/Team/Data-Riders/DecisionTree")

library(tree)

model <- tree(Sale_Price ~ ., data=training_data)
summary(model)

plot(model)
text(model, pretty = 0)

validate_model <- function (model) {
  actual <- training_data$Sale_Price
  actual <- exp(actual)

  predicted <- predict(model, training_data)
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
}
validate_model(model)