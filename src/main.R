# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders"
setwd(project_dir)
library(dplyr)

# Get the Raw Data from the file
read_file <- function () {
  data <- read.csv("data/cereals_data.csv")
  return (data)
}

# Handle missing values
handle_missing_values <- function (input) {
  output <- input
  # any handling of missing values
  output <- na.omit(output)
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

summary(data)


names(data)
#boxplot(data$calories)
#boxplot(data$protein)
#boxplot(data$fat)
#boxplot(data$sodium)
#boxplot(data$fiber)
#boxplot(data$carbo)
#boxplot(data$sugars)
#boxplot(data$potass)
#boxplot(data$vitamins)
#boxplot(data$shelf)
#boxplot(data$weight)
#boxplot(data$rating)

#plot(select(data, c("calories", "protein", "fat", "sodium", "rating")))

#cor.test(data$weight, data$rating)
#cor.test(data$calories, data$rating)
#data[-c(1, 2, 3)]
correlation <- cor(data[-c(1, 2, 3)])
correlation
correlation

#cor.test(data$calories, data$carbo)

cor.test(data$rating, data$sugars)
pairs(formula=~rating+sugars, data=data)

#dev.off()

# JUST BETWEEN SUGAR AND RATING!!!!!
# so dont expect it to be correct!
model <- lm(rating~sugars+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+shelf+weight+cups, data=data)
model

data[data$name=='Corn_Flakes', ]

a <- data[1,]
a
b <- a
b["fat"] <- 20

#data.frame(data$sugars, data$rating)
predict(model, b, interval="prediction")
data.frame(data$rating, data$sugars, predict(model, data.frame(sugars=data$sugars), interval="prediction"))