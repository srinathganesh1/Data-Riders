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

  # Set company names
  output$mfr_names <- as.character(output$mfr)
  output$mfr_names[output$mfr_names=="A"] <- "American Home Food Products"
  output$mfr_names[output$mfr_names=="G"] <- "General Mills"
  output$mfr_names[output$mfr_names=="K"] <- "Kelloggs"
  output$mfr_names[output$mfr_names=="N"] <- "Nabisco"
  output$mfr_names[output$mfr_names=="P"] <- "Post"
  output$mfr_names[output$mfr_names=="Q"] <- "Quaker Oats"
  output$mfr_names[output$mfr_names=="R"] <- "Ralston Purina"
  output$mfr_names_factor = as.factor(output$mfr_names)

  # some data processing steps that would be required
  return (output)
}

# Get the Data Frame for the data
get_data <- function () {
  raw <- read_file()
  data <- data_processing(raw)
  return (data)
}

data <- get_data()
View(data)

#data %>% group_by(mfr) %>%
#  mutate(names=paste0(name, collapse=", ")) %>%
#  select(mfr, names) %>%
#  distinct(mfr, names)
#
#data %>% group_by(mfr, type) %>%
#  summarise(
#    count=n(),
#    names=paste0(name, collapse=", ")
#  )


#names(data)
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
#correlation <- cor(data[-c(1, 2, 3)])

#cor.test(data$calories, data$carbo)

#cor.test(data$rating, data$sugars)
#pairs(formula=~rating+sugars, data=data)

#dev.off()

# JUST BETWEEN SUGAR AND RATING!!!!!
# so dont expect it to be correct!
model <- lm(rating~sugars+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+shelf+weight+cups, data=data)
#model

data[data$name=='Corn_Flakes', ]

a <- data[1,]
a
b <- a
b["fat"] <- 2

#data.frame(data$sugars, data$rating)
predict(model, b, interval="prediction")
#data.frame(data$rating, data$sugars, predict(model, data.frame(sugars=data$sugars), interval="prediction"))

#install.packages("plotly")
#install.packages("quantmod")
library(ggplot2)
library(plotly)
library(quantmod)
library(dplyr)


#process_w_rating <- data %>%
#  group_by(mfr_names_factor) %>%
#  summarise(
#    product_count=n(),
#    rating_range_low=range(rating)[1],
#    rating_range_high=range(rating)[2],
#    rating_mean=mean(rating),
#  )

#plot(process_w_rating$mfr_names_factor, process_w_rating$rating_range_low, ylim = c(1, 100), type="l", col="red")
#par(new=TRUE)
#plot(process_w_rating$mfr_names_factor, process_w_rating$rating_range_high, ylim = c(1, 100), type="l", col="blue")
#par(new=TRUE)
#plot(process_w_rating$mfr_names_factor, process_w_rating$rating_mean, ylim = c(1, 100), type="l", col="blue")

data_grp_mfr <- data %>%
  group_by(mfr_names_factor)

#fig <- plot_ly(y=a$rating, type="box")
#fig

#library(ggplot2)
#ggplot(data=data2, aes(x=mfr_names_factor, y=rating)) + geom_boxplot(aes(fill=mfr_names_factor))

# IF you see a new product by company X, how good would it be?
data_grp_mfr %>% plot_ly(y= ~rating, x= ~mfr_names_factor, type = "box", color= ~mfr_names_factor) %>%
  layout(title="Product Consistency", xaxis=list(title="Manufactures"), yaxis=list(title="Rating"))

data_grp_mfr %>% plot_ly(y= ~calories, x= ~mfr_names_factor, type = "box", color= ~mfr_names_factor) %>%
  layout(title="Product Consistency (by Calories)", xaxis=list(title="Manufactures"), yaxis=list(title="Calories"))

a <- data_grp_mfr %>% select_if(is.numeric)
a[-c(1)]
(data_grp_mfr %>% select_if(is.numeric))[-1]
library(corrplot)
corrplot(cor((data_grp_mfr %>% select_if(is.numeric))[-1]))

cor((data_grp_mfr %>% select_if(is.numeric))[-1])
