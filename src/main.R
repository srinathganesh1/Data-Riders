# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders"
setwd(project_dir)

# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("VIM", "dplyr", "plotly", "psych", "corrplot", "cluster", "factoextra")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

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
  # TODO KNN
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

  # Calorie Categories
  data <- within(data, {
    calories_category <- NA
    calories_category[calories>=50&calories<80] <- "L"
    calories_category[calories>=80&calories<110] <- "M"
    calories_category[calories>=110] <- "H"
  })

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
#View(data)

# ---------------------------------
# Evaluate all 16 variables one by one through concepts of sample statistics and build your
#understanding and observe interesting findings
# 1. Describe
describe(data)

# 2. Density Chart for Calorie
plot(density(data$calories))

# 3. Top 10 Cerials that are rating high
d <- data %>% arrange(rating) %>% head(10) %>% select(name, rating)
ggplot(data=d, aes(x=name, y=rating)) + geom_bar(stat = "identity")

# 4. Calories and Cups
cor.test(data$calories, data$weight)
plot(data$calories, data$weight)
# abline(lm(calories~weight,data=data), col='blue')
# TODO add liner model line

# 5. Correlation View of Variables
data_grp_mfr <- data %>%
  group_by(mfr_names_factor)
corrplot(cor((data_grp_mfr %>% select_if(is.numeric))[-1]))

# 6. Distribution of Cups Per Serving
data %>% ggplot(aes(x = weight, fill = mfr_names)) + geom_histogram() + scale_fill_brewer(palette = "Set5") +
    scale_x_continuous(name = "Weight (in ounces)", expand = c(0,0)) +
    labs(fill = "Manufacturer", title = "Distribution of Weight per Serving", subtitle = "different weights for servings") +
    theme_classic()

# 7. Distribution of cups per Serving
data %>% ggplot(aes(x = cups, fill = mfr)) + geom_histogram() + scale_fill_brewer(palette = "Spectral") + scale_x_continuous(name = "no of cups", expand = c(0,0)) + labs(fill = "Manufacturer", title = "Distribution of cups per Serving", subtitle = "different cups for servings") + theme_classic()

# 8. Dist of rating vs cal
data %>% ggplot(aes(x = rating, fill = calories_category)) + geom_histogram() + scale_fill_brewer(palette = "Set1") + scale_x_continuous(name = "Ratings", expand = c(0,0)) + labs(fill = " Cal ",y ='count', title = "Distribution of Ratings vs cal", subtitle = "Low rating for Hogh calories") + theme_classic()

# 9. Mfr wise calorie distribution
count_cal <- data %>% group_by(mfr_names) %>% count(calories_category)
ggplot(count_cal,aes(x=reorder(calories_category,n),y=n,fill=mfr_names))+geom_bar(stat='identity')+ labs(x='cal',y='count',title='Mfr wise calorie distribution')

# 10. Company wise Rating Consistency
# IF you see a new product by company X, how good would it be?
data_grp_mfr %>% plot_ly(y= ~rating, x= ~mfr_names_factor, type = "box", color= ~mfr_names_factor) %>%
  layout(title="Product Consistency", xaxis=list(title="Manufactures"), yaxis=list(title="Rating"))

# 11. Company wise Calorie Constency
data_grp_mfr %>% plot_ly(y= ~calories, x= ~mfr_names_factor, type = "box", color= ~mfr_names_factor) %>%
  layout(title="Product Consistency (by Calories)", xaxis=list(title="Manufactures"), yaxis=list(title="Calories"))

# 12. Word Cloud

# 13. Cluster
# TODO cluster layout does not match
cluster_data <- data %>% select(calories, rating, protein, fat)
km.res <- kmeans(cluster_data, 5, nstart = 25)
fviz_cluster(km.res, data=cluster_data, palette = "jco", ggtheme = theme_minimal())

# TODO slicing and dicing
# TODO LOGO

# 14. Good and Bad parameters
# 15. prediction with liner model...

a <- 1
#data[21,]
#kNN(data)[21,]
#dummy <- 1

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
#names(data)
model <- lm(rating~sugars+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+shelf+weight+cups, data=data)

#a <- data[1,]
#a
#b <- a
#b["fat"] <- 2
data
#data.frame(data$sugars, data$rating)
predict(model, data[c(1:3), ], interval='prediction')
changed_data <- data
changed_data[changed_data$name=="100%_Natural_Bran",]
changed_data[changed_data$name=="100%_Natural_Bran",]$carbo <- 10
data.frame(data, predicted_rating=predict(model, changed_data))[c(1:3), ]
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



a <- data_grp_mfr %>% select_if(is.numeric)
a[-c(1)]
(data_grp_mfr %>% select_if(is.numeric))[-1]
library(corrplot)
corrplot(cor((data_grp_mfr %>% select_if(is.numeric))[-1]))

cor((data_grp_mfr %>% select_if(is.numeric))[-1])
