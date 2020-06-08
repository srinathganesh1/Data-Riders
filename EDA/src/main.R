# Each team member would need to set their directory path to variable "project_dir"
# we will make sure everything else is relative to "project_dir"
project_dir <- "/home/admin-12/Documents/IMARTICUS/Data-Riders/EDA"
setwd(project_dir)

# Setup Packages
load_packages <- function () {
  # Imports
  packages <- c("VIM", "dplyr", "plotly", "psych", "corrplot", "cluster", "factoextra", "VIM", "tm", "RColorBrewer", "wordcloud2")
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
  output <- kNN(output)
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
  output <- within(output, {
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
data

# ---------------------------------
# Evaluate all 16 variables one by one through concepts of sample statistics and build your
#understanding and observe interesting findings
# 1. Describe
describe(data %>% select_if(is.numeric))

# 2. Density Chart for Calorie
skewness <- round(skew(data$calories), 2)
plot(density(data$calories), ylab = "Probabilty Density", xlab = "Calories", main=paste("Density Chart for Calorie   |   Skewness: ", skewness))

# 3. Top 10 Cereals (by Rating)
d <- data %>% arrange(rating) %>% head(10) %>% select(name, rating)
ggplot(data=d, aes(x=name, y=rating)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + labs(y="Ratings", x="Products")

# 4. Calories and Cups
plot(data$calories, data$cups, pch = 19, xlab = "Cups", ylab = "Calories", main = "Calories and Cups")
abline(lm(cups ~ calories, data=data), col="red")

# 5. Correlation View of Variables
data_grp_mfr <- data %>%
  group_by(mfr_names_factor)
cor_result <- cor((data_grp_mfr %>% select_if(is.numeric))[-1])
corrplot(cor_result)
heatmap(cor_result)

# 6. Distribution of Cups Per Serving
data %>% ggplot(aes(x = cups, fill = mfr_names)) +
        geom_histogram() + scale_fill_brewer(palette = "Spectral") +
        scale_x_continuous(name = "# of Cups", expand = c(0,0)) +
        labs(fill = "Manufacturer", title = "Distribution of cups per Serving",
             subtitle = "different cups for servings", y="# of Products") +
        theme_classic() + scale_fill_brewer(palette="Dark2")

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
get_file_content <- function () {
  txt <- readLines("../../data/health.txt")
  txt <- paste(txt, collapse = " ")
  txt <- tolower(txt)
  txt <- gsub("[^a-zA-Z]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  txt <- removeWords(txt, stopwords())
  words <- strsplit(txt, " ")
  word_freq <- table(words)
  return (word_freq)
}
word_freq <- get_file_content()
wordcloud2(word_freq, size=10)

# 13. Cluster
cluster_data <- data %>% select(calories, rating, protein, fat)
km.res <- kmeans(cluster_data, 5, nstart = 25)
fviz_cluster(km.res, data=cluster_data, palette = "jco", ggtheme = theme_minimal())

# 14. Good and Bad parameters for rating
cor_result_rating <- cor_result["rating",]
cor_result_rating_names <- names(cor_result_rating)
names(cor_result_rating) <- NULL
cor_result_df <- data.frame(cor_result_rating_names, cor_result_rating) %>% arrange(cor_result_rating)
names(cor_result_df) <- c("Parameters", "Corellation")
ggplot(cor_result_df, aes(x=Parameters, y=Corellation)) + geom_bar(stat = "identity")

cor_result["rating",]

# 15. prediction with liner model...
model <- lm(rating~sugars+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+shelf+weight+cups, data=data)
changed_data <- data
changed_data[changed_data$name=="100%_Natural_Bran",]$carbo
changed_data[changed_data$name=="100%_Natural_Bran",]$carbo <- 15
data.frame(data, predicted_rating=predict(model, changed_data))[changed_data$name=="100%_Natural_Bran",]
