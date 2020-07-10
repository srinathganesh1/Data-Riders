setwd("/home/admin-12/Documents/IMARTICUS/Data-Riders/LogisticRegression")

data <- read.csv("data/bankloan.csv")
data$default <- as.factor(data$default)

load_packages <- function () {
  # Imports
  packages <- c("psych", "dplyr", "car", "pROC", "ROCR", "caret", "tree", "randomForest", "class")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

get_conf_matrix <- function (tree) {
  # Train
  pred <- as.factor(ifelse(predict(tree, newdata = train_data)[, 2] >= 0.1, 1, 0))
  conf_matrix <- confusionMatrix(pred, train_data$default, positive = "1")
  print("--- Train Confusion Matrix ---")
  print(conf_matrix)
  print(conf_matrix$byClass)

  print("**********************************************************")

  # Test
  pred <- as.factor(ifelse(predict(tree, newdata = test_data)[, 2] >= 0.1, 1, 0))
  conf_matrix <- confusionMatrix(pred, test_data$default, positive = "1")
  print("--- Test Confusion Matrix ---")
  print(conf_matrix)
  print(conf_matrix$byClass)
}

## set the seed to make your partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Decision Tree (without Pruned)
variable_names <- c("age", "ed", "employ", "address", "income", "debtinc", "creddebt", "othdebt")
formula <- paste0("default~", paste0(variable_names, collapse = "+"))
tree1 <- tree(default ~ ., data=train_data)
get_conf_matrix(tree1)

# Decision Tree (Pruned)
set.seed(123)
cv_tree <- cv.tree(tree1, FUN = prune.misclass)
plot(cv_tree$size, cv_tree$dev, type="b", lwd=2)
abline(v=6)
plot(cv_tree$k, cv_tree$dev, type="b", lwd=2)
abline(v=3)
tree2 <- prune.misclass(tree1, best = 6)
get_conf_matrix(tree2)

# Bagging
set.seed(1)
bagging <- randomForest(default~age+ed+employ+address+income+debtinc+creddebt+othdebt,
                        train_data, mtry = length(variable_names), importance = TRUE)
varImpPlot(bagging)
bagging
conf_matrix <- confusionMatrix(
  predict(bagging, newdata = train_data),
  train_data$default, positive = "1"
)
print("--- train ---")
conf_matrix
conf_matrix$byClass

print("--- test ---")
conf_matrix <- confusionMatrix(
  predict(bagging, newdata = test_data),
  test_data$default, positive = "1"
)
conf_matrix
conf_matrix$byClass

print("##############################################")

set.seed(1)
random_forest <- randomForest(default~age+ed+employ+address+income+debtinc+creddebt+othdebt,
                        train_data, mtry = 3, importance = TRUE)
varImpPlot(random_forest)
random_forest
print("--- train ---")
conf_matrix <- confusionMatrix(
  predict(random_forest, newdata = train_data),
  train_data$default, positive = "1"
)
conf_matrix
conf_matrix$byClass

print("--- test ---")
conf_matrix <- confusionMatrix(
  predict(random_forest, newdata = test_data),
  test_data$default, positive = "1"
)
conf_matrix
conf_matrix$byClass

print("###################### knn ########################")

# KNN
knn_dataset <- subset(train_data, select = -default)
knn_pred <- knn(knn_dataset, knn_dataset, cl=train_data$default, k=5)

print("--- train ---")
conf_matrix <- confusionMatrix(
  knn_pred,
  train_data$default, positive = "1"
)
conf_matrix
conf_matrix$byClass

print("--- test ---")
knn_test_dataset <- subset(test_data, select = -default)

knn_pred <- knn(knn_dataset, knn_test_dataset, cl=train_data$default, k=5)
conf_matrix <- confusionMatrix(
  knn_pred,
  test_data$default, positive = "1"
)
conf_matrix
conf_matrix$byClass