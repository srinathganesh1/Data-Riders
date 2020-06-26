setwd("/home/admin-12/Documents/IMARTICUS/Data-Riders/LogisticRegression")
data <- read.csv("data/bankloan.csv")
data$default <- as.factor(data$default)

load_packages <- function () {
  # Imports
  packages <- c("psych", "dplyr", "car", "pROC", "ROCR", "caret")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

## set the seed to make your partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build the model
do_model <- function (dataset, variable_names) {
  formula <- paste0("default~", paste0(variable_names, collapse = "+"))
  model <- glm(formula, data=dataset, family=binomial)

  predict_val <- predict(model, type = "response")
  auc_formula_str <- paste0("train_data$", variable_names, collapse = "+")
  auc_formula <- eval(parse(text = auc_formula_str))
  find_auc <- round(roc(train_data$default, auc_formula)$auc, 2)

  # ROC Curve
  prediction_v <- prediction(auc_formula, train_data$default)
  performance_v_roc <- performance(prediction_v, "tpr", "fpr")
  #plot(performance_v_roc, colorize=TRUE)

  # Performance vs Recall
  performance_v_pr <- performance(prediction_v, "prec", "rec")
  #plot(performance_v_pr, colorize=TRUE)

  result <- list()

  for (i in seq(by = 0.5, from = 1, to = 9.5)) {
    real_conf_matrix <- confusionMatrix(as.factor(ifelse(predict(model, train_data) >= i/10, 1, 0)), train_data$default)
    sensitivity_v <- round(real_conf_matrix$byClass[1], 2)
    specificity_v <- round(real_conf_matrix$byClass[2], 2)
    percision <- round(real_conf_matrix$byClass[5], 4)
    recall <- round(real_conf_matrix$byClass[6], 4)
    balanced_accuracy <- round(real_conf_matrix$byClass[11], 2)
    result[[i]] <- c(
      formula=formula,
      sensitivity_v=sensitivity_v,
      specificity_v=specificity_v,
      percision=percision,
      recall=recall,
      balanced_accuracy=balanced_accuracy,
      result=result
    )

    #lbl <- paste("Threashold:", i/10, "& AUC:", find_auc,
    #             "# Sensitivity:", round(sensitivity_v, 2),
    #             "Specificity:", round(specificity_v, 2),
    #             "# Precision:", round(percision, 2),
    #             "Recall:", round(recall, 2)
    #)
    #fourfoldplot(ctable, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = lbl)
  }

  return (result)
}

names(data)
all_variables <- c("age", "ed", "employ", "address", "income", "debtinc", "creddebt", "othdebt")
var_variations <- strsplit(unlist(lapply(1:8, function(i) combn(all_variables, i, paste, collapse = "+"))), "\\+")

collection <- list()
x <- NA
i <- 1
for (var_variation in var_variations) {
  print(paste("i:", i))
  collection[[i]] <- do_model(train_data, var_variation)

  #if (is.na(x)) {
  #  x <- do_model(train_data, var_variation)
  #}
  #else {
  #  m <- do_model(train_data, var_variation)
  #  x <- rbind(x, m)
  #}
  i <- i + 1
}

#write.csv(x, "x.csv")

#do_model(train_data, c("employ", "address", "creddebt", "age"))

#model <- do_model(train_data, c("employ", "address", "creddebt", "age"))

#library(caret)
#class(predict(model, train_data) > 0.5)
#length(train_data$default)


#names(data)
#model <- glm(default~employ+address+creddebt+age, data=train_data, family = binomial)
#
#predict_val <- predict(model, type = "response")
#find_auc <- round(roc(train_data$default, train_data$employ +
#    train_data$address +
#    train_data$creddebt +
#    train_data$age)$auc, 2)
#for (i in seq(by = 0.5, from = 1, to = 9.5)) {
#  print("------------------------")
#  print(i)
#  print(table(train_data$default, predict_val >= i/10))
#
#  conf_matrix <- table(train_data$default, predict_val >= i/10)
#  ctable <- as.table(conf_matrix, nrow = 2, byrow = TRUE)
#
#  lbl <- paste("Threashold:", i/10, "& AUC:", find_auc)
#  fourfoldplot(ctable, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = lbl)
#}

#library(ROCR)
#prediction_v <- prediction(train_data$employ+train_data$address+train_data$creddebt+train_data$age, train_data$default)
#performance_v <- performance(prediction_v, "tpr", "fpr")
#plot(performance_v, colorize=TRUE)

#install.packages("pracma")

#
#
#library(ROCit)
#plot(rocit(train_data$employ+train_data$address+train_data$creddebt+train_data$age, train_data$default))
#
#roc.plot(train_data$default, train_data$employ+train_data$address+train_data$creddebt+train_data$age, binormal = TRUE)
