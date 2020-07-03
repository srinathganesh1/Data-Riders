setwd("/home/admin-12/Documents/IMARTICUS/Team/Data-Riders/LogisticRegression")

data <- read.csv("data/bankloan.csv")
data$default <- as.factor(data$default)

load_packages <- function () {
  # Imports
  packages <- c("psych", "dplyr", "car", "pROC", "ROCR", "caret", "tidyverse")
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

plot(train_data)

t.test(train_data$age ~ train_data$default)
t.test(train_data$ed ~ train_data$default)
t.test(train_data$employ ~ train_data$default)
t.test(train_data$address ~ train_data$default)
t.test(train_data$income ~ train_data$default)
t.test(train_data$debtinc ~ train_data$default)
t.test(train_data$creddebt ~ train_data$default)
t.test(train_data$othdebt ~ train_data$default)

# Build the model
do_model <- function(dataset, variable_names, p_threashold) {
  formula <- paste0("default~", paste0(variable_names, collapse = "+"))
  model <- glm(formula, data=dataset, family=binomial)

  #predict_val <- predict(model, type = "response")
  auc_formula_str <- paste0("train_data$", variable_names, collapse = "+")
  auc_formula <- eval(parse(text = auc_formula_str))
  area_under_curve <- round(roc(train_data$default, auc_formula)$auc, 3)

  # ROC Curve
  #prediction_v <- prediction(auc_formula, train_data$default)
  #performance_v_roc <- performance(prediction_v, "tpr", "fpr")
  #plot(performance_v_roc, colorize=TRUE)

  # Performance vs Recall
  #performance_v_pr <- performance(prediction_v, "prec", "rec")
  #plot(performance_v_pr, colorize=TRUE)

  real_conf_matrix <- confusionMatrix(as.factor(ifelse(predict(model, train_data, type="response") >= p_threashold, 1, 0)), train_data$default)
  sensitivity_v <- round(real_conf_matrix$byClass[1], 2)
  specificity_v <- round(real_conf_matrix$byClass[2], 2)
  percision <- round(real_conf_matrix$byClass[5], 4)
  recall <- round(real_conf_matrix$byClass[6], 4)
  balanced_accuracy <- round(real_conf_matrix$byClass[11], 4)

  predicted_value <- as.factor(ifelse(predict(model, test_data, type = "response") >= p_threashold, 1, 0))
  test_conf_matrix <- confusionMatrix(predicted_value, test_data$default, positive="1")

  result <- c(
    formula=formula,
    area_under_curve=area_under_curve,
    sensitivity_v=sensitivity_v,
    specificity_v=specificity_v,
    percision=percision,
    recall=recall,
    balanced_accuracy=balanced_accuracy,
    accuracy=round(unname(real_conf_matrix$overall['Accuracy']), 2),
    test_accuracy=round(unname(test_conf_matrix$overall['Accuracy']), 2)
  )

  #lbl <- paste("Threashold:", i/10, "& AUC:", find_auc,
  #             "# Sensitivity:", round(sensitivity_v, 2),
  #             "Specificity:", round(specificity_v, 2),
  #             "# Precision:", round(percision, 2),
  #             "Recall:", round(recall, 2)
  #)
  #fourfoldplot(ctable, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = lbl)

  return (result)
}

names(data)
head(data)
all_variables <- c("age", "ed", "employ", "address", "income", "debtinc", "creddebt", "othdebt")
var_variations <- strsplit(unlist(lapply(1:8, function(i) combn(all_variables, i, paste, collapse = "+"))), "\\+")

var_length <- length(var_variations)
res_accuracy <- integer(var_length)
res_test_accuracy <- integer(var_length)
res_formula <- character(var_length)
res_area_under_curve <- integer(var_length)
res_sensitivity <- integer(var_length)
res_specificity <- integer(var_length)
res_bal_acc <- integer(var_length)
res_precision <- integer(var_length)
res_recall <- integer(var_length)
res_p_threashold <- integer(var_length)
res_var_count <- integer(var_length)
res_index <- 1

for (var_variation in var_variations) {
  print(var_variation)
  for (i in seq(from = 0.5, to = 9.5, by = 0.5)) {
    p_threashold <- i/10

    res <- do_model(train_data, var_variation, p_threashold=p_threashold)

    res_formula[res_index] <- res[["formula"]]
    res_accuracy[res_index] <- res[["accuracy"]]
    res_test_accuracy[res_index] <- res[["test_accuracy"]]
    res_area_under_curve[res_index] <- res[["area_under_curve"]]
    res_sensitivity[res_index] <- res[["sensitivity_v.Sensitivity"]]
    res_specificity[res_index] <- res[["specificity_v.Specificity"]]
    res_bal_acc[res_index] <- res[["balanced_accuracy.Balanced Accuracy"]]
    res_precision[res_index] <- res[["percision.Precision"]]
    res_recall[res_index] <- res[["recall.Recall"]]
    res_p_threashold[res_index] <- round(p_threashold, 3)
    res_var_count[res_index] <- length(var_variation)
    res_index <- res_index + 1
  }
}

result_df <- data.frame(
  formula=res_formula,
  var_count=res_var_count,
  train_accuracy=res_accuracy,
  test_accuracy=res_test_accuracy,
  p_threashold=res_p_threashold,
  specificity=res_specificity,
  sensitivity=res_sensitivity,
  precision=res_precision,
  recall=res_recall,
  balanced_accuracy=res_bal_acc,
  area_under_curve=res_area_under_curve
)

View(result_df %>% arrange(desc(recall), desc(precision)))

#model <- glm(default~employ+creddebt+address+debtinc, data=train_data, family=binomial)
#pred <- predict(model, train_data, type="response") >= 0.5
#pred <- ifelse(pred, 1, 0)
#
#table(as.factor(pred), train_data$default)
#library(ROCR)
#performance(prediction(pred, train_data$default), "prec", "rec")
#
#confusionMatrix(as.factor(ifelse(predict(model, train_data, type="response") >= 0.5, 1, 0)), train_data$default, positive="1")
#confusionMatrix(as.factor(ifelse(predict(model, train_data, type="response") >= 0.5, 1, 0)), train_data$default, positive="1")$byClass
#confusionMatrix(as.factor(ifelse(predict(model, train_data, type="response") >= 0.5, 1, 0)), train_data$default)$byClass

