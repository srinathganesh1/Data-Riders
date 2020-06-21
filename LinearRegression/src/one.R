model <- do_liner_model("Sale_Price", training_data, 10, 5, 0.05)
summary(model)

var_one <- names(model$coefficients)[-1]