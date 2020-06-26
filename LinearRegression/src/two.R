model <- do_liner_model("Sale_Price", training_data, 10, 3, 0.001)
summary(model)

var_two <- names(model$coefficients)[-1]

validate_model()