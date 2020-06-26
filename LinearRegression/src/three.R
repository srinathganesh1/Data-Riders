model <- do_liner_model("Sale_Price", training_data, 10, 1, 0.001)

model <- lm(Sale_Price~Screen_Lobby_Area+Garage_Size+Kitchen_Above_Grade+First_Floor_Area+Second_Floor_Area+BsmtUnfSF+BsmtFinSF1+BsmtFinSF2+Remodel_Year+House_Condition+Construction_Year+Sale_Type.ConLD+Functional_Rate.MajD1+Functional_Rate.MajD2+Functional_Rate.MD+Kitchen_Quality.Ex+Air_Conditioning.N+Heating_Quality.Ex+Foundation_Type.PC+Exterior_Condition.Gd+Roof_Quality.CT+Condition2.PosN+Neighborhood.Edwards+Property_Slope.MS+Zoning_Class.Commer, data=training_data)
summary(model)

var_three <- names(model$coefficients)[-1]

validate_model()