model <- lm(Sale_Price~Condition2.PosN+Roof_Quality.CT+Kitchen_Quality.Ex+
  House_Condition+Construction_Year+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+
  First_Floor_Area+Second_Floor_Area+Garage_Size, data=training_data)

summary(model)


validate_model()