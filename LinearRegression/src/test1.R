local_training_data <- training_data
local_training_data$Area <- training_data$First_Floor_Area + training_data$Second_Floor_Area
local_training_data <- local_training_data %>% select(-c(First_Floor_Area, Second_Floor_Area))

model <- lm(Sale_Price~Zoning_Class.Commer+Neighborhood.Edwards+Condition2.PosN+
  Roof_Quality.CT+Heating_Quality.Ex+Air_Conditioning.N+Kitchen_Quality.Ex+
  Functional_Rate.MajD2+House_Condition+Construction_Year+BsmtFinSF1+BsmtFinSF2+
  BsmtUnfSF+Area+Garage_Size+Screen_Lobby_Area
  , data=local_training_data)
summary(model)