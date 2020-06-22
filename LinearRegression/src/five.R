model <- lm(Sale_Price~Zoning_Class.Commer+Zoning_Class.FVR+Zoning_Class.RHD+Zoning_Class.RLD+
  Lot_Configuration.CulDSac+Condition2.PosN+House_Type.Twnhs+Overall_Material+
  House_Condition+Roof_Quality.CT+
  BsmtFinType1.GLQ+Heating_Type.Grav+Heating_Quality.Ex+Air_Conditioning.N+Kitchen_Quality.Ex+
  Functional_Rate.MajD2+Functional_Rate.SD+Garage.2Types+Lot_Size+Remodel_Year+Underground_Full_Bathroom+
  Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area, data=training_data)

summary(model)


validate_model()