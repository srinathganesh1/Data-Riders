model <- do_liner_model("Sale_Price", training_data, 10, 2, 0.05)
summary(model)

library(corrplot)
corrplot(cor(
  training_data %>%
    select(First_Floor_Area, Second_Floor_Area, Garage_Size, Screen_Lobby_Area, W_Deck_Area, Open_Lobby_Area,
           Enclosed_Lobby_Area, Three_Season_Lobby_Area, Pool_Area, Brick_Veneer_Area)
          ), type = "upper", tl.pos = "td", diag = FALSE, tl.cex = 1, order = "hclust",
)

# influencers
# Lot_Size

# First_Floor_Area
# Second_Floor_Area
# Garage_Size
# Screen_Lobby_Area
# W_Deck_Area
# Open_Lobby_Area
# Enclosed_Lobby_Area
# Three_Season_Lobby_Area
# Pool_Area
# Brick_Veneer_Area

# BsmtFinSF1
# BsmtFinSF2

# BsmtUnfSF

# BsmtFinType1
# BsmtFinType2