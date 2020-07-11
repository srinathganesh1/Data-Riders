setwd("/home/admin-12/Documents/IMARTICUS/Data-Riders/CrudeOil")

load_packages <- function () {
  # Imports
  packages <- c("dplyr", "TTR", "ggplot2", "plotly", "forecast", "psych", "car", "tseries")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
load_packages()

data <- read.csv("data/crudets.csv")
train_data <- data[1:(nrow(data)-12) ,]
test_data <- data[(nrow(data)-11):nrow(data) ,]

# -----------------------------------------
plot_ly(train_data, x= ~X, y= ~Price, name="Price", type="scatter", mode="lines") %>%
  add_trace(y=c(rep(0, 1), diff(ts(train_data$Price), differences = 1)), name="difference 1") %>%
  add_trace(y=c(rep(0, 2), diff(ts(train_data$Price), differences = 2)), name="difference 2")

adf.test(ts(train_data$Price))
adf.test(diff(ts(train_data$Price), differences = 1))
adf.test(diff(ts(train_data$Price), differences = 2))

acf(diff(ts(train_data$Price), differences = 1))
pacf(diff(ts(train_data$Price), differences = 1))
# -----------------------------------------

# -----------------------------------------
fig <- plot_ly(train_data, x= ~X, y= ~Price, name="Price", type="scatter", mode="lines")
for (n in 2:20) {
  sma <- SMA(train_data$Price, n=n)
  residual <- train_data$Price - sma
  residual <- residual[!is.na(residual)]
  rsme <- round(sqrt(mean(residual^2)), 2)
  legend_name <- paste0("SMA", n, " (", rsme, ")")
  print(legend_name)
  fig <- fig %>% add_trace(y=sma, name=legend_name)
}
fig
# -----------------------------------------

# -----------------------------------------
# MOCK PREDICT FUTURE DATA
sma_value <- 5
sma <- SMA(train_data$Price, n=sma_value)

to_forcast_count <- 50
forecast_low <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$lower[,1]))
forecast_mean <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$mean))
forecast_high <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$upper[,1]))
plot_ly(train_data, x= c(train_data$X, rep(229:278)), y= c(train_data$Price, rep(0, to_forcast_count)), name="Price", type="scatter", mode="lines") %>%
  add_trace(y=forecast_low, name="Forecast Low") %>%
  add_trace(y=forecast_mean, name="Forecast Mean") %>%
  add_trace(y=forecast_high, name="Forecast High") %>%
  add_segments(y=0, yend = max(train_data$Price), x=228, xend=228, name="Forecast Start")
sqrt(mean((test_data$Price - as.numeric(forecast(sma, h=to_forcast_count)$mean))^2)) # test data RSME
# -----------------------------------------------------

# -----------------------------------------------------
# Predict with SMA
to_forcast_count <- 12
sma_value <- 5
sma <- SMA(train_data$Price, n=sma_value)
forecast_low <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$lower[,1]))
forecast_mean <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$mean))
forecast_high <- c(rep(0, nrow(train_data)), as.numeric(forecast(sma, h=to_forcast_count)$upper[,1]))
plot_ly(data, x= ~X, y= ~Price, name="Price", type="scatter", mode="lines") %>%
  add_trace(y=forecast_low, name="Forecast Low") %>%
  add_trace(y=forecast_mean, name="Forecast Mean") %>%
  add_trace(y=forecast_high, name="Forecast High") %>%
  add_segments(y=0, yend = max(train_data$Price), x=229, xend=229, name="Forecast Start")
# -----------------------------------------------------


# -----------------------------------------------------
# Predict with HoltWinters
HoltWinters(ts(train_data$Price), gamma = FALSE)
plot(HoltWinters(ts(train_data$Price), gamma = FALSE), col = "blue")

forecast:::forecast.HoltWinters(HoltWinters(ts(train_data$Price), gamma = FALSE), h=5)

to_forcast_count <- 12
do_forecast <- forecast:::forecast.HoltWinters(HoltWinters(ts(train_data$Price), gamma = FALSE), h=to_forcast_count)
forecast_low <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$lower[,1]))
forecast_mean <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$mean))
forecast_high <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$upper[,1]))
plot_ly(data, x= ~X, y= ~Price, name="Price", type="scatter", mode="lines") %>%
  add_trace(y=forecast_low, name="Forecast Low") %>%
  add_trace(y=forecast_mean, name="Forecast Mean") %>%
  add_trace(y=forecast_high, name="Forecast High") %>%
  add_segments(y=0, yend = max(train_data$Price), x=229, xend=229, name="Forecast Start")
sqrt(mean((do_forecast$residuals[!is.na(do_forecast$residuals)])^2))  # training data RSME
sqrt(mean((test_data$Price - as.numeric(do_forecast$mean))^2)) # test data RSME
acf(do_forecast$residuals, na.action = na.pass)
Box.test(do_forecast$residuals, type="Ljung-Box")
forecast:::plot.forecast(do_forecast)

describe(do_forecast$residuals)
hist(do_forecast$residuals)

qqPlot(do_forecast$residuals)

qqnorm(do_forecast$residuals)
qqline(do_forecast$residuals)
# -----------------------------------------------------

# -----------------------------------------------------
# arima
auto.arima(ts(train_data$Price))

arima(x=ts(train_data$Price), order = c(1,1,0))
plot(forecast(arima(x=ts(train_data$Price), order = c(1,1,0)), h=12))

do_arima <- arima(x=ts(train_data$Price), order = c(1,1,0))
do_forecast <- forecast(do_arima, h=12)
forecast_low <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$lower[,1]))
forecast_mean <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$mean))
forecast_high <- c(rep(0, nrow(train_data)), as.numeric(do_forecast$upper[,1]))
plot_ly(data, x= ~X, y= ~Price, name="Price", type="scatter", mode="lines") %>%
  add_trace(y=forecast_low, name="Forecast Low") %>%
  add_trace(y=forecast_mean, name="Forecast Mean") %>%
  add_trace(y=forecast_high, name="Forecast High") %>%
  add_segments(y=0, yend = max(train_data$Price), x=229, xend=229, name="Forecast Start")
sqrt(mean((do_forecast$residuals[!is.na(do_forecast$residuals)])^2))  # training data RSME
sqrt(mean((test_data$Price - as.numeric(do_forecast$mean))^2)) # test data RSME

tsdisplay(residuals(do_arima))
hist(do_arima$residuals)
qqPlot(do_arima$residuals)
Box.test(do_arima$residuals, type="Ljung-Box")
accuracy(do_arima)