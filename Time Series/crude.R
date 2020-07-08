#Crude oil Forecasting

crude <- read.csv(file.choose())
head(crude)

crudets <- ts(crude, frequency = 12, start=c(2000,6))
crudets

#write.csv(crudets,"D:/Data Science/Dr Vinod online classes/Project 4 - Time Series/crudets.csv")

attributes(crudets)
plot(crudets,col='blue')
abline(reg=lm(crudets~time(crudets)))

#EDA

cycle(crudets) #cycle across years.
plot(aggregate(crudets,FUN=mean))#aggregate the cycles and display a year on year trend
boxplot(crudets~cycle(crudets))#Box plot across months will give us a sense on seasonal effect

#decomposing (no seasonality in this case)

library(TTR)
crudets_comp <- decompose(crudets)
#crudets_comp
plot(crudets_comp)
crudets_comp$seasonal
crudets_adjusted <-crudets-crudets_comp$seasonal
crudets_adjusted
plot(crudets_adjusted)
plot(crudets)#not much diff

#stationary check
library(tseries)
plot(crudets)
adf.test(crudets) #not stationary , so we have to make stationary

crudetsd1 <- diff(crudets,differences = 1)
adf.test(crudetsd1)#now stationary
plot(crudetsd1)


#acf and pacf

acf(crudets, lag.max = 20, main="ACF",col='red')
pacf(crudets,lag.max = 20,  main="PACF",col='red')

acf(crudetsd1, lag.max = 20, main="ACF",col='red')
pacf(crudetsd1,lag.max = 20,  main="PACF",col='red')

library(forecast)
auto.arima(crudets)

#Model 1----

crudets_arima <- arima(crudets,order = c(1,1,0))
crudets_arima

crudets_forecast <- forecast(crudets_arima,h=5)
crudets_forecast

plot(crudets_forecast)

plot(crudets_forecast$residuals,col='red',lwd=4)
tsdisplay(crudets_forecast$residuals,lag.max = 20,col='blue',lwd=4)
hist(crudets_forecast$residuals,col='green')
library(psych)
describe(crudets_forecast$residuals) #approx normal
Box.test(crudets_forecast$residuals,lag = 20,type = 'Ljung-Box')#no autocor

accuracy(crudets_arima)#train RMSE=5.0



#Model 2 with train and test----

crudetrain <- read.csv('D:/Data Science/Dr Vinod online classes/Project 4 - Time Series/crude train.csv')
head(crudetrain)

crudetraints <- ts(crudetrain, frequency = 12, start=c(2000,6))
crudetraints

#stationary check
library(tseries)
plot(crudetraints)
adf.test(crudetraints) #not stationary , so we have to make stationary

crudetraintsd1 <- diff(crudetraints,differences = 1)
adf.test(crudetraintsd1)#now stationary
plot(crudetraintsd1)
#model training
auto.arima(crudetraints)

crudetraints_arima <- arima(crudetraints,order = c(1,1,0))
crudetraints_arima
attributes(crudetraints_arima)
#prediction
crudetraints_forecast <- forecast(crudetraints_arima,h=12)
crudetraints_forecast
attributes(crudetraints_forecast)


plot(crudetraints_forecast)

plot(crudetraints_forecast$residuals,col='red',lwd=4)
tsdisplay(crudetraints_forecast$residuals,lag.max = 20,col='blue',lwd=4)
hist(crudetraints_forecast$residuals,col='green')
library(psych)
describe(crudetraints_forecast$residuals) #mean 0 and approx normal
Box.test(crudetraints_forecast$residuals,lag = 20,type = 'Ljung-Box')#no autocor

accuracy(crudetraints_arima)#train RMSE=4.84
actuals <- read.csv('D:/Data Science/Dr Vinod online classes/Project 4 - Time Series/crude test.csv')
actuals
crudetraints_forecast
str(crudetraints_forecast)
# rslt <- data.frame(Actual=actuals,lower=crudetraints_forecast$lower,Upper=crudetraints_forecast$upper)
# rslt <- rslt[,-c(2,4)]







#model 3 Holts winter exp smoothening----

plot(crudetraints) #with trend and no seasonlity

crudetraints_hw <- HoltWinters(crudetraints,gamma = F)
crudetraints_hw

attributes(crudetraints_hw)
#crudetraints_hw$fitted
crudetraints_hw$SSE
plot(crudetraints_hw) #black is original, red is predicted
RMSE <- sqrt(mean(crudetraints_hw$SSE))
RMSE #HW rmse for the training

#adjusting starting value
crudetraints_hw <- HoltWinters(crudetraints,gamma = F,l.start=29.62 , b.start =1.44 )
crudetraints_hw #not much diff

attributes(crudetraints_hw)
#crudetraints_hw$fitted
crudetraints_hw$SSE
plot(crudetraints_hw) #black is original, red is predicted
RMSE <- sqrt(mean(crudetraints_hw$SSE))
RMSE #HW rmse for the training, almost same

#prediction

crudetest_hw <- forecast:::forecast.HoltWinters(crudetraints_hw,h=12)
crudetest_hw
forecast:::plot.forecast(crudetest_hw)

plot(crudetest_hw$residuals,col='red',lwd=4)
tsdisplay(crudetest_hw$residuals,lag.max = 20,col='blue',lwd=4)
hist(crudetest_hw$residuals,col='green')
library(psych)
describe(crudetest_hw$residuals) #mean 0 says normal but kutosis is high
Box.test(crudetest_hw$residuals,lag = 20,type = 'Ljung-Box')#autocor exists
#auto corelation exists , not a best in prediction
accuracy(crudetest_hw)#train RMSE=5.39
actuals <- read.csv('D:/Data Science/Dr Vinod online classes/Project 4 - Time Series/crude test.csv')
actuals
crudetraints_forecast










































