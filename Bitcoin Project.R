# BITCOIN PRICE PREDICTION
#Submitted by : 21BDA15, 21BDA22, 21BDA42
#Date: 05/07/2022

#===============================================================================================================================
#-------------------------------------------------------------------------------------------------------------------------------


# Load required packages
library(data.table)
library(gridExtra)
library(tidyquant)
library(changepoint)
library(ggfortify)
library(xts)
library(TSA)
library(fUnitRoots)
library(forecast)
library(lmtest)
library(fGarch)
library(rugarch)
library(tseries)
library(ggplot2)
library(dplyr)
library(magrittr)
library(coinmarketcapr)
library(treemap)
library(tidyverse)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)
library(TSstudio)
library(zoo)
library(quantmod)
library(prophet)


windowsFonts(Georgia = windowsFont("Georgia"))


#checking for null values
n_col_wise = sapply(bitcoin.data, function(x) sum(is.na(x)))
n_col_wise


#reading csv file
bitcoin.data = read.csv("BTC-USD.csv")
View(bitcoin.data)
names(bitcoin.data)
summary(bitcoin.data[,c(-1)])

# Accessing data from coinmarketcapr to know which cryptocurrency is popular
key <- 'b8ced24f-5a16-4157-8085-d0682f7c7ae7'
coinmarketcapr::setup(key)

cryptos <- get_crypto_listings(currency = "USD", latest = TRUE)

View(cryptos)

plot_top_currencies(currency="USD",k=5, bar_color = "#354259")

# Examine how the market share is split among the top cryptocurrencies

market_today <- get_marketcap_ticker_all(currency = "USD")
View(market_today)
names(market_today)


# changing the data to time series

class(bitcoin.data)
bitcoin.data$Close<- as.numeric(as.character(gsub(",","",bitcoin.data$Close)))
bitcoin<- ts(as.vector(bitcoin.data$Close), start = c(2014, as.numeric(format(as.Date("2014-04-27"), "%j"))), frequency = 365.25)
class(bitcoin)
View(bitcoin)
# Checking the class of bitcoin.data object showed that the data was read as a data frame, so it was converted to a time series bitcoin.


# DATA EXPLORATION AND VISUALISATION

plot(bitcoin,type="o", main='Time series of daily bitcoin price', ylab='Daily bitcoin closing price')


df_new<-bitcoin.data[, c('Date', 'Close')]
df_new[['Date']]<-as.factor(df_new[['Date']])
df_new[['Date']]<-as.Date(df_new[['Date']],format="%Y-%m-%d") 
head(df_new)
str(df_new)

ts_plot(df_new,
        title = "Bitcoin closing prices 2015-2022",
        Xtitle = "Time",
        Ytitle = "In thousand's",
        slider = TRUE)




r.bitcoin=diff(log(bitcoin))*100 # Continuous compound return
plot(r.bitcoin, xlab="Time", ylab="Return in %", xlim=c(2014,2022), main="Bitcoin Return", col = "#354c7c")

plot(y=bitcoin, x=zlag(bitcoin), ylab='Daily closing price', xlab='Previous closing price',
     main='Daily bitcoin closing price vs previous closing price')


x=zlag(bitcoin) 
index=2:length(x)
cor(bitcoin[index],x[index])


gglagplot(bitcoin, do.lines = F) + 
  scale_color_continuous(low = "#022954", high = "#354c7c", breaks = c(1, 366, 731), labels = c('2019', '2020', '2021')) + 
  scale_y_continuous(breaks = c(0, 20000, 40000, 60000), 
                     labels = c('$0', '$20K', '$40K', '$60K')) +
  scale_x_continuous(breaks = c(20000, 40000, 60000), 
                     labels = c('$20K', '$40K', '$60K'))




par(mfrow=c(1,2))
acf(bitcoin,lag.max = 500, main= "ACF plot of bitcoin")
pacf(bitcoin, lag.max = 500, main="PACF plot of bitcoin")

par(mfrow=c(1,2))
ggAcf(bitcoin, lag.max = 200)  + labs(title = 'ACF' , y = 'Correlation')
ggPacf(bitcoin, lag.max = 200)  + labs(title = 'PACF', y = '')
par(mfrow=c(1,1))

#The Augmented Dickey Fuller Test (ADF)
#The hypotheses for the test:
  
#The null hypothesis for this test is that there is a unit root.
#The alternate hypothesis differs slightly according to which equation you're using. 
#The basic alternate is that the time series is stationary (or trend-stationary).

adf.test(bitcoin)




# DATA TRANSFORMATION

bitcoin.t = BoxCox.ar(bitcoin, method='yule-walker')
bitcoin.t$ci

# log transformation
# Detrending
bitcoin.log = log(bitcoin)
qqnorm(bitcoin.log)
qqline(bitcoin.log, col = 2)

shapiro.test(bitcoin.log)
# series is still non-stationary


# Differencing
diff.bitcoin.t = diff(bitcoin.log ,differences = 1)
plot(diff.bitcoin.t,type='l', ylab='Daily bitcoin closing price', main = "Time Series plot of the first difference")

adf.test(diff.bitcoin.t)
#series is now stationary




# ARIMA MODEL SPECIFICATION


par(mfrow=c(1,2))
acf(diff.bitcoin.t, main="ACF plot of transformed data")
pacf(diff.bitcoin.t, main="PACF plot of transformed data")
par(mfrow=c(1,1))

eacf(diff.bitcoin.t)


auto.arima(diff.bitcoin.t)
#sigma^2 = 0.001527
#sigma: 0.03906405
checkresiduals(auto.arima(diff.bitcoin.t))
#Ljung-Box test: p-value = 0.3454
summary(Arima(diff.bitcoin.t, order = c(2,0,0), include.drift = T))



# ARIMA MODELLING
# Parameter estimation


# ARIMA model estimation

AR = arima(diff.bitcoin.t, order = c(2,0,0))
print(AR)

#plotting the original data and the fitted one

ts.plot(bitcoin, main = "Real and Predicted")
AR_fit = bitcoin - residuals(AR)
points(AR_fit, type ="l", col="red", lty = 2)


# using predict to make 1-step through 5-step forecasts
predict(AR,data=bitcoin, n.ahead = 5)


fit <- Arima(bitcoin, order = c(2,0,0))
fit
autoplot(forecast(fit))

model=Arima(log(bitcoin),c(2,0,0))
plot(forecast(model,h=20), ylab="Log(Bitcoin Price)", xlab="Time")

pred_values=predict(model, n.ahead=10,newxreg = NULL, se.fit = TRUE)
pred_values

fcst3 <- forecast(fit, h=80)
autoplot(fcst3,include=380)
print(summary(fcst3))

#Model SNAIVE(for handling seasonality)
fit <- snaive(diff.bitcoin.t)
print(summary(fit))
checkresiduals(fit)
#Residual sd: 0.0558 
fcst1 <- forecast(fit, h=80)
autoplot(fcst1,include=380)



#Model ETS
fit_ets <- stlf(bitcoin)
print(summary(fit_ets))
checkresiduals(fit_ets)
#sigma:  707.2773
fcst2 <- forecast(fit_ets, h=80)
autoplot(fcst2,include=380)



# Model Prophet
df_prophet <- setNames(df_new, c("ds","y"))
head(df_prophet)


Model1 <- prophet(df_prophet)
Future1 <- make_future_dataframe(Model1, periods = 365)
tail(Future1)


Forecast1 <- predict(Model1, Future1)
tail(Forecast1[c('ds','yhat','yhat_lower','yhat_upper')])


#Plot Forecast
dyplot.prophet(Model1,Forecast1)
prophet_plot_components(Model1,Forecast1)

str(Forecast1)


#Prophet Model Performance
pred <- Forecast1$yhat[1:2814]
actual <- Model1$history$y
plot(actual,pred)
abline(lm(pred~actual), col= 'red')
summary(lm(pred~actual))
#R-squared:  0.8602

x <-cross_validation(Model1, 365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x, metric = 'rmse', rolling_window = 0.1)

