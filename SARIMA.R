library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(TSstudio)
library(vars)
library(MTS)


df_health=read.csv("C:/D-Drive/AA Subjects/Thesis/dataset final/health.csv")
GP <- ts(df_health$Health, start=c(2007, 1) ,freq=4)

Nursing_and_midwifery <- ts(df_health$Nursing_and_midwifery, start=c(2007, 1) ,freq=4)

gdp_UK <- ts(df_health$gdp_UK, start=c(2007, 1) ,freq=4)

population_UK <- ts(df_health$population_UK, start=c(2007, 1) ,freq=4)

yplot<- cbind(GP,Nursing_and_midwifery)
plot(yplot)

yplot<- cbind(gdp_UK,GP)
plot(yplot)
#################Test ADF######
cor(gdp_UK, GP, method = c("pearson"))

#ndiffs uses a unit root test to determine the number of 
#differences required for time series x to be made stationary.

ndiffs(Nursing_and_midwifery, alpha = 0.05, test = c('adf'))
ndiffs(GP, alpha = 0.05, test = c('adf'))

diff1_Nurse <- diff(Nursing_and_midwifery, lag = 1, differences = 1) #this will calculate the diff=1
diff1_GP <- diff(GP, lag = 1, differences = 1)
ts_plot(diff1_Nurse)
ts_plot(diff1_GP)

ndiffs(diff1_Nurse, alpha = 0.05, test = c('adf'))
ndiffs(diff1_GP, alpha = 0.05, test = c('adf'))

######Checking ACF and PACF####
attach(mtcars)
par(mfrow=c(2,1))
acf(Nursing_and_midwifery)   # MA model
pacf(Nursing_and_midwifery)  # AR

attach(mtcars)
par(mfrow=c(2,1))
acf(GP)
pacf(GP)

attach(mtcars)
par(mfrow=c(2,1))
acf(diff1_Nurse) #MA
pacf(diff1_Nurse) #AR

attach(mtcars)
par(mfrow=c(2,1))
acf(diff1_GP)
pacf(diff1_GP)

yplot<- cbind(pacf(diff1_Nurse),acf(diff1_Nurse))
plot(yplot)

##############################Auto Model NAM###############
# Nursing and midwifery
model_nam=auto.arima(ts(df_health$Nursing_and_midwifery,frequency=4,start=c(2007, 1)))
summary(model_nam)
checkresiduals(model_nam)

predict_nam=forecast(model_nam,h=8)
summary(predict_nam)
checkresiduals(predict_nam)
plot(predict_nam)
#ARIMA(0,1,0)(0,0,1)[4]

############  Seasonal Model on NAM
model_Seasonal_NAm <- Arima(df_health$Nursing_and_midwifery, order=c(2,1,2), seasonal=list(order=c(1,1,1),period=4))
summary(model_Seasonal_NAm)
checkresiduals(model_Seasonal_NAm)

predict_season_NAM=forecast(model_Seasonal_NAm,h=8)
plot(predict_season_NAM )
plot(predict_season_NAM ,main="Forecast from ARIMA(2,1,2),(1,1,1)4  for Nurse & Midwifes (Thousand) in UK")
#predict_season_NAM
#(4,1,1), seasonal=list(order=c(0,1,0),period=4))

############  Seasonal Model on GP
model_Seasonal <- Arima(df_health$Health, order=c(3,1,1), seasonal=list(order=c(0,1,1),period=4))
summary(model_Seasonal)

checkresiduals(model_Seasonal)

predict_season=forecast(model_Seasonal,h=8)
plot(predict_season ,main=" Forecast from ARIMA(3,1,1),(0,1,1)4  for GP(Thousand) in UK")


##############################Auto Model GP###############

model_GP=auto.arima(ts(df_health$Health,frequency=4,start=c(2007, 1)))
summary(model_GP)
checkresiduals(model_GP)

predict_GP=forecast(model_GP,h=8)
plot(predict_GP)
