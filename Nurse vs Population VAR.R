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



Nursing_and_midwifery <- ts(df_health$Nursing_and_midwifery, start=c(2007, 1) ,freq=4)
ts_plot(Nursing_and_midwifery)

population_UK <- ts(df_health$population_UK, start=c(2007, 1) ,freq=4)
ts_plot(population_UK)  

population_UK_million <- ts(df_health$population_UK_million, start=c(2007, 1) ,freq=4)
ts_plot(population_UK_million)  

GP <- ts(df_health$Health, start=c(2007, 1) ,freq=4)
ts_plot(GP)

yplot<-cbind(GP,Nursing_and_midwifery,population_UK_million)
plot(yplot , main= "Time series of General Physician, Nurses and Midwifery in (Thousand) and Population(Million) ")


#############################
grangertest(population_UK, Nursing_and_midwifery, order = 3)
grangertest(Nursing_and_midwifery, population_UK, order = 3)

########################
acf(Nursing_and_midwifery)
pacf(Nursing_and_midwifery)
acf(population_UK)
pacf(population_UK)


ndiffs(Nursing_and_midwifery, alpha = 0.05, test = c('adf'))
ndiffs(population_UK, alpha = 0.05, test = c('adf'))

diff_nurse <- diff(Nursing_and_midwifery)
tspop1 <- diff(population_UK)
tspop <- diff(tspop1)

ndiffs(diff_nurse, alpha = 0.05, test = c('adf'))
ndiffs(tspop, alpha = 0.05, test = c('adf'))

par(mfrow=c(2,1))
acf(diff_nurse)
pacf(diff_nurse)
acf(tspop)
pacf(tspop)

yplot= cbind(Nursing_and_midwifery, population_UK)
yplot= cbind(diff_nurse, tspop)
plot(yplot)

df_combined<-data.frame(Nursing_and_midwifery,population_UK)

VARselect(yplot, lag.max=30)

#checking model
detach("package:MTS", unload = TRUE)
model <-VAR(yplot,p=6,type='const')
summary(model)
serial.test(model, lags.pt=15, type='PT.asymptotic')

#plot(serial.test(model))

model <-VAR(yplot,p=10,type='const',season = 4)
values<-predict(model, n.ahead=6)
plot(values)
fcst = forecast(model)
plot(values)
fcst = forecast(model)
plot(fcst , main= "Forecast from VAR model for 2 years ahead for Nurses and Midwifes and Population (Thousand)")

#extracting and appending in main dataset
popp<-values$fcst$population_UK[,'fcst']
nurp<-values$fcst$Nursing_and_midwifery[,'fcst']

popp<-ts(popp, start=c(2022, 2) ,freq=4)
nurp<-ts(nurp, start=c(2022, 2) ,freq=4)

covp<-data.frame(popp,nurp)
colnames(covp)<-c("pop","nur")
covp

df_combined$population_UK
covp$pop
covp$nur

pop_x<-c(df_combined$population_UK,covp$pop)
nurse_y<-c(df_combined$Nursing_and_midwifery,covp$nur)

fulldataset<-data.frame(pop_x,nurse_y)
fulldataset<-ts(fulldataset, start=c(2007, 1) ,freq=4)
plot(fulldataset)

#series test




