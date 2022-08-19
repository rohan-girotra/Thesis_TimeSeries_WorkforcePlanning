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
library(dplyr)
library(hrbrthemes)


df_health=read.csv("C:/D-Drive/AA Subjects/Thesis/dataset final/health.csv")

GP <- ts(df_health$Health, start=c(2007, 1) ,freq=4)
ts_plot(GP, title = "UK General Physicians in Thousand")


population_UK <- ts(df_health$population_UK, start=c(2007, 1) ,freq=4)
ts_plot(population_UK, title = "UK Population (Million)")  

gdp_UK <- ts(df_health$gdp_UK, start=c(2007, 1) ,freq=4)
ts_plot(gdp_UK , title = "UK quarter wise GDP (Million)") 

yplot<-cbind(GP,population_UK,gdp_UK)
plot(yplot)
#############################
cor(population_UK, GP, method = c("pearson"))
cor(population_UK, GP, method = c("spearman"))
cor(GP, population_UK, method = c("spearman"))


grangertest(population_UK, GP, order = 4)
grangertest(GP, population_UK, order = 4)

####################
attach(mtcars)
par(mfrow=c(2,1))
#dev.off()
acf(GP)  # MA
pacf(GP) # AR




attach(mtcars)
par(mfrow=c(2,1))
acf(population_UK)
pacf(population_UK)

ndiffs(GP, alpha = 0.05, test = c('adf'))
ndiffs(population_UK, alpha = 0.05, test = c('adf'))

diff_GP <- diff(GP)
diff_pop1 <- diff(population_UK)
diff_pop <- diff(diff_pop1)

ndiffs(diff_GP, alpha = 0.05, test = c('adf'))
ndiffs(diff_pop, alpha = 0.05, test = c('adf'))

########
grangertest(diff_GP, diff_pop1, order = 4)
grangertest(diff_pop1, diff_GP, order = 4)
########

par(mfrow=c(2,1))
#dev.off()
acf(diff_GP)
pacf(diff_GP)


par(mfrow=c(2,1))
acf(diff_pop)
pacf(diff_pop)

yplot= cbind(GP, population_UK)
#yplot= cbind(diff_GP, diff_pop1)

dev.off()
#acf(yplot)
pacf(yplot)


yplot= cbind(diff_GP, tspop1)
plot(yplot, main= "General Physician and Population (Thousand)")

df_combined<-data.frame(GP,population_UK)
#df_combined<-data.frame(diff_GP, tspop1)

VARselect(yplot, lag.max=30)

#checking model
detach("package:MTS", unload = TRUE)
model <-VAR(yplot,p=6,type='const')
residuals(model)
#serial.test(model, lags.pt=15, type='PT.asymptotic')

summary(model)

checkresiduals(model)

#plot(serial.test(model))
model <-VAR(yplot,p=6,type='const',season = 4)
values<-predict(model, n.ahead=6)
accuracy(values)
plot(values)
fcst = forecast(model)
accuracy(fcst)
plot(fcst)
fcst
plot(fcst , main= "Forecast from VAR model for 6 quarters ahead")

#extracting and appending in main dataset
popp<-values$fcst$population_UK[,'fcst']
nurp<-values$fcst$GP[,'fcst']

popp<-ts(popp, start=c(2022, 2) ,freq=4)
nurp<-ts(nurp, start=c(2022, 2) ,freq=4)

covp<-data.frame(popp,nurp)
colnames(covp)<-c("pop","nur")
covp

df_combined$population_UK
covp$pop
covp$nur

pop_x<-c(df_combined$population_UK,covp$pop)
nurse_y<-c(df_combined$GP,covp$nur)
nurse_y1 <- ts(nurse_y, start=c(2007, 1) ,freq=4)
ts_plot(nurse_y1,
        line.mode =  "lines+markers")

fulldataset<-data.frame(nurse_y,pop_x)
fulldataset<-ts(fulldataset, start=c(2007, 1) ,freq=4)
fulldataset<-ts(fulldataset, start=c(2007, 1) ,freq=4)


ts_plot(nurse_y1,
        line.mode =  "lines+markers")

#series test
serial.test(model, lags.pt=10, type='PT.asymptotic')



