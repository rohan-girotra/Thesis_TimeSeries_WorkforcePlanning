library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(tseries)
library(TSstudio)

df_health=read.csv("C:/D-Drive/AA Subjects/Thesis/dataset final/health.csv")

GP <- ts(df_health$Health, start=c(2007, 1) ,freq=4)
ts_plot(GP,
        title = "GP in the UK quarter wise",
        Xtitle = "Date",
        Ytitle = "Thousand",
        line.mode =  "lines+markers",
        Xgrid = TRUE,
        Ygrid = TRUE)

Nursing_and_midwifery <- ts(df_health$Nursing_and_midwifery, start=c(2007, 1) ,freq=4)
ts_plot(Nursing_and_midwifery,
        title = "Nursing and midwifery in the UK quarter wise",
        Xtitle = "Date",
        Ytitle = "Thousand",
        line.mode =  "lines+markers",
        Xgrid = TRUE,
        Ygrid = TRUE)



population_UK <- ts(df_health$population_UK, start=c(2007, 1) ,freq=4)
ts_plot(population_UK,
        title = "Population in the UK quarter wise",
        Xtitle = "Date",
        Ytitle = "Thousand",
        line.mode =  "lines+markers",
        Xgrid = TRUE,
        Ygrid = TRUE)




gdp_UK <- ts(df_health$gdp_UK, start=c(2007, 1) ,freq=4)
ts_plot(gdp_UK,
        title = "GDP of the UK quarter wise",
        Xtitle = "Date",
        Ytitle = "million £",
        line.mode =  "lines+markers",
        Xgrid = TRUE,
        Ygrid = TRUE)

#gdp_UK,   population_UK, GP ,Nursing_and_midwifery
fit_stl= stl(gdp_UK,s.window=5)
plot(fit_stl)

###############

ndiffs(population_UK, alpha = 0.05, test = c('adf'))
ndiffs(gdp_UK, alpha = 0.05, test = c('adf'))
ndiffs(GP, alpha = 0.05, test = c('adf'))
ndiffs(Nursing_and_midwifery, alpha = 0.05, test = c('adf'))

