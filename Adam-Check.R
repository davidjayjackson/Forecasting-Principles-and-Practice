library(fpp2) # Includes forecast package
library(tidyverse)
library(DBI)
library(odbc)
library(janitor)
# library(fredr)
library(lubridate)

## Time Series Forecasting Example in RStudio
## YouTube: https://youtu.be/dBNy_A6Zpcc

rm(list=ls())


## https://db.rstudio.com/databases/microsoft-sql-server/
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "superstore", 
                      Trusted_Connection = "True")


### Retail Sales

retail <- read_csv("./real_sales_per_day.csv") %>% janitor::clean_names()
retail$date <- mdy(retail$date)

## convert to time series

Y <- ts(retail$sales_per_day,start=c(1992,1),frequency = 12)
class(Y) # Class ts

## Begin analysis

autoplot(Y) + 
  labs(title ="Time Plot: US Retail Sales",y="Millions of USD 2017")



## Data has as strong trend.
DY <- diff(Y)

autoplot(DY) + 
  labs(title ="Time Plot: Change in US Retail Sales ")

## Data Appears trend stationary: Use to investigate seasonality.

ggseasonplot(DY) +
  labs(title = "Seasonial Plot: Change In Daily Sales")

### Seasonial Plot: sub-series

ggsubseriesplot(DY)

## Forecast with various methods

fit <- snaive(DY) # Residual sd: 286.5849 
print(fit)
summary(fit)
checkresiduals(fit)
autoplot(fit)

## ETS forecast

fit_ets <- ets(Y) # sigma:  218.8133
summary(fit_ets)
checkresiduals(fit_ets)
autoplot(fit_ets)

### auto.arima model
## Best fit: Best model: ARIMA(0,1,1)(2,1,2)[12]
fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE, trace= TRUE)
summary(fit_arima)
checkresiduals(fit_arima)
autoplot(fit_arima)
sqrt(39129) # SD 197.8105

## Forecast using Arima
fcast1 <- forecast(fit_arima,h=24)
autoplot(fcast1)
autoplot(fcast1,include=180) + labs(title ="15 Year")
autoplot(fcast1,include=60) + labs(title = "Last 5 Years") 

