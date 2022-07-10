library(fpp2) # Includes forecast package
library(tidyverse)
library(DBI)
library(odbc)
library(janitor)
# library(fredr)
library(lubridate)

rm(list = ls())
isn <- read_csv2("./kh_spots.csv")


DY <- diff(Y)

autoplot(DY) + 
  labs(title ="Time Plot:ISN")

## Data Appears trend stationary: Use to investigate seasonality.

ggseasonplot(DY) +
  labs(title = "Seasonial Plot: Daily Change in ISN")

### Seasonial Plot: sub-series

ggsubseriesplot(DY)


fit <- snaive(Y) # Residual sd: 286.5849 
print(fit)
summary(fit)
checkresiduals(fit)
autoplot(fit,include=700)

## ETS forecast

fit_ets <- ets(Y) # sigma:  218.8133
summary(fit_ets)
checkresiduals(fit_ets)
autoplot(fit_ets,include=700)

fit_mean <- meanf(Y) # sigma:  218.8133
summary(fit_mean)
checkresiduals(fit_mean)
autoplot(fit_ets,include=700)
