library(fpp2) # Includes forecast package
library(tidyverse)
library(DBI)
library(odbc)
library(janitor)
library(lubridate)
library(tidymodels)
#
rm(list=ls())
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
summary(beer2)

autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE)

autoplot(beer2) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE)

autoplot(beer2) +
   autolayer(snaive(beer2, h=11),
series="Naïve", PI=FALSE)


autoplot(goog200) +
  autolayer(snaive(goog200, h=40),
            series="snaive", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

