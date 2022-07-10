library(fpp2)
rm(list=ls())
data <- read.csv("./tute1.csv")

View(data)

mytimeseries <- ts(data[,-1], start=1981, frequency=4)
autoplot(mytimeseries,facets = TRUE)
summary(mytimeseries)
frequency(mytimeseries)
which.min(mytimeseries)
which.max(mytimeseries)

### Retail Data

retaildata <- readxl::read_excel("retail.xlsx", skip=1)

#### Craete Time Series

myts <- ts(retaildata[,"A3349849A"],
           frequency=12, start=c(1982,4))
autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts)
ggAcf(myts)

### More time plots

autoplot(bicoal) + labs(title ="Annual bituminous coal production")
#
autoplot(chicken) +
  labs(title ="Price of chicken in US", subtitle = "(constant dollars): 1924–1993.")
#
autoplot(dole) + 
  labs(title ="Unemployment benefits in Australia",subtitle = "(Jan 1965 – Jul 1992).")
#
autoplot(usdeaths) + 
  labs(title ="Monthly accidental deaths in USA") +
  geom_smooth()
#
autoplot(lynx) + 
  labs(title="Annual Canadian Lynx trappings 1821-1934")
#
autoplot(goog) +
  labs(title="Daily closing stock prices of Google Inc",
       subtitle =" 25 February 2013 and 13 February 2017", 
       caption = "(NASDAQ exchange)",
       x="Days",y="US Dollars")
# 
autoplot(writing) +
  labs(title ="Industry sales for printing and writing paper ",
       subtitle = " Jan 1963 – Dec 1972.",
       y="Thousands of French francs")
#

autoplot(fancy) +
  labs(title = "Monthly sales for a souvenir shop",
       subtitle = "Monthly sales for a souvenir shop",y="Aus. Dollars"
         )
#
autoplot(a10) +
  labs(title ="Monthly anti-diabetic drug subsidy in Australia",
       subtitle = "from 1991 to 2008.",
       y="Aus x1,000,000") +  geom_smooth()
#
autoplot(h02) +
  labs(title ="Monthly corticosteroid drug subsidy in Australia ",
       subtitle="from 1991 to 2008",y="Aus $ x1,000,000",
       caption = "( Pharmaceutical Benefit Scheme for products falling under ATC code H02 as recorded by the Australian Health Insurance Commission.)")

### Question %

ggseasonplot(writing)
# 
ggseasonplot(fancy)
#
ggseasonplot(a10)
#
ggseasonplot(h02,polar = TRUE)
#

### Question 6

## Sales of one-family houses
autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales) 
gglagplot(hsales) 
ggAcf(hsales)

## Accidental deaths in USA
autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths) 
gglagplot(usdeaths) 
ggAcf(usdeaths)

## Quarterly clay brick production
autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq) 
gglagplot(bricksq) 
ggAcf(bricksq)

## Annual average sunspot area (1875-2015)
autoplot(sunspotarea)
ggseasonplot(sunspotarea) # Data are not seasonal
ggsubseriesplot(sunspotarea) # Data are not seasonal
gglagplot(sunspotarea) 
ggAcf(sunspotarea)

#
## US finished motor gasoline product supplied.
autoplot(gasoline)
ggseasonplot(gasoline)
ggsubseriesplot(gasoline) 
gglagplot(gasoline) 
ggAcf(gasoline)

### Question 7: Airline Arrivals

autoplot(arrivals)
ggseasonplot(arrivals)
ggsubseriesplot(arrivals)

### Question 9: Pigs Slaugthered

mypigs <- window(pigs, start=1990)
autoplot(mypigs)
ggAcf(mypigs)

## Question Dow Jones

ddj <- diff(dj)
autoplot(ddj)
ggAcf(ddj)
