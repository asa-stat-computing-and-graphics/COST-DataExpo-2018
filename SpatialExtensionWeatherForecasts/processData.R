#################################################
## This file takes the raw Data Expo data (CSV files)
## and does some cleaning (removing NA) and merges
## everything into one data set, expo.data

library(tidyverse)

# setwd("C:/Users/fishert4/Desktop/DataExpo2018")

histWeather <- read_csv("histWeather.csv")
locations <- read_csv("locations.csv")
forecasts <- read_csv("forecastsProcessed.csv")

####
## histWeather: 130457x24
## locations:      113x5
## forecasts:  3191972x5

locations$NumCode <- 1:113
histWeatherLocation <- left_join(histWeather, locations, by='AirPtCd')
names(histWeatherLocation)[1] <- "Date"
names(forecasts)[2] <- "Date"

# Before: 3191972
forecasts <- na.omit(forecasts) ## Missing values will not help us
# Now: 3191892
forecasts <- distinct(forecasts, NumCode, Date, Measure, DateOfForecast, .keep_all=TRUE)
# Now: 3100708
# All rows are now distinct -- Pretty certain the distinct() function will keep the first occurance
forecasts.wide <- forecasts %>%
  spread(key=Measure, value=Value)

## expo.data currently matches the number of rows in histWeather
## each row is a particular date and particular location, combinations are unique
## Now some of these combinations will be replicated because of multiple forecasting days
## for that particular days weather.
expo.data <- inner_join(histWeatherLocation, forecasts.wide, by=c("NumCode", "Date"))

names(expo.data)[2:4] <- c("ObsMaxTempF", "ObsMeanTempF", "ObsMinTempF")
names(expo.data)[31:32] <- c("ForecastMaxTempF", "ForecastMinTempF")
expo.data <- expo.data %>% mutate(DaysDiff=Date-DateOfForecast,
                                  PrecipitationIn=round(as.numeric(ifelse(PrecipitationIn=="T","0.005", PrecipitationIn)),3))

## Now expo.data has 818978 forecast for (upto) 130457 days of weather
save(expo.data, file="fullyMergedDataExpo.RData")


