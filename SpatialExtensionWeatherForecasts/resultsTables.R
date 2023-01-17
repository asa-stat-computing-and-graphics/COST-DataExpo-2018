#########################################
## Some data aggregation and processing
##   to calculate some error rates. 

library(tidyverse)
library(xtable)

load("ncdcBigDataWithForecast.RData")

cont.usa.stations <- read_csv("../dataProcessing/stationsNAmerica.csv") %>%
  filter(str_sub(ID,1,2)=="US",
         !state %in% c("HI, AK"))

ncdc.lasso.forecast.tmax.summary <- ncdc.forecast %>%
  group_by(lat, long, ID) %>%
  summarise(meanTMAX1 = mean(TMAXF-Day1LassoForecastedTMaxF, na.rm=TRUE),
            meanTMAX2 = mean(TMAXF-Day2LassoForecastedTMaxF, na.rm=TRUE),
            meanTMAX3 = mean(TMAXF-Day3LassoForecastedTMaxF, na.rm=TRUE),
            meanTMAX4 = mean(TMAXF-Day4LassoForecastedTMaxF, na.rm=TRUE),
            meanTMAX5 = mean(TMAXF-Day5LassoForecastedTMaxF, na.rm=TRUE),
            
            stdTMAX1 = sd(TMAXF-Day1LassoForecastedTMaxF, na.rm=TRUE),
            stdTMAX2 = sd(TMAXF-Day2LassoForecastedTMaxF, na.rm=TRUE),
            stdTMAX3 = sd(TMAXF-Day3LassoForecastedTMaxF, na.rm=TRUE),
            stdTMAX4 = sd(TMAXF-Day4LassoForecastedTMaxF, na.rm=TRUE),
            stdTMAX5 = sd(TMAXF-Day5LassoForecastedTMaxF, na.rm=TRUE) ) %>%
  ungroup() %>%
  drop_na() %>%
  distinct(long, lat, .keep_all=TRUE)

ncdc.lasso.forecast.tmin.summary <- ncdc.forecast %>%
  group_by(lat, long, ID) %>%
  summarise(meanTMIN1 = mean(TMINF-Day1LassoForecastedTMinF, na.rm=TRUE),
            meanTMIN2 = mean(TMINF-Day2LassoForecastedTMinF, na.rm=TRUE),
            meanTMIN3 = mean(TMINF-Day3LassoForecastedTMinF, na.rm=TRUE),
            meanTMIN4 = mean(TMINF-Day4LassoForecastedTMinF, na.rm=TRUE),
            meanTMIN5 = mean(TMINF-Day5LassoForecastedTMinF, na.rm=TRUE),
            stdTMIN1 = sd(TMINF-Day1LassoForecastedTMinF, na.rm=TRUE),
            stdTMIN2 = sd(TMINF-Day2LassoForecastedTMinF, na.rm=TRUE),
            stdTMIN3 = sd(TMINF-Day3LassoForecastedTMinF, na.rm=TRUE),
            stdTMIN4 = sd(TMINF-Day4LassoForecastedTMinF, na.rm=TRUE),
            stdTMIN5 = sd(TMINF-Day5LassoForecastedTMinF, na.rm=TRUE) ) %>%
  ungroup() %>%
  drop_na() %>%
  distinct(long,lat,.keep_all=TRUE)


ncdc.hull.forecast.tmax.summary <- ncdc.forecast %>%
  group_by(lat, long, ID) %>%
  summarise(meanTMAX1 = mean(TMAXF-Day1HullForecastedTMaxF, na.rm=TRUE),
            meanTMAX2 = mean(TMAXF-Day2HullForecastedTMaxF, na.rm=TRUE),
            meanTMAX3 = mean(TMAXF-Day3HullForecastedTMaxF, na.rm=TRUE),
            meanTMAX4 = mean(TMAXF-Day4HullForecastedTMaxF, na.rm=TRUE),
            meanTMAX5 = mean(TMAXF-Day5HullForecastedTMaxF, na.rm=TRUE),
            
            stdTMAX1 = sd(TMAXF-Day1HullForecastedTMaxF, na.rm=TRUE),
            stdTMAX2 = sd(TMAXF-Day2HullForecastedTMaxF, na.rm=TRUE),
            stdTMAX3 = sd(TMAXF-Day3HullForecastedTMaxF, na.rm=TRUE),
            stdTMAX4 = sd(TMAXF-Day4HullForecastedTMaxF, na.rm=TRUE),
            stdTMAX5 = sd(TMAXF-Day5HullForecastedTMaxF, na.rm=TRUE) ) %>%
  ungroup() %>%
  drop_na() %>%
  distinct(long, lat, .keep_all=TRUE)

ncdc.hull.forecast.tmin.summary <- ncdc.forecast %>%
  group_by(lat, long, ID) %>%
  summarise(meanTMIN1 = mean(TMINF-Day1HullForecastedTMinF, na.rm=TRUE),
            meanTMIN2 = mean(TMINF-Day2HullForecastedTMinF, na.rm=TRUE),
            meanTMIN3 = mean(TMINF-Day3HullForecastedTMinF, na.rm=TRUE),
            meanTMIN4 = mean(TMINF-Day4HullForecastedTMinF, na.rm=TRUE),
            meanTMIN5 = mean(TMINF-Day5HullForecastedTMinF, na.rm=TRUE),
            stdTMIN1 = sd(TMINF-Day1HullForecastedTMinF, na.rm=TRUE),
            stdTMIN2 = sd(TMINF-Day2HullForecastedTMinF, na.rm=TRUE),
            stdTMIN3 = sd(TMINF-Day3HullForecastedTMinF, na.rm=TRUE),
            stdTMIN4 = sd(TMINF-Day4HullForecastedTMinF, na.rm=TRUE),
            stdTMIN5 = sd(TMINF-Day5HullForecastedTMinF, na.rm=TRUE) ) %>%
  ungroup() %>%
  drop_na() %>%
  distinct(long,lat,.keep_all=TRUE)

rm(ncdc.forecast)
gc()
load("../CleanedDataExpo.RData")
expo.summary <- expo.data %>%
  group_by(lat, long, state, DaysDiff) %>%
  summarise(meanTMAX = mean(ObsMaxTempF - ForecastMaxTempF, na.rm=TRUE),
            stdTMAX = sd(ObsMaxTempF - ForecastMaxTempF, na.rm=TRUE),
            meanTMIN = mean(ObsMinTempF - ForecastMinTempF, na.rm=TRUE),
            stdTMIN = sd(ObsMinTempF - ForecastMinTempF, na.rm=TRUE) ) %>%
  dplyr::filter(DaysDiff>0, DaysDiff<6) %>%
  drop_na()

my.fun <- function(days) {
expo.summary %>%
  ungroup() %>% 
  dplyr::filter(!state %in% c("Hawaii", "Alaska"),
                DaysDiff==days) %>% 
  summarize(Avg.Day.error = mean(meanTMAX),
            Avg.Day.std = mean(stdTMAX)) 
}

out.tmax <- rbind(expo.tmax.summary <- unlist(c((sapply(1:4, my.fun)))),
             ncdc.lasso.forecast.tmax.summary %>%
               filter(ID %in% cont.usa.stations$ID) %>%
               summarize(Avg1.Day.error = mean(meanTMAX1),
                         Avg1.Day.std = mean(stdTMAX1),
                         Avg2.Day.error = mean(meanTMAX2),
                         Avg2.Day.std = mean(stdTMAX2),
                         Avg3.Day.error = mean(meanTMAX3),
                         Avg3.Day.std = mean(stdTMAX3),
                         Avg4.Day.error = mean(meanTMAX4),
                         Avg4.Day.std = mean(stdTMAX4) ),
             ncdc.hull.forecast.tmax.summary %>%
               filter(ID %in% cont.usa.stations$ID) %>%
               summarize(Avg1.Day.error = mean(meanTMAX1),
                         Avg1.Day.std = mean(stdTMAX1),
                         Avg2.Day.error = mean(meanTMAX2),
                         Avg2.Day.std = mean(stdTMAX2),
                         Avg3.Day.error = mean(meanTMAX3),
                         Avg3.Day.std = mean(stdTMAX3),
                         Avg4.Day.error = mean(meanTMAX4),
                         Avg4.Day.std = mean(stdTMAX4))
) %>% as.data.frame()

rownames(out.tmax) <- c("Expo Data", "LASSO Model", "Hull Model")
out.tmax

xtable(out.tmax)

### Now minimums


my.fun <- function(days) {
  expo.summary %>%
    ungroup() %>% 
    dplyr::filter(!state %in% c("Hawaii", "Alaska"),
                  DaysDiff==days) %>% 
    summarize(Avg.Day.error = mean(meanTMIN),
              Avg.Day.std = mean(stdTMIN)) 
}

out.tmin <- rbind(expo.tmin.summary <- unlist(c((sapply(1:4, my.fun)))),
                  ncdc.lasso.forecast.tmin.summary %>%
                    filter(ID %in% cont.usa.stations$ID) %>%
                    summarize(Avg1.Day.error = mean(meanTMIN1),
                              Avg1.Day.std = mean(stdTMIN1),
                              Avg2.Day.error = mean(meanTMIN2),
                              Avg2.Day.std = mean(stdTMIN2),
                              Avg3.Day.error = mean(meanTMIN3),
                              Avg3.Day.std = mean(stdTMIN3),
                              Avg4.Day.error = mean(meanTMIN4),
                              Avg4.Day.std = mean(stdTMIN4) ),
                  ncdc.hull.forecast.tmin.summary %>%
                    filter(ID %in% cont.usa.stations$ID) %>%
                    summarize(Avg1.Day.error = mean(meanTMIN1),
                              Avg1.Day.std = mean(stdTMIN1),
                              Avg2.Day.error = mean(meanTMIN2),
                              Avg2.Day.std = mean(stdTMIN2),
                              Avg3.Day.error = mean(meanTMIN3),
                              Avg3.Day.std = mean(stdTMIN3),
                              Avg4.Day.error = mean(meanTMIN4),
                              Avg4.Day.std = mean(stdTMIN4))
) %>% as.data.frame()

rownames(out.tmin) <- c("Expo Data", "LASSO Model", "Hull Model")
out.tmin
xtable(out.tmin)
