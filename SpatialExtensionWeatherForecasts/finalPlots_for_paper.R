########################################
## Code that does a little data aggregation
##   of our forecasts and builds plots
##   for the research article. 



library(tidyverse)
library(ggvoronoi)
library(ggthemes)
library(Cairo)

load("ncdcBigDataWithForecast.RData")

ncdc.forecast.tmax.summary <- ncdc.forecast %>%
  group_by(lat, long) %>%
  summarise(meanTMAX1 = mean(TMAXF-Day1ForecastedTMaxF, na.rm=TRUE),
            meanTMAX2 = mean(TMAXF-Day2ForecastedTMaxF, na.rm=TRUE),
            meanTMAX3 = mean(TMAXF-Day3ForecastedTMaxF, na.rm=TRUE),
            meanTMAX4 = mean(TMAXF-Day4ForecastedTMaxF, na.rm=TRUE),
            meanTMAX5 = mean(TMAXF-Day5ForecastedTMaxF, na.rm=TRUE),
            
            stdTMAX1 = sd(TMAXF-Day1ForecastedTMaxF, na.rm=TRUE),
            stdTMAX2 = sd(TMAXF-Day2ForecastedTMaxF, na.rm=TRUE),
            stdTMAX3 = sd(TMAXF-Day3ForecastedTMaxF, na.rm=TRUE),
            stdTMAX4 = sd(TMAXF-Day4ForecastedTMaxF, na.rm=TRUE),
            stdTMAX5 = sd(TMAXF-Day5ForecastedTMaxF, na.rm=TRUE) ) %>%
  drop_na()

ncdc.forecast.tmin.summary <- ncdc.forecast %>%
  group_by(lat, long) %>%
  summarise(meanTMIN1 = mean(TMINF-Day1ForecastedTMinF, na.rm=TRUE),
            meanTMIN2 = mean(TMINF-Day2ForecastedTMinF, na.rm=TRUE),
            meanTMIN3 = mean(TMINF-Day3ForecastedTMinF, na.rm=TRUE),
            meanTMIN4 = mean(TMINF-Day4ForecastedTMinF, na.rm=TRUE),
            meanTMIN5 = mean(TMINF-Day5ForecastedTMinF, na.rm=TRUE),
            stdTMIN1 = sd(TMINF-Day1ForecastedTMinF, na.rm=TRUE),
            stdTMIN2 = sd(TMINF-Day2ForecastedTMinF, na.rm=TRUE),
            stdTMIN3 = sd(TMINF-Day3ForecastedTMinF, na.rm=TRUE),
            stdTMIN4 = sd(TMINF-Day4ForecastedTMinF, na.rm=TRUE),
            stdTMIN5 = sd(TMINF-Day5ForecastedTMinF, na.rm=TRUE) ) %>%
  drop_na()

rm(ncdc.forecast)
gc()
load("../CleanedDataExpo.RData")

############################################
## All of North America Map
############################################

usa_cont <- map_data("usa")
alaska <- map_data("world", "usa") %>% filter(subregion == "Alaska")
hawaii <- map_data("world", "usa") %>% filter(subregion == "Hawaii")
florida <- map_data("world", "usa") %>% filter(subregion == "Florida")
mexico <- map_data("world", "mexico")
canada <- map_data("world", "canada")

outline.map <- rbind(usa_cont, alaska, hawaii, mexico, canada, florida)
outline.map <- outline.map %>% 
  mutate(group = paste(region, subregion, group, sep = '.')) %>% # Need 'group' variable to be a unique variable now, wasn't from rbinding multiple together
  filter(long < 100) # Just to ignore that little Alaskan island that is on other side of 180/-180 line
rm(alaska, canada, florida, hawaii, mexico, usa_cont)

ncdc.forecast.tmax.summary <- as.data.frame(ncdc.forecast.tmax.summary)
ncdc.forecast.tmin.summary <- as.data.frame(ncdc.forecast.tmin.summary)
ptm <- proc.time()
northAmerica.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.forecast.tmax.summary,x="long",y="lat",outline=outline.map))
northAmerica.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.forecast.tmin.summary,x="long",y="lat",outline=outline.map))
print(proc.time() - ptm)

usa_cont <- map_data("usa")
florida <- map_data("world", "usa") %>% filter(subregion == "Florida")
usa.outline <- rbind(usa_cont, florida)
usa.outline <- usa.outline %>% 
  mutate(group = paste(region, subregion, group, sep = '.'))
rm(usa_cont, florida)

ptm <- proc.time()
usa.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.forecast.tmax.summary,x="long",y="lat",outline=usa.outline))
usa.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.forecast.tmin.summary,x="long",y="lat",outline=usa.outline))
print(proc.time() - ptm)

load("../CleanedDataExpo.RData")
expo.max.summary <- expo.data %>%
  group_by(lat, long, DaysDiff) %>%
  summarise(meanTMAX = mean(ObsMaxTempF - ForecastMaxTempF, na.rm=TRUE),
            stdTMAX = sd(ObsMaxTempF - ForecastMaxTempF, na.rm=TRUE)) %>%
  dplyr::filter(DaysDiff>0) %>%
  drop_na()

save(usa.max, usa.min, northAmerica.max, northAmerica.min,
     expo.max.summary, ncdc.forecast.tmax.summary, ncdc.forecast.tmin.summary,
     file="stuffForPlots.RData")


# ggplot(usa.max) + 
#   geom_polygon(aes(x=x, y=y ,fill=meanTMAX1,group=group), size=0) + 
#   scale_fill_gradient2(low = "darkblue", high = "red", mid = "gray99", midpoint=0, limits=c(-6.5,6.5), guide=FALSE) +
#   coord_quickmap() + 
#   theme_map() +
#   theme(panel.background = element_rect(fill = "slategray2", linetype="blank"),
#         legend.background = element_rect(fill = "slategray2"))
# 
# ggplot(northAmerica.max) + 
#   geom_polygon(aes(x=x, y=y ,fill=meanTMAX1,group=group), size=0) + 
#   scale_fill_gradient2(low = "darkblue", high = "red", mid = "gray99", midpoint=0, limits=c(-6.5,6.5), guide=FALSE) +
#   coord_quickmap(xlim=c(-70,-130), ylim=c(23.5,50)) + 
#   theme_map() +
#   theme(panel.background = element_rect(fill = "slategray2", linetype="blank"),
#         legend.background = element_rect(fill = "slategray2"))

usa <- getData("GADM",country="USA",level=1)
can <- getData("GADM", country="Canada", level=1)
mex <- getData("GADM", country="Mexico", level=1)

tmax.plots <- function(days, map="usa", zoom=FALSE, avg=TRUE, expo=TRUE, paths=FALSE) {
  varForLocs <- paste(days, " Day Forecasted Maximum Temperature (F)", sep="")
  word.days <- c("One", "Two", "Three", "Four", "Five")[days]
  if(avg) {
    usa.max$tmpVar <- usa.max[,(days+5)]
    northAmerica.max$tmpVar <- northAmerica.max[,(days+5)]
    expo.locs <- expo.max.summary %>%
      dplyr::filter(DaysDiff==days) %>%
      dplyr::select(lat, long, tmpVar=meanTMAX)
  } else {
    usa.max$tmpVar <- usa.max[,(days+10)]
    northAmerica.max$tmpVar <- northAmerica.max[,(days+10)]
    expo.locs <- expo.max.summary %>%
      dplyr::filter(DaysDiff==days) %>%
      dplyr::select(lat, long, tmpVar=stdTMAX)
  }
  if(map=="usa")
    expo.locs <- expo.locs %>% dplyr::filter(long>-130)
  out <- ggplot(if(map=="usa") usa.max else northAmerica.max) + 
    geom_polygon(aes(x=x, y=y ,fill=tmpVar,group=group), size=0) + 
    coord_quickmap() + 
    theme_map() +
    theme(panel.background = element_rect(fill = "slategray2", linetype="blank"),
          legend.background = element_rect(fill = "slategray2"))
  if(paths && map=="usa") {
    my.map <- usa[!us$NAME_1 %in% c("Hawaii", "Alaska"),]
    out <- out + geom_path(data=my.map, aes(x=long,y=lat,group=group),color="gray40")
  }
  else if(paths) {
    out <- out + 
      geom_path(data=usa, aes(x=long,y=lat,group=group),color="gray40") +
      geom_path(data=can, aes(x=long,y=lat,group=group),color="gray40") +
      geom_path(data=mex, aes(x=long,y=lat,group=group),color="gray40")
  }
  if(expo) {
    out <- out + 
      geom_point(data=expo.locs, aes(x=long, y=lat, fill=tmpVar), size=3, shape=21 )
  }
  if(avg) {
    out <- out + scale_fill_gradient2(name="Average\nForecast\nDeviation",
                                     low = "darkblue", high = "red", mid = "gray99", 
                                     midpoint=0, limits=c(-6.5,6.5)) +
      ggtitle(paste(word.days, "Day Forecast Maximum Temperature (F)"),
              subtitle = "Mean of Forecasted Error (Observed-Forecasted)")
  } else {
    out <- out + scale_fill_gradient2(name=expression("Standard Deviation\nForecast\nDeviation"),
                                      low = "gray99", mid = "darkorchid4", high = "navy", 
                                      midpoint=9, limits=c(0,14)) +
      ggtitle(paste(word.days, "Day Forecast Maximum Temperature (F)"),
              subtitle = "Standard Deviation of Forecasted Error (Observed-Forecasted)")
  }
  if(zoom)
    out <- out + coord_quickmap(xlim=c(-70,-130), ylim=c(23.6,50.5))
  out
}

usaMean1 <- tmax.plots(1,paths=TRUE)
usaMean2 <- tmax.plots(2)
usaMean3 <- tmax.plots(3)
usaMean4 <- tmax.plots(4)
usaMean5 <- tmax.plots(5)
naMean1 <- tmax.plots(1, map="north")
naMean2 <- tmax.plots(2, map="north")
naMean3 <- tmax.plots(3, map="north")
naMean4 <- tmax.plots(4, map="north")
naMean5 <- tmax.plots(5, map="north")
naMean1zoom <- tmax.plots(1, map="north", zoom=TRUE)
naMean2zoom <- tmax.plots(2, map="north", zoom=TRUE)
naMean3zoom <- tmax.plots(3, map="north", zoom=TRUE)
naMean4zoom <- tmax.plots(4, map="north", zoom=TRUE)
naMean5zoom <- tmax.plots(5, map="north", zoom=TRUE)

usaStd1 <- tmax.plots(1, avg=FALSE)
usaStd2 <- tmax.plots(2, avg=FALSE)
usaStd3 <- tmax.plots(3, avg=FALSE)
usaStd4 <- tmax.plots(4, avg=FALSE)
usaStd5 <- tmax.plots(5, avg=FALSE)
naStd1 <- tmax.plots(1, map="north", avg=FALSE)
naStd2 <- tmax.plots(2, map="north", avg=FALSE)
naStd3 <- tmax.plots(3, map="north", avg=FALSE)
naStd4 <- tmax.plots(4, map="north", avg=FALSE)
naStd5 <- tmax.plots(5, map="north", avg=FALSE)
naStd1zoom <- tmax.plots(1, map="north", zoom=TRUE, avg=FALSE)
naStd2zoom <- tmax.plots(2, map="north", zoom=TRUE, avg=FALSE)
naStd3zoom <- tmax.plots(3, map="north", zoom=TRUE, avg=FALSE)
naStd4zoom <- tmax.plots(4, map="north", zoom=TRUE, avg=FALSE)
naStd5zoom <- tmax.plots(5, map="north", zoom=TRUE, avg=FALSE)

grid.arrange(naStd1zoom, naStd5zoom, nrow=1)






