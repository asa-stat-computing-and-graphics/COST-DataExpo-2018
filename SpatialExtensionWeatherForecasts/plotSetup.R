#######################################
## Data aggregation and processing
##   needed to make our maps/error plots

library(tidyverse)
library(ggvoronoi)
library(ggthemes)
library(rgeos)
library(raster)

load("ncdcBigDataWithForecast.RData")

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


############################################
## All of North America Map
############################################
getSmallPolys <- function(poly, minarea=0.01) {
  # Get the areas
  areas <- lapply(poly@polygons, 
                  function(x) sapply(x@Polygons, function(y) y@area))
  
  # Quick summary of the areas
  print(quantile(unlist(areas)))
  
  # Which are the big polygons?
  bigpolys <- lapply(areas, function(x) which(x > minarea))
  length(unlist(bigpolys))
  
  # Get only the big polygons and extract them
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]] >= 1){
      poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
      poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
    }
  }
  return(poly)
}
mex <- getData("GADM", country="Mexico", level=1)
can <- getData("GADM", country="Canada", level=1)
usa <- getData("GADM", country="USA", level=1)

can.prov <- gSimplify(can, tol=0.005, topologyPreserve=TRUE)
can.prov <- getSmallPolys(can.prov)

mex.state <- gSimplify(mex, tol=0.005, topologyPreserve=TRUE)
mex.state <- getSmallPolys(mex.state)

rm(can, mex)
all.state <- do.call(bind, list(can.prov,mex.state))
cnt <- 0
my.states <- NULL
for(i in 1:length(all.state) ) {
  for(j in 1:length(all.state@polygons[[i]]@Polygons) ) {
    cnt <- cnt + 1
    tmp.df <- data.frame(all.state@polygons[[i]]@Polygons[[j]]@coords)
    names(tmp.df) <- c("long", "lat")
    tmp.df$group = cnt
    my.states <- rbind(my.states, tmp.df)
  }
}

## Make those handful of Positive longitude Alaksa points negative
# for(i in 1:length(usa.state@polygons[[2]]@Polygons)) {
#   usa.state@polygons[[2]]@Polygons[[i]]@coords[,1] <- 
#     ifelse(usa.state@polygons[[2]]@Polygons[[i]]@coords[,1]>0, 
#            abs(usa.state@polygons[[2]]@Polygons[[i]]@coords[,1])-360,
#            usa.state@polygons[[2]]@Polygons[[i]]@coords[,1])
# }
usa_cont <- map_data("usa")
alaska <- map_data("world", "usa") %>% filter(subregion == "Alaska")
hawaii <- map_data("world", "usa") %>% filter(subregion == "Hawaii")
florida <- map_data("world", "usa") %>% filter(subregion == "Florida")

outline.map <- rbind(usa_cont, alaska, hawaii,florida, map_data("state"))
outline.map <- outline.map %>%
  mutate(group = paste(region, subregion, group, sep = '.'), # Need 'group' variable to be a unique variable now, wasn't from rbinding multiple together
         long = ifelse(long>0, long-360, long)) %>%          # Adjust those islands in Alaska
  dplyr::select(-order,-region,-subregion)
na.map <- rbind(my.states,outline.map)

# ggplot(all.state) + geom_path(aes(x=long,y=lat,group=group))


# ggplot(my.states) + geom_path(aes(x=long,y=lat,group=group))
# rm(alaska, canada, florida, hawaii, mexico, usa_cont)

ncdc.lasso.forecast.tmax.summary <- as.data.frame(ncdc.lasso.forecast.tmax.summary)
ncdc.lasso.forecast.tmin.summary <- as.data.frame(ncdc.lasso.forecast.tmin.summary)

ncdc.hull.forecast.tmax.summary <- as.data.frame(ncdc.hull.forecast.tmax.summary)
ncdc.hull.forecast.tmin.summary <- as.data.frame(ncdc.hull.forecast.tmin.summary)


ptm <- proc.time()
northAmerica.lasso.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.lasso.forecast.tmax.summary,x="long",y="lat",outline=na.map))
northAmerica.lasso.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.lasso.forecast.tmin.summary,x="long",y="lat",outline=na.map))

northAmerica.hull.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.hull.forecast.tmax.summary,x="long",y="lat",outline=na.map))
northAmerica.hull.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.hull.forecast.tmin.summary,x="long",y="lat",outline=na.map))
print(proc.time() - ptm)

usa_cont <- map_data("usa")
florida <- map_data("world", "usa") %>% filter(subregion == "Florida")
usa.outline <- rbind(usa_cont, florida)
usa.outline <- usa.outline %>% 
  mutate(group = paste(region, subregion, group, sep = '.'))
rm(usa_cont, florida)

ptm <- proc.time()
usa.lasso.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.lasso.forecast.tmax.summary,x="long",y="lat",outline=usa.outline))
usa.lasso.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.lasso.forecast.tmin.summary,x="long",y="lat",outline=usa.outline))

usa.hull.max <- fortify_voronoi(
  voronoi_polygon(data=ncdc.hull.forecast.tmax.summary,x="long",y="lat",outline=usa.outline))
usa.hull.min <- fortify_voronoi(
  voronoi_polygon(data=ncdc.hull.forecast.tmin.summary,x="long",y="lat",outline=usa.outline))
print(proc.time() - ptm)


save(usa.lasso.max, usa.lasso.min, northAmerica.lasso.max, northAmerica.lasso.min,
     usa.hull.max, usa.hull.min, northAmerica.hull.max, northAmerica.hull.min,
     expo.summary, ncdc.lasso.forecast.tmax.summary, ncdc.lasso.forecast.tmin.summary,
     ncdc.hull.forecast.tmax.summary, ncdc.hull.forecast.tmin.summary,
     na.map, usa.outline,
     file="stuffForPlots.RData")
