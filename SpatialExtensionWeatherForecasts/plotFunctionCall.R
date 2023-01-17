###################################
## A function to build temperature error plots.

library(tidyverse)
library(ggvoronoi)
library(ggthemes)

load("stuffForPlots.RData")

temp.plots <- function(days, tmax=TRUE, map="usa", zoom=FALSE, avg=TRUE, expo=TRUE, expo.var=FALSE, ncdc=TRUE, paths=FALSE, lasso=TRUE) {
  word.days <- c("One", "Two", "Three", "Four", "Five")[days]
  if(expo.var) {
    if(tmax) {
      usa.tmp <- usa.lasso.max
      northAmerica.tmp <- northAmerica.lasso.max
      our.title <- paste(word.days, "Day Forecast Maximum Temperature (F)")
    } else {
      usa.tmp <- usa.lasso.min
      northAmerica.tmp <- northAmerica.lasso.min
      our.title <- paste(word.days, "Day Forecast Minimum Temperature (F)")
    }
  } else if(lasso) {
    if(tmax) {
      usa.tmp <- usa.lasso.max
      northAmerica.tmp <- northAmerica.lasso.max
      our.title <- paste(word.days, "Day LASSO Forecast Maximum Temperature (F)")
    } else {
    usa.tmp <- usa.lasso.min
    northAmerica.tmp <- northAmerica.lasso.min
    our.title <- paste(word.days, "Day LASSO Forecast Minimum Temperature (F)")
    }
  } else {
    if(tmax) {
      usa.tmp <- usa.hull.max
      northAmerica.tmp <- northAmerica.hull.max
      our.title <- paste(word.days, "Day Hull Forecast Maximum Temperature (F)")
    } else {
      usa.tmp <- usa.hull.min
      northAmerica.tmp <- northAmerica.hull.min
      our.title <- paste(word.days, "Day Hull Forecast Minimum Temperature (F)")
    }
    
  }
  if(avg) {
    usa.tmp$tmpVar <- usa.tmp[,(days+7)]
    northAmerica.tmp$tmpVar <- northAmerica.tmp[,(days+7)]
    expo.locs <- expo.summary %>%
      dplyr::filter(DaysDiff==days) %>%
      mutate(tmpVar=ifelse(tmax, meanTMAX, meanTMIN),
             varVar=ifelse(tmax, stdTMAX, stdTMIN)) %>%
      dplyr::select(lat, long, tmpVar, varVar) %>%
      ungroup()
  } else {
    usa.tmp$tmpVar <- usa.tmp[,(days+12)]
    northAmerica.tmp$tmpVar <- northAmerica.tmp[,(days+12)]
    expo.locs <- expo.summary %>%
      dplyr::filter(DaysDiff==days) %>%
      mutate(tmpVar=ifelse(tmax, stdTMAX, stdTMIN) ) %>%
      dplyr::select(lat, long, tmpVar) %>%
      ungroup()
  }
  if(map=="usa") {
    expo.locs <- expo.locs %>% dplyr::filter(long>-130)
    my.map <- usa.tmp
  } else my.map <- northAmerica.tmp
    
  out <- ggplot(my.map) +
    theme_map() #+
    #theme(panel.background = element_rect(fill = "slategray2", linetype="blank"),
    #      legend.background = element_rect(fill = "slategray2"))
  
  if(ncdc) out <- out + geom_polygon(aes(x=x, y=y ,fill=tmpVar,group=group), size=0)
  else out <- out + geom_polygon(aes(x=x,y=y,group=group), fill="white", color="white", size=0);

  if(paths && map=="usa" && !expo.var) {
    out <- out + geom_path(data=map_data("state"), aes(x=long,y=lat,group=group),color="gray40") +
      geom_path(data=(map_data("world","usa")%>%dplyr::filter(subregion=="Florida")),
                aes(x=long,y=lat,group=group),color="gray40")
  } else if(paths && map=="usa" && expo.var) {
    alaska <- map_data("world", "usa") %>% 
      dplyr::filter(subregion=="Alaska") %>%
      mutate(long=ifelse(long>0, long-360, long),
             lat=lat-40)
    out <- out + geom_path(data=map_data("state"), aes(x=long,y=lat,group=group),color="gray40") +
      geom_path(data=(map_data("world","usa")%>%dplyr::filter(subregion=="Florida")),
                aes(x=long,y=lat,group=group),color="gray40")# +
      #geom_path(data=alaska, aes(x=long,y=lat,group=group),color="gray40")
  } else if(paths) {
    out <- out + 
      geom_path(data=na.map, aes(x=long,y=lat,group=group),color="gray40")
  }
  if(expo & !expo.var) {
    out <- out + 
      geom_point(data=expo.locs, aes(x=long, y=lat, fill=tmpVar), size=3, shape=21, stroke=1.25 )
  }
  if(expo & expo.var) {
    out <- out + 
      geom_point(data=expo.locs, aes(x=long, y=lat, fill=tmpVar, size=varVar), shape=21, stroke=1.25 ) +
      guides(size="none")
  }
  
  if(avg) {
    out <- out + scale_fill_gradientn(name="Average\nForecast\nDeviation", 
                                      values=c(0,0.3,0.5,0.7,1),
                                      colors=c("midnightblue", "blue", "gray99", "orange2", "red"),
                                      #colors=c("gray99", "gray75", "gray50", "gray25", "gray1"),
                                      limits=c(-8,8) )# +
     # ggtitle(our.title,
       #       subtitle = "Mean of Forecasted Error (Observed-Forecasted)")
  } else {
    
    out <- out + scale_fill_gradientn(name=expression("Standard\nDeviation\nForecast\nDeviation"),
                                      values=c(0,0.2,0.45,0.75,1),limits=c(0,15),
                                      colors=c("gray99", "seagreen1", "forestgreen", "darkgreen","navy"))# +
      #ggtitle(our.title,
      #        subtitle = "Standard Deviation of Forecasted Error (Observed-Forecasted)")
  }
  if(map!="usa")
    out <- out + coord_map("bonne", lat0 = 50)
  else
    out <- out + coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
                           xlim = c(-119,-74.25),
                           ylim = c(23,52)) +
    theme(legend.position = c(0.88, 0.02))
  if(zoom)
    out <- out + coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
                           xlim = c(-119,-74.25),
                           ylim = c(23,52)) +
    theme(legend.position = c(0.88, 0.02))
  out
}


save.plot <- function(days, usa, zoom, avg, tmax, expo.only=FALSE, lasso=TRUE) {
  if(usa) map.type <- "usa" else map.type <- "north";
  tmp <- temp.plots(days, map=map.type, zoom=zoom, avg=avg, tmax=tmax, 
                    paths=TRUE ,expo.var=expo.only, ncdc=!expo.only, lasso=lasso)
  if(tmax) file.name <- "max" else file.name <- "min";
  if(lasso) file.name <- paste(file.name, "LASSO", sep="") else file.name <- paste(file.name, "Hull", sep="");
  if(avg) file.name <- paste(file.name, "Mean", sep="")
  else file.name <- paste(file.name, "Std", sep="")
  if(expo.only) {
    file.name <- paste(file.name, "Expo", sep="")
    wid <- 9; hei <- 5.5;
  } else if(usa) {
    file.name <- paste(file.name, "Us", sep="")
    wid <- 9; hei <- 5.5;
  }
  else {
    file.name <- paste(file.name, "Na", sep="")
    wid <- 7.5; hei <- 5;
  }
  if(zoom){
    file.name <- paste(file.name, "Zoom", sep="")
    wid <- 8; hei <- 5;
  }
  file.name <- paste(file.name, days, ".jpg", sep="")
  ggsave(tmp, filename=file.name, width=wid, height=hei, dpi=600)
  0
}

setwd("./plots")
sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, tmax=TRUE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=TRUE, tmax=TRUE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=TRUE, tmax=TRUE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=FALSE, tmax=TRUE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=FALSE, tmax=TRUE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=FALSE, tmax=TRUE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, tmax=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=TRUE, tmax=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=TRUE, tmax=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=FALSE, tmax=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=FALSE, tmax=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=FALSE, tmax=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, expo.only=TRUE, tmax=TRUE)
sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, expo.only=TRUE, tmax=FALSE)



sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, tmax=TRUE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=TRUE, tmax=TRUE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=TRUE, tmax=TRUE, lasso=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=FALSE, tmax=TRUE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=FALSE, tmax=TRUE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=FALSE, tmax=TRUE, lasso=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, tmax=FALSE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=TRUE, tmax=FALSE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=TRUE, tmax=FALSE, lasso=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=FALSE, tmax=FALSE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=FALSE, avg=FALSE, tmax=FALSE, lasso=FALSE)
sapply(1:5, save.plot, usa=FALSE, zoom=TRUE, avg=FALSE, tmax=FALSE, lasso=FALSE)

sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, expo.only=TRUE, tmax=TRUE, lasso=FALSE)
sapply(1:5, save.plot, usa=TRUE, zoom=FALSE, avg=TRUE, expo.only=TRUE, tmax=FALSE, lasso=FALSE)
