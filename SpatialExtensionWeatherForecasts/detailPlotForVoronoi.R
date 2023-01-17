########################################
## Some code to make an isolated plot
##   for the state of Maryland as an 
##   example for the paper.

library(tidyverse)
library(ggvoronoi)
library(ggthemes)

load("stuffForPlots.RData")
load("../fullyProcessedNCDCdataBig.RData")

state.picked <- map_data("state") %>% 
  filter(region=="maryland")

state.picked.ncdc <- ncdc.all %>% 
  filter(state=="MD") %>% 
  select(ID) %>% 
  distinct()

state.lasso.max <- northAmerica.lasso.max %>% 
  filter(ID %in% state.picked.ncdc$ID)

p.state.points <- ggplot() + 
  geom_path(data=state.picked, aes(x=long, y=lat), color="gray40") + 
  geom_point(data=state.lasso.max, aes(x=point.y, y=point.x, fill=meanTMAX1), size=2.5, shape=23, color="gray50", stroke=1.15) + 
  theme_map() + 
  coord_quickmap() + 
  theme(legend.position = c(0.2, 0.15) ) +
  scale_fill_gradientn(name="Average\nForecast\nDeviation", 
                       values=c(0,0.3,0.5,0.7,1),
                       colors=c("midnightblue", "blue", "gray99", "orange2", "red"),
                       #colors=c("gray99", "gray75", "gray50", "gray25", "gray1"),
                       limits=c(-8,8) )# +

p.state.voronoi <- ggplot() + 
  geom_path(data=state.picked, aes(x=long, y=lat), color="gray40") + 
  geom_voronoi(data=distinct(state.lasso.max, point.y, point.x, .keep_all = TRUE), aes(x=point.y, y=point.x, fill=meanTMAX1, group=group), outline=state.picked) + 
  geom_point(data=state.lasso.max, aes(x=point.y, y=point.x, fill=meanTMAX1), size=2.5, shape=23, color="gray50", stroke=1.15) + 
  theme_map() + 
  coord_quickmap() + 
  theme(legend.position = c(0.2, 0.15) ) +
  scale_fill_gradientn(name="Average\nForecast\nDeviation", 
                       values=c(0,0.3,0.5,0.7,1),
                       colors=c("midnightblue", "blue", "gray99", "orange2", "red"),
                       #colors=c("gray99", "gray75", "gray50", "gray25", "gray1"),
                       limits=c(-8,8) )

ggsave(plot=p.state.points, filename="paper/stateVoronoiPoint.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=p.state.voronoi, filename="paper/stateVoronoiExpanded.jpg", width=9, height=5.5, dpi=300)

