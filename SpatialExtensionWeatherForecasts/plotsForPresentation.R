###############################
## Code for plots to explain what we did.
##
## Do not need the REALLY BIG NCDC data.  just the original

library(tidyverse) 
library(ggthemes)
load("../fullyProcessedNCDCdataBig.RData")
load("../CleanedDataExpo.RData")

source("lassoRegSpecialForPlot.R")
source("hullSelectedSitesSpecialForPlot.R")

usa <- map_data("state")
fla <- map_data("world", "usa") %>% 
  dplyr::filter(subregion=="Florida") %>%
  mutate(group=group+56)
usa <- rbind(usa, fla)

##### FT Wayne, IN
special.loc <- "USW00014827"
##### Rapid City, SD
# special.loc <- "USC00396948"

##### Clemson, SC
# special.loc <- "USW00053850"
set.seed(4321)
out <- mySpecialLasso(special.loc)
lasso.airptcd <- out[[1]]
hull.airptcd <- selectedSitesHull(out[[4]])

ncdc.temp.loc <- ncdc.all %>%
  drop_na(TMAX, TMIN) %>%
  distinct(ID, long, lat, state, name) %>%
  filter(!substr(ID, 1,2) %in% c("CA","MX"),
         !state %in% c("AK", "HI")) %>%
  filter(ID != special.loc)  %>%
  mutate(Source="NCNC")
#  mutate(Special = ifelse(ID==special.loc, "Picked", "Normal"),
#         Source="NCDC")

expo.data.loc <- expo.data %>%
  distinct(AirPtCd, city, state, long, lat) %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  mutate(Lasso = ifelse(AirPtCd %in% lasso.airptcd, "Picked", "Nope"),
         Hull = ifelse(AirPtCd %in% hull.airptcd[[2]], "Picked", "Nope"),
         Source="Data Expo")

ncdc.loc.picked <- ncdc.all %>% 
  filter(ID==special.loc) %>%
  drop_na(TMAX, TMIN) %>%
  distinct(ID, long, lat, state, name)

expo.data.loc.picked <- expo.data.loc %>% filter(AirPtCd %in% lasso.airptcd)
expo.data.loc.picked <- inner_join(expo.data.loc.picked, data.frame(AirPtCd=names(out[[2]]$coefficients[-1]*10), Coef=out[[2]]$coefficients[-1]*10))
expo.data.loc.picked <- expo.data.loc.picked %>%
  mutate(Coef.sign = ifelse(Coef<0, "Neg", "Pos"),
         Coef = abs(Coef))
expo.data.loc.hull.picked <- expo.data.loc %>% filter(AirPtCd %in% hull.airptcd)

expo.places <- ggplot() +
  geom_path(data=usa, aes(x=long, y=lat, group=group), col="gray70") +
  geom_point(data=expo.data.loc, aes(x=long, y=lat), fill="gray60", size=3, shape=21, stroke=1.25) +
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank())
  # ggtitle("Data Expo Supplied Locations") 
# expo.places

ncdc.places <- ggplot() +
  geom_path(data=usa, aes(x=long, y=lat, group=group), col="gray70") +
  geom_point(data=ncdc.temp.loc, aes(x=long, y=lat), alpha=0.2) +
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank())
  # ggtitle("NCDC Weather Station Locations") 
# ncdc.places

both.records <- rbind(ncdc.temp.loc %>% dplyr::select(long,lat,Source),
                      expo.data.loc %>% dplyr::select(long,lat,Source))
both.records$Source <- factor(both.records$Source, levels=c("NCDC", "Data Expo"))

both.places <- ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=both.records, aes(x=long, y=lat, fill=Source, alpha=Source, size=Source, shape=Source)) + 
  scale_fill_manual(values=c("black", "gray60")) +
  scale_size_manual(values=c(1.5,3)) + 
  scale_alpha_manual(values=c(0.25,1)) + 
  scale_shape_manual(values=c(16,21)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) + 
  ggtitle("Weather Station Locations") 
# both.places

ncdc.places.special <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=ncdc.temp.loc, aes(x=long, y=lat), alpha=0.25, size=1.5) +
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), size=4, stroke=2, shape=23, fill="gray90") + 
  # scale_fill_manual(values=c("black", "gray60")) +
  # scale_size_manual(values=c(1.5,4)) + 
  # scale_alpha_manual(values=c(0.25,1)) + 
  # scale_shape_manual(values=c(16,23), stroke=c(1,2)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("Selected NCDC Location for Prediction - Fort Wayne, IN")
# ncdc.places.special

both.places.special <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), fill="gray90", size=4, shape=23, stroke=2) +
  geom_point(data=expo.data.loc, aes(x=long, y=lat), fill="gray60", size=3, shape=21, stroke=1.25) +
  scale_color_manual("Special", values=c("Picked"="coral", "Normal"="black")) + 
  scale_size_manual("Special", values=c("Picked"=4, "Normal"=1) )+
  scale_alpha_manual("Special", values=c("Picked"=1, "Normal"=0.2) )+
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("Selected NCDC Location for Prediction - Fort Wayne, IN")
# both.places.special

both.places.lasso.pick <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), fill="gray90", size=4, shape=23, stroke=2) +
  geom_point(data=expo.data.loc, aes(x=long, y=lat, size=Lasso), fill="gray60", shape=21, stroke=1.25) +
  scale_color_manual("Special", values=c("Picked"="coral", "Normal"="black")) + 
  scale_size_manual(values=c(1,4)) +
  scale_alpha_manual(values=c(0.3,1)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("LASSO Selected Locations - Fort Wayne, IN")
# both.places.lasso.pick



both.places.hull.drawn <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), fill="gray90", size=4, shape=23, stroke=2) +
  geom_point(data=expo.data.loc.picked, aes(x=long, y=lat, size=Coef, fill=Coef.sign, shape=Coef.sign), stroke=1.25) +
  geom_path(data=hull.airptcd[[1]], aes(x=long,y=lat), size=2, color="gray40") + 
  scale_color_manual("Special", values=c("Picked"="coral", "Normal"="black")) + 
  scale_fill_manual("Special", values=c("gray60", "gray60")) + 
  scale_shape_manual("Special", values=c(25, 24)) + 
  scale_alpha_manual(values=c(0.3,1)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("Weighted Density Hull of LASSO Selected Locations - Fort Wayne, IN")
# both.places.hull.drawn


both.places.hull.pick <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_path(data=hull.airptcd[[1]], aes(x=long,y=lat), size=2, color="gray40") + 
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), fill="gray90", size=4, shape=23, stroke=2) +
  geom_point(data=expo.data.loc, aes(x=long, y=lat, size=Hull), fill="gray60", shape=21, stroke=1.25) +
  scale_color_manual("Special", values=c("Picked"="coral", "Normal"="black")) + 
  scale_size_manual(values=c(1,4)) +
  scale_alpha_manual(values=c(0.3,1)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("Hull Selected Locations - Fort Wayne, IN")
# both.places.hull.pick

both.places.lasso.model <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill=NA, col="gray70") +
  geom_point(data=ncdc.loc.picked, aes(x=long, y=lat), fill="gray90", size=4, shape=23, stroke=2) +
  geom_point(data=expo.data.loc.picked, aes(x=long, y=lat, size=Coef, fill=Coef.sign, shape=Coef.sign), stroke=1.25) +
  scale_color_manual("Special", values=c("Picked"="coral", "Normal"="black")) + 
  scale_fill_manual("Special", values=c("gray60", "gray60")) + 
  scale_shape_manual("Special", values=c(25, 24)) + 
  scale_alpha_manual(values=c(0.3,1)) + 
  coord_map(projection = "albers", lat0 = 39.8, lat1 = 45,
            xlim = c(-119,-74.25),
            ylim = c(23,52))+
  theme_map() + 
  theme(legend.position="none",
        line=element_blank()) 
  # ggtitle("Fitted Model for Fort Wayne, IN")
# both.places.lasso.model

# expo.places
# ncdc.places
# both.places
# ncdc.places.special
# both.places.special
# both.places.lasso.pick
# both.places.lasso.model
# both.places.hull.pick

ggsave(plot=expo.places, filename="paper/expoPlaces.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=ncdc.places, filename="paper/ncdcPlaces.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places, filename="paper/bothPlaces.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=ncdc.places.special, filename="paper/ftWayneNCDC.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places.special, filename="paper/ftWayneWithExpo.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places.lasso.pick, filename="paper/ftWayneLassoPick.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places.lasso.model, filename="paper/ftWayneLassoModel.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places.hull.drawn, filename="paper/ftWayneHullDrawn.jpg", width=9, height=5.5, dpi=300)
ggsave(plot=both.places.hull.pick, filename="paper/ftWayneHullPicks.jpg", width=9, height=5.5, dpi=300)
