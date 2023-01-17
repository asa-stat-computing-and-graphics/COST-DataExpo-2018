#################################################
## hullSelectedSites.R
##
## A function that selects all the NCDC sites within
##   a convex Hull. 


#need these libraries
library(tidyverse)
library(maptools)
library(sp)

#have the locations loaded and input the weights
selectedSitesHull = function(weights, buffer=0.5){
  if(length(weights)==1) weights <- weights*2;
  
  #NCDC sites as a spatial object
  locations <- expo.data %>%
    dplyr::select(AirPtCd, lat, long) %>%
    distinct(AirPtCd, .keep_all=TRUE)
  
  sp_locs <- SpatialPointsDataFrame(coords = locations %>% dplyr::select(long,lat),
                                   data = locations)
  
  # Airport codes associated with the given weights
  ncdc.picked <- left_join(data.frame(AirPtCd = rep(names(weights), weights) ), locations, by="AirPtCd") %>%
    mutate(long = jitter(long, amount = 0.05),      ## Add a little noise to lat & long
           lat = jitter(lat, amount = 0.05))        ## to ensure non-zero variable in hull
  
  #build a density hull around the weighted sites
  #Convert SpatialLines to SpatialPolygons:
  #buffer a radius around the density hull
  hull <- rgeos::gBuffer(SpatialPolygons( lapply(slot(ContourLines2SLDF(contourLines(MASS::kde2d(x=ncdc.picked$long, 
                                                                                                 y=ncdc.picked$lat,
                                                                                                 n=150,
                                                                                                 lims = c(-179,-50,10,85)))),
                                                      "lines"), function(x) {
                                                        Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], 
                                                                                   "coords"))), ID=slot(x, "ID"))
                                                      })), width = buffer)

  #find which sites fall in the hull and return airport codes
  overlay <- sp_locs %over% hull
  locations$AirPtCd[which(overlay == 1)]
}




