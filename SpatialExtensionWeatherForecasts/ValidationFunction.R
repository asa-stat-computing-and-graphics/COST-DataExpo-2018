#############################################
## This is our expo validation function
## The basic idea is that the NCDC data has been cleaned
## of errors (based on flagged observations in the original SAS
## processing). So for each expo site location we build a little window
## by +/- 2 degrees lat and long (essentially a 'square'). We look at
## all the NCDC in that region and compare the expo temp to the NCDC temps
## If the expo temp is more than 25 greater than the max NCDC temp on a given day
## or less than 25 degrees lower than the max ncdc temp on a given day, it is likely 
## "wrong" with some sort of data entry error or something
## We do the same for the observed minimum temperatures.
#
## In one last process, we rename longitude and latitude to long and lat
## for consistency with all the mapping tools.

library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(tidyr)
#setwd("C:/Users/Benjamin Schweitzer/Desktop/Spring 2018/Research")
rm(list=ls())
gc()

load("fullyMergedDataExpo.RData")
load("fullyProcessedNCDCdataBig.RData")

ncdc.locations <- ncdc.all %>%
  dplyr::select(ID,lat,long) %>%
  distinct()

cleanExpoData <- function(our.AirPtCd) {
  sub.expo.data <- expo.data %>%
    filter(AirPtCd == our.AirPtCd)
  lat <- sub.expo.data[1,]$latitude
  long <- sub.expo.data[1,]$longitude
  blah1 <- cut(ncdc.locations$lat,c(lat-2,lat+2))
  blah2 <- cut(ncdc.locations$long,c(long-2,long+2))
  in.bubble <- ncdc.locations[!is.na(blah1) & !is.na(blah2),]
  ncdc.inBubble <- ncdc.all %>% filter(ID %in%
                                         in.bubble$ID)
  clean.date <- function(our.date){
    sub.expo.data.our.date <- sub.expo.data %>% filter(Date==our.date)
    ncdc.inBubble.ourDate <- ncdc.inBubble %>% filter(Date == our.date)
    max.max <- max(ncdc.inBubble.ourDate$TMAXF, na.rm=TRUE)
    min.max <- min(ncdc.inBubble.ourDate$TMAXF, na.rm=TRUE)
    max.min <- max(ncdc.inBubble.ourDate$TMINF, na.rm=TRUE)
    min.min <- min(ncdc.inBubble.ourDate$TMINF, na.rm=TRUE)
    prcp.max <- max(ncdc.inBubble.ourDate$PRCP, na.rm=TRUE)
    if(sum(ifelse(sub.expo.data.our.date$ObsMaxTempF >= max.max+25 | sub.expo.data.our.date$ObsMaxTempF <= min.max-25, 1, 0), na.rm=TRUE) ){
      expo.data[expo.data$AirPtCd==our.AirPtCd & expo.data$Date==our.date, ]$ObsMaxTempF <<- NA
      cnt.max <<- cnt.max + 1
    }
    if(sum(ifelse(sub.expo.data.our.date$ObsMinTempF >= max.min+25 | sub.expo.data.our.date$ObsMinTempF <= min.min-25, 1, 0), na.rm=TRUE) ){
      expo.data[expo.data$AirPtCd==our.AirPtCd & expo.data$Date==our.date, ]$ObsMinTempF <<- NA
      cnt.min <<- cnt.min + 1
    }
    if(sum(ifelse(sub.expo.data.our.date$PrecipitationIn >= prcp.max+10, 1, 0), na.rm=TRUE) ) {
      expo.data[expo.data$AirPtCd==our.AirPtCd & expo.data$Date==our.date,]$PrecipitationIn <<- NA
      cnt.prcp <<- cnt.prcp + 1
    }
  }
  sapply(unique(sub.expo.data$Date), clean.date)
  c(cnt.max, cnt.min, cnt.prcp)
}

cnt.max <- cnt.min <- cnt.prcp <- 0
loc <- unique(expo.data$AirPtCd)
ptm <- proc.time()
sapply(loc, cleanExpoData)
print(proc.time() -ptm)
# 
#      KBHB KBGR KPWM KBOS KPVD KMHT KMPV KCEF KHVN KALB KNYC KACY KPHL KDOV KAVP KART KSYR KDMH KOFP KILM KRDU KBUF KCHS KAGC KROA KMIA KCLT KCUB KSVN
# [1,]    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# [2,]    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# [3,]    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#      KBKL KCRW KNIP KEYW KTPA KOSU KDET KTYS KANJ KATL KLUK KGRR KSDF KEYE KMXF KBNA KBHM KMDW KMKE KBFM KSPI KMEM KNEW KHKS KCPS KDBQ KDLH KHOT KMSP
# [1,]    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    1    1
# [2,]    0    0    0    0    0    0    0    0    0    0    0    0    0    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
# [3,]    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#      KSGF KDSM KBAD KMKC KLNK KFSD KDAL KFAR KAAO KTIK KSKF KPIR KLBF KBIS KGCK KAMA KCNM KTAD KCYS KBJC KSAF KELP KGJT KLND KHVR KOLS KPVU KFLG KSLC
# [1,]    1    1    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    3    3    3    3    3    3    3    3    3    3    3
# [2,]    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    2    2    2    2    2    2    2    2    2    2    2    2    2
# [3,]    0    0    0    0    0    0    0    0    0    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
#      KIDA KHLN KPHX KMLF KSGU KSMN KEED KVGT KBOI KLWS KP68 KSAN KGEG KBKE KCQT KFAT KRNO KSAC KLMT KBFI KSFO KVUO KEUG KHQM PAMR PHNL
# [1,]    3    4    4    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6
# [2,]    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    3    3    3    3    3    4
# [3,]    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
# > print(proc.time() -ptm)
# user  system elapsed 
# 660.940   1.040 662.556 


## Rename longitude and latitude for consistency with everything else
names(expo.data)[27:28] <- c("long", "lat")

save(expo.data, file="CleanedDataExpo.RData")
