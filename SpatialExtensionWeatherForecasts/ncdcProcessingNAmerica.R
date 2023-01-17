###################################
## This code takes the provided data from
##  the DataExpo and combines it 
##  with the NCDC data over the same time window

library(tidyverse)
library(lubridate)

rm(list=ls())
gc()

load("fullyMergedDataExpo.RData")
important.dates <- expo.data %>% 
  distinct(Date)
rm(expo.data)
min.date <- min(important.dates$Date) - 7
max.date <- max(important.dates$Date) + 7

rm(important.dates)
gc()


ncdc14 <- read_csv("ncdc2014namerica.csv")
ncdc14.wide <- ncdc14 %>% spread(key=element, value=value)
rm(ncdc14)

ncdc15 <- read_csv("ncdc2015namerica.csv")
ncdc15.wide <- ncdc15 %>% spread(key=element, value=value)
rm(ncdc15)

ncdc16 <- read_csv("ncdc2016namerica.csv")
ncdc16.wide <- ncdc16 %>% spread(key=element, value=value)
rm(ncdc16)

ncdc17 <- read_csv("ncdc2017namerica.csv")
ncdc17.wide <- ncdc17 %>% spread(key=element, value=value)
rm(ncdc17)

ncdc.all <- rbind(ncdc14.wide, ncdc15.wide, ncdc16.wide, ncdc17.wide)
rm(ncdc14.wide, ncdc15.wide, ncdc16.wide, ncdc17.wide)
gc()

### ncdc.all has 31818945 obs and 7 variables

ncdc.all$date <- ymd(ncdc.all$date)
ncdc.all <- ncdc.all %>%
  dplyr::filter(date >= min.date, date <= max.date) %>%
  dplyr::select(-SNOW, -WESF)

### ncdc.all has 25521884 and 5 variables
ncdc.bak <- ncdc.all  ## Copy for left join
### 

stations <- read.csv("stationsNAmerica.csv")
stations <- stations %>%
  mutate(ID = as.character(ID))

ncdc.all <- left_join(stations, ncdc.bak, by=c("ID") )
ncdc.all <- ncdc.all %>% 
  mutate(long=ifelse(long>100, long-360, long))  ## take the couple of spots in Alaska and swap them

## Down to 2554689 obs in North America
ncdc.all <- ncdc.all %>% mutate(TMIN=TMIN/10, TMAX=TMAX/10,      ## Convert to Celsius from tenths of Celsius
                                TMINF=TMIN*9/5 + 32,             ## Convert to degrees F
                                TMAXF=TMAX*9/5 + 32,
                                PRCP=round(PRCP/10/25.4, 3) )    ## Convert to inches
rm(stations)
names(ncdc.all)[7] <- "Date"

ncdc.all <- ncdc.all %>% drop_na(Date)

## 25514571 by 12

save(ncdc.all, file="fullyProcessedNCDCdataBig.RData")

