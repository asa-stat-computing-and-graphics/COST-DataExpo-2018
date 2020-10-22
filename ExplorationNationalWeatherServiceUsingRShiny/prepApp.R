list.of.packages <- c("shinythemes", "plyr","dplyr","mapproj", "leaflet","lubridate","shiny","plotly", "sm","zoo","scales","ggplot2","readr","reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(readr)
library(reshape)
library(ggplot2)
library(data.table)
library(shiny)
library(mapproj)
library(leaflet)
library(lubridate)
library(plotly)
library(zoo)
library(scales)
library(plyr)


locations <- read.csv("locations.csv", stringsAsFactors=FALSE)
histWeather <- read.csv("histWeather.csv", stringsAsFactors=FALSE)
forecast <- read.table("forecast.dat", sep = " ", stringsAsFactors=FALSE)

fore_cast = forecast

locations <- as.data.table(locations)
histWeather <- as.data.table(histWeather)
forecast <- as.data.table(forecast)
histWeather[Max_TemperatureF > 200, Max_TemperatureF := 28] #added edit
code_list <- c("All Airports", unique(histWeather[,AirPtCd]))

# XL code: for the map ---------------------------------------------------------

loc <- read.csv("locations.csv", stringsAsFactors=FALSE)
histdata = read.csv("histWeather.csv")
error_max <- read.csv("error_max.csv")
error_min <- read.csv("error_min.csv")
error_max_reduce <- read.csv("error_max_reduce.csv")
error_min_reduce <- read.csv("error_min_reduce.csv")

colnames(fore_cast) = c("locID", "date","res","stat","today")
histdata[as.character(histdata$PrecipitationIn) == "T", "PrecipitationIn"] = factor(1)
histdata$Precipitation = as.numeric(as.character(histdata$PrecipitationIn))
histdata$date2day = sapply(histdata$Date, function(x) as.numeric(x))

#col_list <- names(histdata)
aircode = data.frame(airptid = unique(fore_cast$locID), airptcd = loc$AirPtCd)

error_max <- error_max[-(76:78),]
error_max_reduce <- error_max_reduce[-(76:78),]

error_min <- error_min[-(76:78),]
error_min_reduce <- error_min_reduce[-(76:78),]

summary_max <- apply(error_max[,-1],2,median)[-c(1,4,7)]
summary_max_reduce <- apply(error_max_reduce[,-1],2,median)[-c(1,4,7)]
summary_min <- apply(error_min[,-1],2,median)[-c(1,4,7)]
summary_min_reduce <- apply(error_min_reduce[,-1],2,median)[-c(1,4,7)]

dat_err <- data.frame("maxTemp_original"=summary_max,"maxTemp_updated"=summary_max_reduce,"minTemp_original"=summary_min,"minTemp_updated"=summary_min_reduce)


###################################
## For Temperature Plots
## GV 7/23/2018

print("prepping Temp Data")

plotColor <- "dodgerblue"



## to do list 1
locations$cityID <- 1:nrow(locations)

names(forecast) <- c("cityID", "forecastFOR", "value", "variable", "forecastON")

## to do list 2
forecast$daysfromforecast = as.numeric(as.Date(forecast$forecastFOR)) - as.numeric(as.Date(forecast$forecastON))

tempDat <- forecast[variable == "MaxTemp" | variable =="MinTemp" ]

#delete'M'
tempDat <- tempDat[value!= 'M']

## converting temp predictions to numbers
tempDat[,value :=as.numeric(value)]


## averaging for multiple measurements
#tempDat <- tempDat[,.(value = mean(value)), by = .(cityID, forecastFOR, forecastON, variable, daysfromforecast)]

## merging to get AirPtCode
tempDat <- merge( tempDat, locations)

## adding forecastFOR column for easy merging
histWeather[,forecastFOR := Date]

## mergeing historical and prediction on AirPtcd and ForecastFor (Date)
histWeatherSubset <- histWeather[,.(forecastFOR, AirPtCd, MaxTemp = as.numeric(Max_TemperatureF),
                                    MinTemp = as.numeric(Min_TemperatureF),
                                    Mean_Humidity,Mean_Wind_SpeedMPH,CloudCover,MeanDew_PointF,
                                    Mean_VisibilityMiles,Mean_Sea_Level_PressureIn)] 
## melting dataset
histWeatherSubset <- melt(histWeatherSubset, measure.vars = c("MaxTemp", "MinTemp"),
                          variable.name = "variable", value.name = "histValue")
## averaging ~366 double counts
#histWeatherSubset <- histWeatherSubset[,.(histValue = mean(histValue)), by = .(forecastFOR, AirPtCd, variable)]

## merging data
tempDat <- merge(tempDat, histWeatherSubset, by = intersect(names(tempDat), names(histWeatherSubset)))

## adding variables for plotting
tempDat[,daysfromforecastLABEL := paste0(as.character(daysfromforecast), " Days")]
## cleaning out NAs
NA.rows <- is.na(tempDat$histValue)
tempDat <- tempDat[!NA.rows]

## cleaning out the hist temps that didn't make sense 

tempDat <- tempDat[histValue<200]


#####################################
## num bins for bubble plots
nHistTempBins <- 20
nForecastTempBins <- 30



##################################
## For Precipitaiton Plots
## GV 7/13/2018

print("Prepping precipitation Data")

#histdata <- read.csv("histWeather.csv", stringsAsFactors = FALSE)
#locations <- as.data.table(locations)
#histWeather <- as.data.table(histdata)
#forecast <- as.data.table(forecast)

histWeather$PrecipitationIn[histWeather$PrecipitationIn=="T"] = 0.005

## constructing preipitation data table
precipDat <- forecast[variable == "ProbPrecip" ]

## converting probbaility predictions to numbers
precipDat[,value :=as.numeric(value)]

## averaging multiple probability predictions 
## for a given day
precipDat <- precipDat[,.(value = mean(value)), by = .(cityID, forecastFOR, variable, forecastON, daysfromforecast)]

########################################################
## an aside, there is a problem here; we have many cases
## where more than two precipitaiton predictions are made for a given
## city-forecastFOR-forecastON combination
test <- precipDat[,.(N = .N), by = .(cityID, forecastFOR, forecastON)]
table(test[,N])
##########################################################

precipDat <- precipDat[,.(value = mean(value)),by = .(cityID, forecastFOR, daysfromforecast)]

## merging to get AirPtCode
precipDat <- merge( precipDat, locations)

## Done for Temp Data
## adding forecastfor column for easy merging
##histWeather[,forecastFOR := Date]

## mergeing historical and prediction on AirPtcd and ForecastFor (Date)
histWeatherSubset <- histWeather[,.(forecastFOR, AirPtCd, PrecipitationIn = as.numeric(PrecipitationIn),
                                    Mean_Humidity,Mean_Wind_SpeedMPH,CloudCover,MeanDew_PointF,
                                    Mean_VisibilityMiles,Mean_Sea_Level_PressureIn)] 

## averaging multiple precipitaiton readings 
## for a given day
histWeatherSubset <- histWeatherSubset[,.(PrecipitationIn = mean(PrecipitationIn),
                                          Mean_Humidity = Mean_Humidity, 
                                          Mean_Wind_SpeedMPH = Mean_Wind_SpeedMPH,
                                          CloudCover = CloudCover,
                                          MeanDew_PointF = MeanDew_PointF,
                                          Mean_VisibilityMiles = Mean_VisibilityMiles,
                                          Mean_Sea_Level_PressureIn = Mean_Sea_Level_PressureIn), 
                                       by = .(forecastFOR, AirPtCd)]

## merging data
precipDat <- merge(precipDat, histWeatherSubset, by = intersect(names(precipDat), names(histWeatherSubset)))


## adding variables for plotting
precipDat[,daysfromforecastLABEL := paste0(as.character(daysfromforecast), " Days")]
## cleaning out NAs
precipDat <- na.omit(precipDat)


## code for binning precipitaiton data
nHistBins <- 60
nForecastBins <- 3

## make sequence of breaks for historical and forecast values
histBreaks <- seq(min(precipDat[,PrecipitationIn]), max(precipDat[,PrecipitationIn]), length = nHistBins + 1)
histBreaks[nHistBins + 1] <- histBreaks[nHistBins + 1] + 1
forecastBreaks <- seq(min(precipDat[,value]), max(precipDat[,value]), length = nForecastBins + 1)
forecastBreaks[nForecastBins + 1] <- forecastBreaks[nForecastBins + 1] + 1

mround <- function(x,base){ 
  base*round(x/base) 
} 
################## fitted error ########################
## logistic regression ##
len=7
par(mfrow=c(1,len))
forecast_rain_new = list(0)
coefficient1 = NULL
miscrate = NULL
miscrate1 = NULL
miscrate2 = NULL
#length(unique(for.hist.loc.maxT$daysfromforecast))
for (i in 1:len){
  forecast_rain_0 = precipDat[precipDat$daysfromforecast == i-1,]
  #a1 = group_by(forecast_rain_0,forecastFOR,Airportcode) 
  #a2 = select(a1,value,precip,humid,wind,cloud,dew,vis,sealevel)
  #a2$value = as.numeric(a2$value)
  #a3 = summarise(a2,valuem = mean(value),precipm = mean(precip),humidm = mean(humid),windm = mean(wind),cloudm = mean(cloud),dewm = mean(dew),vism = mean(vis),sealevelm = mean(sealevel))
  #forecast_0 = as.data.frame(a3)[!is.na(a3$precipm) & !is.na(a3$humidm) & !is.na(a3$windm) & !is.na(a3$cloudm) & !is.na(a3$dewm) & !is.na(a3$vism) & !is.na(a3$sealevelm),]
  forecast_rain_0$precipnew = as.numeric(forecast_rain_0$PrecipitationIn>0)
  fit1 = glm(precipnew ~ value + Mean_Humidity + Mean_Wind_SpeedMPH + CloudCover + MeanDew_PointF + Mean_VisibilityMiles + Mean_Sea_Level_PressureIn, family = "binomial", data = forecast_rain_0)
  fit2 = glm(precipnew ~ value, family = "binomial", data = forecast_rain_0)
  miscrate2[i] = 1-mean((forecast_rain_0$value>0) == forecast_rain_0$precipnew)
  #coefficient1[i] = coef(fit1)[2]
  #plot(precipnew ~ valuem, data=forecast_0, col="red4",xlab="Probability of precipitation",ylab='Actual precipitation status',main=paste("Predicted ", i-1," days away", sep = ""))
  #newdat = data.frame(valuem=seq(min(forecast_0$valuem), max(forecast_0$valuem),len=100))
  #newdat$precipnew = predict(fit1, newdata=newdat, type="response")
  #lines(precipnew ~ valuem, newdat, col="green4", lwd=2)
  miscrate[i] = 1 - mean(forecast_rain_0$precipnew == (predict(fit1,type = "response")>0.5))
  miscrate1[i] = 1 - mean(forecast_rain_0$precipnew == (predict(fit2,type = "response")>0.5))
  # sub=clean.for.hist.loc.maxT[clean.for.hist.loc.maxT$daysfromforecast==unique(clean.for.hist.loc.maxT$daysfromforecast)[i],]
  # 
  # # plot(sub$cityID,sub$Max_TemperatureF,col='blue',ylim=c(-200,200),pch='.')
  # # lines(sub$cityID,sub$value,col='red',pch='.')
  # 
  # subsub=na.omit(sub)
  # plot(forecast_0$precipm,forecast_0$valuem,xlim=c(-1,3),ylim=c(-10,100),pch='.',type='n',xlab="Actual",ylab='Predicted',main=paste("Predicted ", i-1," days away", sep = ""))
  # points(forecast_0$precipm,forecast_0$valuem,pch='.')
  # abline(c(0,0),c(3,100),col='red')
  precipDat$updatepred[precipDat$daysfromforecast == i-1] = mround(predict(fit1,type = "response")*100,5)
}

##  END Prep for Precipitation Plots
##  GV 7/13/2018
####################################

save.image("prePreppedData.RData")



####################################
## Pre Make Precipitaiton Data Plots
## GV 9/6/2019
print("prepping precipitation plots")
plotLocations <- c("All", unique(precipDat[,AirPtCd]))

## Plots Types
## precipPredPlots <- predicted vs actual plots
## precipObsScatterPlots <- predicted probability vs. actual probability scatter plot
## newPrecipPredPlots <- updated prediction vs. actual plots
## newPrecipObsScatterPlots <- upated predicted probability vs. actual probability scatter plot
newPrecipObsScatterPlots <- newPrecipPredPlots <- precipObsScatterPlots <- precipPredPlots <- rep(list(NA), length(plotLocations))

names(newPrecipObsScatterPlots) <- names(newPrecipPredPlots) <- names(precipObsScatterPlots) <- names(precipPredPlots) <- plotLocations

for(plotLoc in plotLocations){
  print(plotLoc)
  
  ##################################################
  ## Data for predicted vs actual plots (precipPred)
  ## test == precipPredPlotDat()
  if(plotLoc == "All"){ #input$precipAirpt == "All Airports"){
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
  } else { 
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == plotLoc]
  }

  test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(value), forecastBreaks)]
  ## Label Bins
  test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]

  ## count number of observaitons
  boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
  test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
  test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]

  ## plots for predicted vs actual plots (precipPred)
  
  titleName <- paste0("Airport ", plotLoc)
  if(plotLoc == "All"){
    titleName <- "All Airports"
  }
  ## test == precipPredPlotDat()
  precipPredPlots[[plotLoc]] <- ggplot(test, aes(as.factor(boxplotLABEL), PrecipitationIn ))+
    geom_boxplot(aes(fill = N)) +
    facet_wrap(~daysfromforecastLABEL, nrow =1) + 
    scale_fill_gradient(low = "blue", high = "red", name = "Count") +
    labs(x = "Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  
  
  
  ##################################################
  ## Data for predicted probability vs. observed probability
  ## test == precipObsPlotDat()
  if(plotLoc == "All"){ #input$precipAirpt == "All Airports"){
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
  } else { 
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == plotLoc]
  }
  
  test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, value)]

  ## plots for predicted vs observed probability plots (precipObsScatter)
  
  titleName <- paste0("Airport ", plotLoc)
  if( plotLoc== "All"){
    titleName <- "All Airports"
  }
  ## test == precipObsPlotDat()
  precipObsScatterPlots[[plotLoc]] <- ggplot(test, aes(value, obsProb))+ 
    geom_point(aes(color = N)) + 
    facet_wrap(~daysfromforecastLABEL, nrow =1) +
    scale_colour_gradient(low = "blue", high = "red", name = "Count") +
    labs(x = "Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
    geom_abline(intercept = 0, slope =1/100) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
  
  
  
  #############################
  ## Improved predictions plots
  #############################  
  
  
  
  ##################################################
  ## Data for improved prediction boxplot
  ## test ==  precipPredPlotDatn()
  if(plotLoc == "All"){ #input$precipAirpt == "All Airports"){
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
  } else { 
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == plotLoc]
  }
  
  test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(updatepred), forecastBreaks)]
  ## Label Bins
  test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]
  
  ## count number of observaitons
  boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
  test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
  test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]
  
  ## plots for updated predicted vs actual plots (newPrecipPred)
  
  titleName <- paste0("Airport ", plotLoc)
  if(plotLoc == "All"){
    titleName <- "All Airports"
  }
  
  ## test ==  precipPredPlotDatn()
  newPrecipPredPlots[[plotLoc]] <- ggplot(test, aes(as.factor(boxplotLABEL), PrecipitationIn ))+
    geom_boxplot(aes(fill = N)) +
    facet_wrap(~daysfromforecastLABEL, nrow =1) + 
    scale_fill_gradient(low = "blue", high = "red", name = "Count") +
    labs(x = "Improved Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  
  
  
  ##################################################
  ## Data for updated predicted probability vs. observed probability
  ## test == precipObsPlotDatn()
  if(plotLoc == "All"){ #input$precipAirpt == "All Airports"){
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
  } else { 
    test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == plotLoc]
  }
  
  test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, updatepred)]
  
  
  ## plots for updated predicted vs observed probability plots (precipObsScatter)
  
  titleName <- paste0("Airport ", plotLoc)
  if( plotLoc== "All"){
    titleName <- "All Airports"
  }
  ## test == precipObsPlotDatn()
  newPrecipObsScatterPlots[[plotLoc]] <- ggplot(test, aes(updatepred, obsProb))+ 
    geom_point(aes(color = N)) + 
    facet_wrap(~daysfromforecastLABEL, nrow =1) +
    scale_colour_gradient(low = "blue", high = "red", name = "Count") +
    labs(x = "improved Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
    geom_abline(intercept = 0, slope =1/100) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
  
  
    
}


## END pre Make Precipitation Data Plots
## GV 9/6/2019
#########################################

save.image("preppedAppData.RData")