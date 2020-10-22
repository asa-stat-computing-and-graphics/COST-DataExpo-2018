library(shiny)
library(ggplot2)
library(data.table)
options(shiny.maxRequestSize=30*1024^2) 
function(input, output, session){
  #####################################
  #XL code: for the map
  
  # get measurements --------------------------------------------------------
  
  item = function(measure, dat){
    if(measure == "High Temperature (F)") res = dat$Max_TemperatureF
    else if(measure == "Low Temperature (F)") res = dat$Min_TemperatureF
    else if(measure == "Precipitation (in)") res = dat$Precipitation
    else if(measure == "Mean Humidity") res = dat$Mean_Humidity
    else if(measure == "Mean Visibility (mi)") res = dat$Mean_VisibilityMiles
    else if(measure == "Mean Wind Speed (mph)") res = dat$Mean_Wind_SpeedMPH
    return(res)
  }
  
  # drop-down menu dependency -----------------------------------------------
  subhist = reactive({
    res = item(input$measure, histdata)
    
    sub = data.frame(histdata[!is.na(res),c('Date','date2day','AirPtCd')],res = res[!is.na(res)])
    colnames(sub) = c("Date", "date2day","AirPtCd","res")
    return(sub)
  })
  
  observeEvent({
    input$measure
  },{
    res = item(input$measure, histdata)
    updateSelectInput(
      session = session,
      inputId = "mydate",
      choices = unique(histdata[!is.na(res), 'Date'])
    )
  })
  
  
  output$dateSelection <- renderUI({
    
    selectInput("mydate", label = "Pick a date", choices = NULL, selected = '07/09/2014')
  })
  
  observeEvent({
    input$mydate
  },{
    airptcd = unique(subhist()[subhist()$Date == input$mydate,"AirPtCd"])
    airptcd = airptcd[order(airptcd)]
    updateSelectInput(
      session = session,
      inputId = "Airpt",
      choices = airptcd,
      selected = "KBHB"
    )
  })
  
  
  output$airptSelection <- renderUI({
    selectInput("Airpt", "Pick an airport", choices = NULL, selected = "KBHB")
  })
  
  observeEvent({
    input$Airpt
  },{
    updateSelectInput(
      session = session,
      inputId = "foredate",
      choices = as.list(fore_cast$today[fore_cast$locID %in% aircode$airptid[aircode$airptcd == input$Airpt]])
    )
  })
  
  
  output$foreSelection <- renderUI({
    #selectInput("foredate", "Pick a forecast date", choices = NULL, selected = as.character(input$mydate))
    selectInput("foredate", "Pick a forecast date", choices = NULL, selected = "07/09/2014")
  })
  
  
  # display the map -------------------------------------------------------------
  
  dat = reactive({
    dat = unique(subhist()[subhist()$Date == input$mydate,])
    
    return(dat)
  })
  
  
  output$map <- renderLeaflet({
    subloc = loc[loc$AirPtCd %in% dat()$AirPtCd, ]
    subloc = subloc[order(subloc$AirPtCd), ]
    dat = dat()[order(dat()$AirPtCd), ]
    
    newdat = data.frame(longitude = subloc$longitude, latitude = subloc$latitude, airport = dat$AirPtCd, res = dat$res)
    
    nrc = 15
    rc <- colorRampPalette(colors = c("red","yellow"), space = "Lab")(nrc)
    mycol <- colorNumeric(palette = rc, domain = dat$res, reverse = T)
    
    
    leaflet(data = newdat) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = 3.5,
                       color = mycol(newdat$res),
                       stroke=FALSE,
                       fillOpacity = 0.6
                       
      )%>%
      addLegend(position = "topleft", pal = mycol, values = newdat$res,
                title = input$measure,
                opacity = 1)%>%
      setView(lng = -100.85, lat = 37.45, zoom = 3)
  })
  
  # mouse hover popup -------------------------------------------------------
  
  
  showpopup = function(lon, lat, date){
    sub = loc[loc$longitude == lon & loc$latitude == lat,]
    idx = which(histdata$Date == date & histdata$AirPtCd == sub$AirPtCd)
    
    res = item(input$measure, histdata)[idx]
    
    if (is.na(res)) res = NA
    
    text = paste0("<strong>Location: </strong>",
                  sub$city, ",", sub$state, 
                  "<br><strong>Airport: </strong>",
                  sub$AirPtCd,
                  "<br><strong>",input$measure,":", "</strong>", res)
    leafletProxy("map") %>% addPopups(lng = lon, lat = lat, text)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event = input$map_marker_mouseover
    if (is.null(event)) return()
    isolate({showpopup(event$lng, event$lat, input$mydate)})
  })
  
  
  # highlight a location upon selection -------------------------------------
  
  selectedState <- reactive({
    loc[loc$AirPtCd == input$Airpt,]  
  })
  
  #dat = reactiveValues(selectedMarker = NULL)
  
  observeEvent(input$Airpt,{
    # nrc = 15
    # rc <- colorRampPalette(colors = c("red","yellow"), space = "Lab")(nrc)
    # mycol <- colorNumeric(palette = rc, domain = dat()$res, reverse = T)
    # newdat = data.frame(longitude = subloc$longitude, latitude = subloc$latitude, airport = dat()$AirPtCd, res = dat()$res)
    
    leafletProxy("map") %>%
      # clearMarkers() %>%
      # clearShapes() %>%
      clearGroup(group = "bluecircle") %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircleMarkers(data = selectedState(),lng = ~longitude,
                       lat = ~latitude, group = "bluecircle") 
  })
  
  
  
  
  # create tables and plots -------------------------------------------------
  
  
  
  
  
  
  ##fore_cast table
  
  output$fore = renderTable({
    subfore = fore_cast[fore_cast$today == input$foredate & fore_cast$locID %in% aircode$airptid[aircode$airptcd == input$Airpt], ]
    subfore = aggregate(as.numeric(subfore$res), list(subfore$date,subfore$stat),mean)
    colnames(subfore) = c("date", "stat", "res")
    today = as.numeric(ymd(input$foredate)) - 16251
    subfore$date2day = as.numeric(ymd(subfore$date)) - 16251
    minmax = min(aggregate(subfore$date2day, list(subfore$stat), max)[,2]) - today
    if(minmax <= 5) subfore1 = subfore[subfore$date2day >= today & subfore$date2day <= (minmax + today), ] else{ 
      subfore1 = unique(subfore[subfore$date2day >= today & subfore$date2day<= (5 + today),  ])}
    
    subset = reshape(subfore1[,c("date","stat","res")], timevar = "stat", idvar = "date", direction = 'wide')
    colnames(subset) = c("Date      ", "High", "Low", "Chance of rain")
    return(subset)
  })
  
  
  
  data =reactive({
    data = histdata[histdata$AirPtCd == input$Airpt & histdata$Date == input$mydate, ]
    if(data$date2day <= 30) dat = histdata[histdata$AirPtCd == input$Airpt & 
                                             histdata$date2day >= data$date2day & 
                                             histdata$date2day <= (data$date2day + 29), ]
    else dat = histdata[histdata$AirPtCd == input$Airpt &  
                          histdata$date2day >= 1 & 
                          histdata$date2day <= data$date2day, ]
    return(dat)
  })
  
  # Min Max Temp error plots ------------------------------------------------
  output$max_error = renderPlotly({
    #boxplot for MaxTemp
    if(input$num == "Max_TemperatureF"){
      error_max$class1 <- rep(1,dim(error_max)[1])
      error_max_reduce$class1 <- rep(2,dim(error_max_reduce)[1])
      sum_max <- rbind(error_max,error_max_reduce)
      final <- data.frame("value"=c(sum_max$RMSE,sum_max$MAE,sum_max$MAPE,sum_max$MASE),
                       "error.measure"=c(rep("RMSE",216),
                                         rep("MAE",216), 
                                         rep("MAPE",216),
                                         rep("MASE",216)),
                       "class1"=rep(c(rep("Updated",108),rep("Original",108)),4))
      
      df <- melt(final,id.vars=c("value","error.measure","class1"))
      p <- ggplot(df, aes(error.measure, value, fill = class1))
      p <- p + geom_boxplot() + 
        facet_grid(. ~ class1) +
        labs(fill = "Model") +
        scale_x_discrete(name = "Error Measures") +
        scale_y_continuous(name = "Error") +
        ggtitle("Summary of Errors: Maximum Temperature")
      p
    }
    else if(input$num == "Min_TemperatureF"){
    #boxplot for MinTemp
    error_min$class1 <- rep(1,dim(error_min)[1])
    error_min_reduce$class1 <- rep(2,dim(error_min_reduce)[1])
    sum_min <- rbind(error_min,error_min_reduce)
  
    final1 <- data.frame("value"=c(sum_min$RMSE,sum_min$MAE,sum_min$MAPE,sum_min$MASE),
                      "error.measure"=c(rep("RMSE",216),
                                        rep("MAE",216),
                                        rep("MAPE",216),
                                        rep("MASE",216)),
                      "class1"=rep(c(rep("Updated",108),rep("Original",108)),4))
    df1 <- melt(final1,id.vars=c("value","error.measure","class1"))
    p <- ggplot(df1, aes(x= error.measure, y = value, fill = class1)) +
      geom_boxplot() + 
      facet_grid(. ~ class1) + 
      labs(fill = "Model") +
      scale_x_discrete(name = "Error Measures") +
      scale_y_continuous(name = "Error") +
      ggtitle("Summary of Errors: Minimum Temperature") 
    p
    }
    ggplotly(p)
  })
  
  # output$min_error = renderPlotly({
  #   plot = ggplot(data_min,aes(x=error, fill=Model)) + geom_density(alpha=0.25) + 
  #     ggtitle("Distribution of Mean Absolute Error\n for Min. Temperature") 
  #   ggplotly(plot)
  # })
  # 
  
  output$max_temp_plot = renderPlotly({  
    dat = data()[!is.na(data()$Max_TemperatureF), ]
    plot = ggplot(dat, aes(date2day, Max_TemperatureF))+ geom_line(color = 'blue') +
      labs(x = "Days", y = 'High Temperature (F)')+theme(text = element_text(size=9))
    ggplotly(plot)
    
  })
  
  output$min_temp_plot = renderPlotly({
    dat = data()[!is.na(data()$Min_TemperatureF), ]
    plot = ggplot(dat, aes(date2day, Min_TemperatureF))+  geom_line(color = 'blue') +
      labs(x = "Days", y = 'Low Temperature (F)')+theme(text = element_text(size=9))
    ggplotly(plot)
  })
  output$preciplot = renderPlotly({
    dat = data()[!is.na(data()$Precipitation), ]
    plot =  ggplot(dat, aes(date2day, Precipitation))+ geom_line(color = 'blue') +
      labs(x = "Days", y = 'Precipitation (in)'
           #+scale_x_continuous(breaks=seq(min(dat$PrecipitationIn), max(dat$PrecipitationIn), length.out = 10))
      )+theme(text = element_text(size=9))
    ggplotly(plot)
  })
  output$humidplot = renderPlotly({
    dat = data()[!is.na(data()$Mean_Humidity), ]
    ggplot(dat, aes(date2day, Mean_Humidity))+
      geom_line(color = 'blue') +
      labs(x = "Days", y = 'Mean Humidity')+theme(text = element_text(size=9))
  })
  output$visplot = renderPlotly({
    dat = data()[!is.na(data()$Mean_VisibilityMiles), ]
    plot = ggplot(dat, aes(date2day, Mean_VisibilityMiles))+
      geom_line(color = 'blue') +
      labs(x = "Days", y = 'Mean Visibility (mi)')+theme(text = element_text(size=9))
    ggplotly(plot)
  })
  output$windplot = renderPlotly({
    dat = data()[!is.na(data()$Mean_Wind_SpeedMPH), ]
    plot = ggplot(dat, aes(date2day, Mean_Wind_SpeedMPH))+
      geom_line(color = 'blue') +
      labs(x = "Days", y = 'Mean Wind Speed (mph)')+theme(text = element_text(size=9))
    ggplotly(plot)
  })
  
  
  
  
  output$theLP <- renderPlot({
    sub_data <- histdata[which(histdata$AirPtCd == input$airpt),]
    plot(seq_along(sub_data[,1]),sub_data[,input$num], type="l",
         col = plotColor, main = "Plot over Time", xlab="Days", ylab = paste("",input$num,"", sep=""))
  })
  output$theHist <- renderPlot({
    sub_data <- histdata[which(histdata$AirPtCd == input$airpt),]
    hist(sub_data[,input$num], col = plotColor, main=paste("Histogram of ",input$num,"", sep=""), xlab = "")
    if(input$cbox) abline(v=median(sub_data[,input$num]), col='white')
  })
  output$theLP1 <- renderPlot({
    sub_data1 <- histdata[which(histdata$AirPtCd == input$airpt1),]
    plot(seq_along(sub_data1[,1]),sub_data1[,input$num], type="l",
         col = plotColor, main = "Plot over Time", xlab="Days", ylab = paste("",input$num,"", sep=""))
  })
  output$theHist1 <- renderPlot({
    sub_data1 <- histdata[which(histdata$AirPtCd == input$airpt1),]
    hist(sub_data1[,input$num], col = plotColor, main=paste("Histogram of ",input$num,"", sep=""), xlab = "")
    if(input$cbox) abline(v=median(sub_data1[,input$num]), col='white')
  })
  output$thebubble1 <- renderPlot({
  
    if(input$num=="Max_TemperatureF"){
      for.maxT=forecast[which(forecast$variable=="MaxTemp"),]
      hist.loc.maxT=hist.loc[,c("Max_TemperatureF","AirPtCd","cityID","forecastFOR")]
      for.hist.loc.maxT=merge(x=for.maxT,y=hist.loc.maxT,by=c("cityID","forecastFOR"),all.x=T)
    }else{
      for.maxT=forecast[which(forecast$variable=="MinTemp"),]
      hist.loc.maxT=hist.loc[,c("Min_TemperatureF","AirPtCd","cityID","forecastFOR")]
      for.hist.loc.maxT=merge(x=for.maxT,y=hist.loc.maxT,by=c("cityID","forecastFOR"),all.x=T)
    }

    NA.rows=which(is.na(for.hist.loc.maxT[,input$num]))
    M.rows=which(for.hist.loc.maxT$value=='M')
    clean.for.hist.loc.maxT=for.hist.loc.maxT[-union(NA.rows, M.rows),]
    len=7
    par(mfrow=c(1,len))
    sub_data1 <- clean.for.hist.loc.maxT[which(clean.for.hist.loc.maxT$AirPtCd == input$airpt),]
    for (i in 1:len){
      sub=sub_data1[sub_data1$daysfromforecast==unique(sub_data1$daysfromforecast)[i],]
      subsub=na.omit(sub)
      plot(subsub[,input$num],subsub$value,xlim=c(-50,110),ylim=c(-50,110),pch='.',type='n',xlab="Actual",ylab='Predicted',main=paste("Predicted ", i-1," days away", sep = ""))
      points(subsub[,input$num],subsub$value,pch='.')
      abline(c(0,0),c(1,1),col='red')
    }
  })


################################
## Temperature Panel Plots
## GV 7/23/2018

  ## code for temperature summary plots

  tempPlotDat <- reactive({
    varType <- "MinTemp"
    varName <- "Min Temp"
    if(input$num == "Max_TemperatureF"){
       varType <- "MaxTemp"  
       varName <- "Max Temp"
    } 
    if(input$airpt == "All Airports"){
       #test <- tempDat[daysfromforecast ==0 & variable == varType]
       test <- tempDat[daysfromforecast ==0 & variable == varType]#added edit
       test <- test[,.(value = mean(value)), by = .(forecastFOR)] #added edit
    } else { 
       test <- tempDat[daysfromforecast ==0 & AirPtCd == input$airpt & variable == varType]
    }
    test[,days := 1:nrow(test)]
    test[,varName:= varName]
    test
  })

  tempPlotDat1 <- reactive({
    varType <- "MinTemp"
    varName <- "Min Temp"
    if(input$num == "Max_TemperatureF"){
       varType <- "MaxTemp"  
       varName <- "Max Temp"
    } 
    if(input$airpt1 == "All Airports"){
       test <- tempDat[daysfromforecast ==0 & variable == varType]
       test <- test[,.(value = mean(value)), by = .(forecastFOR)] #added edit
    } else { 
       test <- tempDat[daysfromforecast ==0 & AirPtCd == input$airpt1 & variable == varType]
    }    
    test[,days := 1:nrow(test)]
    test[,varName:= varName]
    test
  })

  ## line plot of Temperatures
  output$tempLine <- renderPlotly({
     DT <- tempPlotDat()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt)
     if( input$airpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(days, value))+
            geom_line(color = plotColor) +
            labs(x = "Days", y = varName, title = titleName) +
            ylim(c(-40,120))
     ggplotly(p)
  })

  output$tempLine1 <- renderPlotly({
     DT <- tempPlotDat1()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt1)
     if( input$airpt1== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(days, value))+
            geom_line(color = plotColor) +
            labs(x = "Days", y = varName, title = titleName)  +
       ylim(c(-40,120))
     ggplotly(p)
  })

  ## histograms of Temperatures
  output$tempHist <- renderPlotly({
     DT <- tempPlotDat()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt)
     if( input$airpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(value))+
            geom_histogram(color = plotColor, fill = plotColor) +
            labs(x = varName, y = "Frequency", title = titleName) +
            ylim(c(0,100))
     ggplotly(p)
  })

  output$tempHist1 <- renderPlotly({
     DT <- tempPlotDat1()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt1)
     if( input$airpt1== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(value))+
            geom_histogram(color = plotColor, fill = plotColor) +
            labs(x = varName, y = "Frequency", title = titleName) +
       ylim(c(0,100))
     ggplotly(p)
  })

#########################################
# Calendar heat map 
  tempchmdat <- reactive({
  if(input$airpt == "All Airports"){
    df1 <- histWeather
    df1 <- df1 %>% dplyr::select(Date, Max_TemperatureF, Min_TemperatureF)
    df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::summarise(Max_TemperatureF = mean(Max_TemperatureF), Min_TemperatureF = mean(Min_TemperatureF))
    df1$date <- as.Date(df1$Date)
  }
    else{
      df1 <- histWeather %>% filter(AirPtCd == input$airpt)
      df1$date <- as.Date(df1$Date)  # format date
    }
  
  
  # Create Month Week
  df1$yearmonth <- as.yearmon(df1$date)
  df1$yearmonthf <- factor(df1$yearmonth)
  df1$year <- year(df1$date)
  df1$week <- week(df1$date)
  df1$month <- month(df1$date)
  df1$monthf <- factor(df1$month)
  df1$weekday <- weekdays(df1$date)
  df1$weekdayf <- factor(df1$weekday)
  df1 <- ddply(df1,.(yearmonthf), transform, monthweek=1+week-min(week))
  })# compute week number of month
##########################################
output$tempchm1 <- renderPlotly({
  df1 <- tempchmdat()
  df_max <- df1[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "Max_TemperatureF","Min_TemperatureF")]
  if(input$num == "Max_TemperatureF")
    titleN <- paste0("Maximum Temperature")
  else if(input$num == "Min_TemperatureF")
    titleN <- paste0("Minimum Temperature")
  
  if(input$num == "Max_TemperatureF")
    fillN <- paste0("High")
  else if(input$num == "Min_TemperatureF")
    fillN <- paste0("Low")
  
  if(input$num == "Max_TemperatureF")
    fillV <- "Max_TemperatureF"
  else if(input$num == "Min_TemperatureF")
    fillV <- "Min_TemperatureF"
  
  highplot <- ggplot(df_max, aes(monthweek, weekdayf, fill = eval(parse(text = fillV)))) + 
    geom_tile(colour = "white") + 
    facet_grid(year~monthf) + 
    scale_fill_gradient(low="blue", high="red") +
    labs(x="Week of Month",
         y="",
         title = titleN, 
         fill=fillN)
  ggplotly(highplot)
  })
####################################  
  # Calendar heat map for airport 2
  tempchmdat1 <- reactive({
    if(input$airpt1 == "All Airports"){
      df2 <- histWeather
      df2 <- df2 %>% dplyr::select(Date, Max_TemperatureF, Min_TemperatureF)
      df2 <- df2 %>% dplyr::group_by(Date) %>% dplyr::summarise(Max_TemperatureF = mean(Max_TemperatureF), Min_TemperatureF = mean(Min_TemperatureF))
      df2 <- df2 %>% dplyr::mutate(date=as.Date(Date))
    }
    else{
      df2 <- histWeather %>% filter(AirPtCd == input$airpt1)
      df2$date <- as.Date(df2$Date)  # format date
    }
  

  # Create Month Week
  df2$yearmonth <- as.yearmon(df2$date)
  df2$yearmonthf <- factor(df2$yearmonth)
  df2$year <- year(df2$date)
  df2$week <- week(df2$date)
  df2$month <- month(df2$date)
  df2$monthf <- factor(df2$month)
  df2$weekday <- weekdays(df2$date)
  df2$weekdayf <- factor(df2$weekday)
  df2 <- ddply(df2,.(yearmonthf), transform, monthweek=1+week-min(week))
  })
  ##########################################
  output$tempchm2 <- renderPlotly({
    df2 <- tempchmdat1()
    df_max <- df2[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "Max_TemperatureF","Min_TemperatureF")]
    if(input$num == "Max_TemperatureF")
      titleN <- paste0("Maximum Temperature")
    else if(input$num == "Min_TemperatureF")
      titleN <- paste0("Minimum Temperature")
    
    if(input$num == "Max_TemperatureF")
      fillN <- paste0("High")
    else if(input$num == "Min_TemperatureF")
      fillN <- paste0("Low")
    
    if(input$num == "Max_TemperatureF")
      fillV <- "Max_TemperatureF"
    else if(input$num == "Min_TemperatureF")
      fillV <- "Min_TemperatureF"
    
    highplot <- ggplot(df_max, aes(monthweek, weekdayf, fill = eval(parse(text = fillV)))) + 
      geom_tile(colour = "white") + 
      facet_grid(year~monthf) + 
      scale_fill_gradient(low="blue", high="red") +
      labs(x="Week of Month",
           y="",
           title = titleN, 
           fill=fillN)
    ggplotly(highplot)
  })
  ####################################
########################################  
  ## code for predicted vs actual plots

  ## making specific dataset by airport 
  tempPredPlotDat <- reactive({

    varType <- "MinTemp"
    varName <- "Min Temp"
    if(input$num == "Max_TemperatureF"){
       varType <- "MaxTemp"  
       varName <- "Max Temp"
    } 
    if(input$airpt == "All Airports"){
       test <- tempDat[daysfromforecast<=6 & daysfromforecast >=0 & variable == varType]
    } else { 
       test <- tempDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$airpt & variable == varType]
    }
 

    histBreaks <- seq(min(test$histValue), max(test$histValue), length = nHistTempBins)
    forecastBreaks <- seq(min(test$value), max(test$value), length = nForecastTempBins)

    test <- test[,c("forecastBinLABEL"):=findInterval(value, forecastBreaks)]
    test <- test[,c("histBinLABEL"):=findInterval(histValue, histBreaks)]

    ## Label Bins
    test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastTempBins)])/2)[test$forecastBinLABEL]
    test$histBin <- ((histBreaks[-1] + histBreaks[-(nHistTempBins)])/2)[test$histBinLABEL]

    ## count number of observaitons
    test <- test[,.(N =.N, varName = varName),by = .(forecastBin,histBin, daysfromforecast)]
    test$daysfromforecastLABEL <- paste0(test$daysfromforecast, " Days")

    test
  })

  tempPredPlotDat1 <- reactive({

    varType <- "MinTemp"
    varName <- "Min Temp"
    if(input$num == "Max_TemperatureF"){
       varType <- "MaxTemp"  
       varName <- "Max Temp"
    } 
    if(input$airpt1 == "All Airports"){
       test <- tempDat[daysfromforecast<=6 & daysfromforecast >=0 & variable == varType]
    } else { 
       test <- tempDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$airpt1 & variable == varType]
    }

    histBreaks <- seq(min(test$histValue), max(test$histValue), length = nHistTempBins)
    forecastBreaks <- seq(min(test$value), max(test$value), length = nForecastTempBins)

    test <- test[,c("forecastBinLABEL"):=findInterval(value, forecastBreaks)]
    test <- test[,c("histBinLABEL"):=findInterval(histValue, histBreaks)]

    ## Label Bins
    test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastTempBins)])/2)[test$forecastBinLABEL]
    test$histBin <- ((histBreaks[-1] + histBreaks[-(nHistTempBins)])/2)[test$histBinLABEL]

    ## count number of observaitons
    test <- test[,.(N =.N, varName = varName),by = .(forecastBin,histBin, daysfromforecast)]
    test$daysfromforecastLABEL <- paste0(test$daysfromforecast, " Days")

    test
  })



  ## bubbleplot of predicted vs. actual values
  output$tempPredBubble <- renderPlot({
     DT <- tempPredPlotDat()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt)
     if( input$airpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(histBin, forecastBin))+ 
     geom_point(aes( size = N, color = N)) + 
     facet_wrap(~daysfromforecastLABEL, nrow =1) + 
     scale_colour_gradient(low = "blue", high = "red", name = "Count") +
     scale_size(name = "Count") +
     labs( x = paste0("Actual ", varName), y = paste0("Predicted ", varName),  title = titleName) +
     theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  +
     geom_abline(intercept = 1, slope =1)

     p
  })

  output$tempPredBubble1 <- renderPlot({
     DT <- tempPredPlotDat1()
     varName <- DT[1,varName]
     titleName <- paste0("Airport ", input$airpt1)
     if( input$airpt1== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(DT, aes(histBin, forecastBin))+ 
     geom_point(aes( size = N, color = N)) + 
     facet_wrap(~daysfromforecastLABEL, nrow =1) + 
     scale_colour_gradient(low = "blue", high = "red", name = "Count") +
     scale_size(name = "Count") +
     labs( x = paste0("Actual ", varName), y = paste0("Predicted ", varName),  title = titleName) + 
     theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  +
     geom_abline(intercept = 1, slope =1)
     p
  })
################################
## Precipitaiton panel plots
## GV 7/13/2018

  ## Precipitaiton summary Plots

  ## making specific dataset by airport
  precipPlotDat <- reactive({
    if(input$precipAirpt == "All Airports"){
       test1 <- precipDat[daysfromforecast ==0]
       test1[complete.cases(test1),]
       test1 <- test1 %>% dplyr::group_by(forecastFOR) %>% dplyr::summarise(PrecipitationIn = mean(PrecipitationIn))
       #test <- test[,.(value = mean(value,na.rm=TRUE)), by = .(forecastFOR)]#added edit
    } else { 
       test1 <- precipDat[daysfromforecast ==0 & AirPtCd == input$precipAirpt]
    }
    test1 <- as.data.table(test1)
    test1[,days := 1:nrow(test1)]
    test1
  })

  precipPlotDat1 <- reactive({
    if(input$precipAirpt1 == "All Airports"){
       test1 <- precipDat[daysfromforecast ==0]
       test1[complete.cases(test1),]
       test1 <- test1 %>% dplyr::group_by(forecastFOR) %>% dplyr::summarise(PrecipitationIn = mean(PrecipitationIn))
       #test <- test[,.(value = mean(valuena.rm=TRUE)), by = .(forecastFOR)]#added edit
    } else { 
       test1 <- precipDat[daysfromforecast ==0 & AirPtCd == input$precipAirpt1]
    }
    test1 <- as.data.table(test1)
    test1[,days := 1:nrow(test)]
    test1
  })

  ## scatter plots of precipitaiton over time
  output$precipScatter <- renderPlotly({
     titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipPlotDat(), aes(days, PrecipitationIn))+
            geom_point(color = plotColor) +
            labs(x = "Days", y = "Precipitation (inches)", title = titleName) 
     ggplotly(p)
  })

  output$precipScatter1 <- renderPlotly({
     titleName <- paste0("Airport ", input$precipAirpt1)
     if( input$precipAirpt1== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipPlotDat1(), aes(days, PrecipitationIn))+ 
            geom_point(color = plotColor) + 
            labs(x = "Days", y = "Precipitation (inches)", title = titleName) 
     ggplotly(p)
  })

  ## histograms of precipitaiton amounts
  output$precipHist <- renderPlotly({
     titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipPlotDat(), aes(PrecipitationIn))+
            geom_histogram(color = plotColor, fill = plotColor) +
            labs(x = "Precipitation (inches)", y = "Frequency", title = titleName) 
      ggplotly(p)
     })

  output$precipHist1 <- renderPlotly({
     titleName <- paste0("Airport ", input$precipAirpt1)
     if( input$precipAirpt1== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipPlotDat1(), aes(PrecipitationIn))+ 
            geom_histogram(color = plotColor, fill = plotColor) + 
           labs(x = "Precipitation (inches)", y = "Frequency", title = titleName)  
     ggplotly(p)
 })


  ## code for predicted vs actual plots

  ## making specific dataset by airport 
  precipPredPlotDat <- reactive({
    if(input$precipAirpt == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt]
    }

    test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(value), forecastBreaks)]
    ## Label Bins
    test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]

    ## count number of observaitons
    boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
    test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
    test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]

    test
  })

  precipPredPlotDat1 <- reactive({
    if(input$precipAirpt1 == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt1]
    }

    test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(value), forecastBreaks)]
    ## Label Bins
    test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]

    ## count number of observaitons
    boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
    test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
    test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]

    test
  })

  output$precipBox <- renderPlot({
     titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipPredPlotDat(), aes(as.factor(boxplotLABEL), PrecipitationIn ))+
           geom_boxplot(aes(fill = N)) +
           facet_wrap(~daysfromforecastLABEL, nrow =1) + 
           scale_fill_gradient(low = "blue", high = "red", name = "Count") +
           labs(x = "Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
           theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  
    p
  })

 output$precipBox1 <- renderCachedPlot({
   plotLoc <- input$precipAirpt1
   if( input$precipAirpt1== "All Airports"){
     plotLoc <- "All"
   }
   precipPredPlots[[plotLoc]]
   
     # titleName <- paste0("Airport ", input$precipAirpt1)
     # if( input$precipAirpt1== "All Airports"){
     #   titleName <- "All Airports"
     # }
     # p <- ggplot(precipPredPlotDat1(), aes(as.factor(boxplotLABEL), PrecipitationIn ))+
     #       geom_boxplot(aes( fill = N)) +
     #       facet_wrap(~daysfromforecastLABEL, nrow =1) + 
     #       scale_fill_gradient(low = "blue", high = "red", name = "Count") +
     #       labs(x = "Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
     #       theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  
     # p
 },
 cacheKeyExpr = { input$precipAirpt1 }
 )

 
## improved prediction boxplot 
 
 ## making specific dataset by airport 
 precipPredPlotDatn <- reactive({
    if(input$precipAirpt == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt]
    }

   test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(updatepred), forecastBreaks)]
   ## Label Bins
   test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]
   
   ## count number of observaitons
   boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
   test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
   test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]
   
   test
 })
 
 precipPredPlotDat1n <- reactive({
    if(input$precipAirpt1 == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt1]
    }

   test <- test[,c("forecastBinLABEL"):=findInterval(as.numeric(updatepred), forecastBreaks)]
   ## Label Bins
   test$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins+1)])/2)[precipDat$forecastBinLABEL]
   
   ## count number of observaitons
   boxplotCounts <- test[,.N,by = .(forecastBin, daysfromforecast)]
   test <- merge(test, boxplotCounts, by = intersect(names(test), names(boxplotCounts)), all =TRUE)
   test[,c("boxplotLABEL") := paste0(round((forecastBinLABEL-1)*100/nForecastBins, digits = 2), "%-", round((forecastBinLABEL)*100/nForecastBins, digits = 2), "%")]
   
   test
 })
 
 output$precipBoxn <- renderPlot({
    titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
 
   p <- ggplot(precipPredPlotDatn(), aes(as.factor(boxplotLABEL), PrecipitationIn ))+
     geom_boxplot(aes(fill = N)) +
     facet_wrap(~daysfromforecastLABEL, nrow =1) + 
     scale_fill_gradient(low = "blue", high = "red", name = "Count") +
     labs(x = "Improved Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
     theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  
   p
   
 })
 
 output$precipBox1n <- renderCachedPlot({
   plotLoc <- input$precipAirpt1
   if( input$precipAirpt1== "All Airports"){
     plotLoc <- "All"
   }
   newPrecipPredPlots[[plotLoc]]
   
   
    # titleName <- paste0("Airport ", input$precipAirpt1)
    #  if( input$precipAirpt1== "All Airports"){
    #    titleName <- "All Airports"
    #  }
    # p <- ggplot(precipPredPlotDat1n(), aes(as.factor(boxplotLABEL), PrecipitationIn ))+
    #  geom_boxplot(aes( fill = N)) +
    #  facet_wrap(~daysfromforecastLABEL, nrow =1) + 
    #  scale_fill_gradient(low = "blue", high = "red", name = "Count") +
    #  labs(x = "Improved Probability of Precipitation", y = "Observed Precipitaiton (inches)", title = titleName ) +
    #  theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
    # p
 },
 cacheKeyExpr = { input$precipAirpt1 }
 )
 

 
 
 
 
 
  ## predited probability vs. observed probability

  ## making specific dataset by airport 
  precipObsPlotDat <- reactive({
    if(input$precipAirpt == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt]
    }

    test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, value)]
    test
  })

  precipObsPlotDat1 <- reactive({
    if(input$precipAirpt1 == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt1]
    }


    test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, value)]
    test
  })


  ## Making predited probability vs. observed probability plots

  output$precipObsScatter <- renderPlot({
    titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
      p <- ggplot(precipObsPlotDat(), aes(value, obsProb))+ 
            geom_point(aes(color = N)) + 
            facet_wrap(~daysfromforecastLABEL, nrow =1) +
            scale_colour_gradient(low = "blue", high = "red", name = "Count") +
            labs(x = "Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
            geom_abline(intercept = 0, slope =1/100) +
            theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
      p
  })

  output$precipObsScatter1 <- renderCachedPlot({
    plotLoc <- input$precipAirpt1
    if( input$precipAirpt1== "All Airports"){
      plotLoc <- "All"
    }
    precipObsScatterPlots[[plotLoc]]
    
     # titleName <- paste0("Airport ", input$precipAirpt1)
     # if( input$precipAirpt1== "All Airports"){
     #   titleName <- "All Airports"
     # }
     # p <- ggplot(precipObsPlotDat1(), aes(value, obsProb))+ 
     #        geom_point(aes(color = N)) + 
     #        facet_wrap(~daysfromforecastLABEL, nrow =1) +
     #        scale_colour_gradient(low = "blue", high = "red", name = "Count") +
     #        labs(x = "Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
     #        geom_abline(intercept = 0, slope =1/100) +
     #         theme(axis.text.x = element_text(angle = -45, hjust = 0.05))
     # p
  },
  cacheKeyExpr = { input$precipAirpt1 }
  )

  
  
  
  ## predited probability vs. observed probability after improvement
  
  ## making specific dataset by airport 
  precipObsPlotDatn <- reactive({
    if(input$precipAirpt == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt]
    }

    test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, updatepred)]
    test
  })
  
  precipObsPlotDat1n <- reactive({
    if(input$precipAirpt1 == "All Airports"){
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0]
    } else { 
       test <- precipDat[daysfromforecast<=6 & daysfromforecast >=0 & AirPtCd == input$precipAirpt1]
    }

    test <- test[,.(obsProb = sum(PrecipitationIn >0)/length(PrecipitationIn), N = .N),by = .(daysfromforecast,daysfromforecastLABEL, updatepred)]
    test
  })
  
  
  ## Making predited probability vs. observed probability plots
  
  output$precipObsScattern <- renderPlot({
    titleName <- paste0("Airport ", input$precipAirpt)
     if( input$precipAirpt== "All Airports"){
       titleName <- "All Airports"
     }
     p <- ggplot(precipObsPlotDatn(), aes(updatepred, obsProb))+ 
      geom_point(aes(color = N)) + 
      facet_wrap(~daysfromforecastLABEL, nrow =1) +
      scale_colour_gradient(low = "blue", high = "red", name = "Count") +
      labs(x = "improved Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
      geom_abline(intercept = 0, slope =1/100) +
      theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
     p
  })
  
  output$precipObsScatter1n <- renderCachedPlot({
    
    plotLoc <- input$precipAirpt1
    if( input$precipAirpt1== "All Airports"){
      plotLoc <- "All"
    }
    newPrecipObsScatterPlots[[plotLoc]]
    
    # titleName <- paste0("Airport ", input$precipAirpt1)
    #  if( input$precipAirpt1== "All Airports"){
    #    titleName <- "All Airports"
    #  }
    #  p <- ggplot(precipObsPlotDat1n(), aes(updatepred, obsProb))+ 
    #   geom_point(aes(color = N)) + 
    #   facet_wrap(~daysfromforecastLABEL, nrow =1) +
    #   scale_colour_gradient(low = "blue", high = "red", name = "Count") +
    #   labs(x = "improved Probability of Precipitation (%)", y = "Observed Probability of Precipitaiton ", title = titleName )+
    #   geom_abline(intercept = 0, slope =1/100) +
    #   theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) 
    #  p
     #ggplotly(p)
  },
  cacheKeyExpr = { input$precipAirpt1 }
  )
## END Precipitaiton panel plots
## GV 7/13/2018
################################
  
 
  

}