################################################
## The main call function that fits the LASSO
##   regression and uses it for predictions

library(dplyr)
library(glmnet)
library(tidyr)
library(foreach)
library(doParallel)

set.seed(1)

source("hullSelectedSites.R")

options(width=120);

load("../CleanedDataExpo.RData")
load("../fullyProcessedNCDCdataBig.RData")


# Reduce the expo data to just what we need (saves memory)
expo.data <- expo.data %>%
  dplyr::select(Date, ObsMaxTempF, ObsMinTempF, AirPtCd, long, lat,
                ForecastMaxTempF, ForecastMinTempF, DaysDiff)

predMatrix.max <- expo.data %>%
  dplyr::select(Date, AirPtCd, ObsMaxTempF) %>%
  distinct() %>%
  spread(AirPtCd, ObsMaxTempF)

predMatrix.min <- expo.data %>%
  dplyr::select(Date, AirPtCd, ObsMinTempF) %>%
  distinct() %>%
  spread(AirPtCd, ObsMinTempF)



# remove(ncdc.all)
ncdc.tmax <- ncdc.all %>%
  drop_na(TMAXF) %>%
  group_by(ID) %>%
  filter(n()>280) %>%
  ungroup() %>%
  dplyr::select(ID, lat, long, Date, TMAXF)
tmax.ids <- unique(ncdc.tmax$ID)

ncdc.tmin <- ncdc.all %>%
  drop_na(TMINF) %>%
  group_by(ID) %>%
  filter(n()>280) %>%
  ungroup() %>%
  dplyr::select(ID, lat, long, Date, TMINF)
tmin.ids <- unique(ncdc.tmin$ID)


ncdc.both <- full_join(ncdc.tmax, ncdc.tmin, by=c("ID", "lat", "long", "Date"))


#Function
myFunction <- function(uniqueID){
  cat(paste(uniqueID, "\n"), file="processedIDs.txt", append=TRUE)
  my.ncdc <- ncdc.both %>%
    filter(ID==uniqueID) %>%
    dplyr::select(Date,ID)
  ################################################
  ## Model for Maximum -- use my.ncdc.tmax
  ################################################
  if(uniqueID %in% tmax.ids) {
    my.ncdc.tmax <- ncdc.both %>%
      filter(ID==uniqueID) %>%
      dplyr::select(Date,TMAXF)

    lasso.data <- left_join(my.ncdc.tmax, predMatrix.max, by="Date")
    lasso.data <- lasso.data %>% dplyr::select(-Date)
    lasso.data <- lasso.data[, which(sapply(lasso.data, function(x) !all(is.na(x))))]
    lasso.data <- as.matrix(na.omit(lasso.data))
    fit.lasso <- cv.glmnet(x=lasso.data[,-1], y=lasso.data[,1], alpha=1,nfolds = 5)
    lasso.data <- as.data.frame(lasso.data)
    coef.mat <- coef(fit.lasso, s=fit.lasso$lambda.1se, exact=TRUE)
    ind <- which(coef.mat != 0)
    ind <- ind[-1]
    importantExpoLoc <- colnames(lasso.data)[ind]
    ## If nothing is picke,d use the minimum lambda standard.
    if(length(importantExpoLoc)==0) {
      coef.mat <- coef(fit.lasso, s=fit.lasso$lambda.min, exact=TRUE)
      ind <- which(coef.mat != 0)
      ind <- ind[-1]
      importantExpoLoc <- colnames(lasso.data)[ind]
    }
    if(length(importantExpoLoc)==0) {
      coef.mat <- coef(fit.lasso, s=min(fit.lasso$lambda), exact=TRUE)
      ind <- which(coef.mat != 0)
      ind <- ind[-1]
      importantExpoLoc <- colnames(lasso.data)[ind]
    }

    my.form <- as.formula(paste("TMAXF ~ ",paste(importantExpoLoc, collapse=' + ' ) ) )

    my.fit <- lm(my.form, data=lasso.data)
    
    my.new.data <- expo.data %>% filter(AirPtCd %in% importantExpoLoc)
    
    ###############################
    ## Using the LASSO fit
    ## Making predictions for the previous 5-days
    for(i in 1:5) {
      predMatrix.new <- my.new.data %>% 
        filter(DaysDiff==i) %>%
        dplyr::select(Date, AirPtCd, ForecastMaxTempF) %>%
        distinct() %>%
        spread(AirPtCd, ForecastMaxTempF)
      ## Need to stop here so predMatrix.new exist for the prediction
      predMatrix.new <- predMatrix.new %>%
        mutate(Pred=predict(my.fit, newdata=predMatrix.new)) %>%
        dplyr::select(Date, Pred)
      predMatrix.new$ID <- uniqueID
      names(predMatrix.new)[2] <- paste("Day", i, "LassoForecastedTMaxF", sep="")
      my.ncdc <- left_join(my.ncdc, predMatrix.new, by=c("ID", "Date"))
    }
 
    
    #######################################
    ##
    ## Now pick the data by hulls from the LASSO selected points
    ##
    hull.weights <- round(1/min(abs(coef.mat[ind,]))*(abs(coef.mat[ind,])))
    
    hullLocations <- tryCatch(selectedSitesHull(hull.weights),
                              error=function(e) {
                                      cat(paste("Max try Error:", uniqueID, "\n"), file="error.txt", append=TRUE)
                                      hullLocations <- importantExpoLoc } )

    ## If the hull picks nothing, go back to previous result
    if(length(hullLocations)==0) {
       hullLocations <- importantExpoLoc
       cat(paste("Max Error:", uniqueID, "\n"), file="error.txt", append=TRUE)
    }
    
    my.form <- as.formula(paste("TMAXF ~ ",paste(hullLocations, collapse=' + ' ) ) )
    
    my.fit <- lm(my.form, data=lasso.data)
    
    my.new.data <- expo.data %>% filter(AirPtCd %in% hullLocations)
    
    ###############################
    ## Using the LASSO fit
    ## Making predictions for the previous 5-days
    for(i in 1:5) {
      predMatrix.new <- my.new.data %>% 
        filter(DaysDiff==i) %>%
        dplyr::select(Date, AirPtCd, ForecastMaxTempF) %>%
        distinct() %>%
        spread(AirPtCd, ForecastMaxTempF)
      ## Need to stop here so predMatrix.new exist for the prediction
      predMatrix.new <- predMatrix.new %>%
        mutate(Pred=predict(my.fit, newdata=predMatrix.new)) %>%
        dplyr::select(Date, Pred)
      predMatrix.new$ID <- uniqueID
      names(predMatrix.new)[2] <- paste("Day", i, "HullForecastedTMaxF", sep="")
      my.ncdc <- left_join(my.ncdc, predMatrix.new, by=c("ID", "Date"))
    }
  } ## End of  if(uniqueID %in% tmax.ids)
  
  ################################################
  ## Model for Minimum -- use my.ncdc.tmin
  ################################################
  if(uniqueID %in% tmin.ids) {
    my.ncdc.tmin <- ncdc.both %>%
      filter(ID==uniqueID) %>%
      dplyr::select(Date,ID,TMINF)
    
    lasso.data <- left_join(my.ncdc.tmin, predMatrix.min, by="Date")
    lasso.data <- lasso.data %>% dplyr::select(-Date, -ID)
    lasso.data <- lasso.data[, which(sapply(lasso.data, function(x) !all(is.na(x))))]
    lasso.data <- as.matrix(na.omit(lasso.data))
    fit.lasso <- cv.glmnet(x=lasso.data[,-1], y=lasso.data[,1], alpha=1, nfolds = 5)
    lasso.data <- as.data.frame(lasso.data)
    coef.mat <- coef(fit.lasso, s=fit.lasso$lambda.1se, exact=TRUE)
    ind <- which(coef.mat != 0)
    ind <- ind[-1]
    importantExpoLoc <- colnames(lasso.data)[ind]
    ## If nothing is picke,d use the minimum lambda standard.
    if(length(importantExpoLoc)==0) {
      coef.mat <- coef(fit.lasso, s=fit.lasso$lambda.min, exact=TRUE)
      ind <- which(coef.mat != 0)
      ind <- ind[-1]
      importantExpoLoc <- colnames(lasso.data)[ind]
    }
    if(length(importantExpoLoc)==0) { ## Still nothing is picked!
      coef.mat <- coef(fit.lasso, s=min(fit.lasso$lambda), exact=TRUE)
      ind <- which(coef.mat != 0)
      ind <- ind[-1]
      importantExpoLoc <- colnames(lasso.data)[ind]
    }

    my.form <- as.formula(paste("TMINF ~ ",paste(importantExpoLoc, collapse=' + ' ) ) )

    my.fit <- lm(my.form, data=lasso.data)
    
    my.new.data <- expo.data %>% filter(AirPtCd %in% importantExpoLoc)
    ###############################
    ## Using the LASSO fit
    ## Making predictions for the previous 5-days
    for(i in 1:5) {
      predMatrix.new <- my.new.data %>% 
        filter(DaysDiff==i) %>%
        dplyr::select(Date, AirPtCd, ForecastMinTempF) %>%
        distinct() %>%
        spread(AirPtCd, ForecastMinTempF)
      ## Need to stop here so predMatrix.new exist for the prediction
      predMatrix.new <- predMatrix.new %>%
        mutate(Pred=predict(my.fit, newdata=predMatrix.new)) %>%
        dplyr::select(Date, Pred)
      predMatrix.new$ID <- uniqueID
      names(predMatrix.new)[2] <- paste("Day", i, "LassoForecastedTMinF", sep="")
      my.ncdc <- left_join(my.ncdc, predMatrix.new, by=c("ID", "Date"))
    }
    #######################################
    ##
    ## Now pick the data by hulls from the LASSO selected points
    ##
    
    hull.weights <- round(const <- 1/min(abs(coef.mat[ind,]))*(abs(coef.mat[ind,])))
    
    hullLocations <- tryCatch(selectedSitesHull(hull.weights),
                              error=function(e) {
				      cat(paste("Min try Error:", uniqueID, "\n"), file="error.txt", append=TRUE)
				      hullLocations <- importantExpoLoc } )
    
    ## If the hull picks nothing, go back to previous result
    if(length(hullLocations)==0) {
       hullLocations <- importantExpoLoc
       cat(paste("Min Error:", uniqueID, "\n"), file="error.txt", append=TRUE)
    }
    
    my.form <- as.formula(paste("TMINF ~ ",paste(hullLocations, collapse=' + ' ) ) )
    
    my.fit <- lm(my.form, data=lasso.data)
    
    my.new.data <- expo.data %>% filter(AirPtCd %in% hullLocations)
    
    ###############################
    ## Using the LASSO fit
    ## Making predictions for the previous 5-days
    for(i in 1:5) {
      predMatrix.new <- my.new.data %>% 
        filter(DaysDiff==i) %>%
        dplyr::select(Date, AirPtCd, ForecastMinTempF) %>%
        distinct() %>%
        spread(AirPtCd, ForecastMinTempF)
      ## Need to stop here so predMatrix.new exist for the prediction
      predMatrix.new <- predMatrix.new %>%
        mutate(Pred=predict(my.fit, newdata=predMatrix.new)) %>%
        dplyr::select(Date, Pred)
      predMatrix.new$ID <- uniqueID
      names(predMatrix.new)[2] <- paste("Day", i, "HullForecastedTMinF", sep="")
      my.ncdc <- left_join(my.ncdc, predMatrix.new, by=c("ID", "Date"))
    }
    
  } ## End of  if(uniqueID %in% tmin.ids)
  my.ncdc
}

IDs.all <- unique(ncdc.both$ID)
length(IDs.all)

## > length(IDs.all)
## [1] 9700

## Clean up some memory before the cluster
rm(ncdc.all, ncdc.tmax, ncdc.tmin)
gc()

cat("Errors thus far\n\n", file="error.txt")
cat("Processed IDs\n\n", file="processedIDs.txt")

## Load everything into the cluster domain
## Setup number of nodes for cluster
num.nodes <- 5;
# cl <- makeForkCluster(num.nodes)
cl <- registerDoParallel(cores = num.nodes)

ptm <- proc.time()


IDs <- IDs.all[1:500]
my.models1 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }
# my.models1 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[501:1000]
my.models2 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models2 <- parLapply(cl, IDs, myFunction)
# my.models3 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[1001:1500]
my.models3 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
  { myFunction(uniqueID) }

IDs <- IDs.all[1501:2000]
my.models4 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models4 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[2001:2500]
my.models5 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models5 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[2501:3000]
my.models6 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models6 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[3001:3500]
my.models7 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models7 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[3501:4000]
my.models8 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models8 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[4001:4500]
my.models9 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models9 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[4501:5000]
my.models10 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models10 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[5001:5500]
my.models11 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models11 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[5501:6000]
my.models12 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models12 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[6001:6500]
my.models13 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models13 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[6501:7000]
my.models14 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models14 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[7001:7500]
my.models15 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models15 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[7501:8000]
my.models16 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models16 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[8001:8500]
my.models17 <- foreach(uniqueID = IDs,
                       .combine = list) %dopar%
                       { myFunction(uniqueID) }

# my.models17 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[8501:9000]
my.models18 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models18 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[9001:9500]
my.models19 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models19 <- parLapply(cl, IDs, myFunction)
IDs <- IDs.all[9501:9700]
my.models20 <- foreach(uniqueID = IDs,
                      .combine = list) %dopar%
                      { myFunction(uniqueID) }

# my.models20 <- parLapply(cl, IDs, myFunction)


proc.time()-ptm

# stopCluster(cl)
stopImplicitCluster()

save(ncdc.both, 
     my.models1, my.models2, my.models3, my.models4, my.models5,
     my.models6, my.models7, my.models8, my.models9, my.models10,
     my.models11, my.models12, my.models13, my.models14, my.models15,
     my.models16, my.models17, my.models18, my.models19, my.models20,
     file="lassoForecast.RData")


## Unlist my predictions into giant dataframe
#my.models1 <- bind_rows(my.models1[[1]][[1]][[1]][[1]][[1]])
#my.models2 <- bind_rows(my.models2[[1]][[1]][[1]][[1]][[1]])
#my.models3 <- bind_rows(my.models3[[1]][[1]][[1]][[1]][[1]])
#my.models4 <- bind_rows(my.models4[[1]][[1]][[1]][[1]][[1]])
#my.models5 <- bind_rows(my.models5[[1]][[1]][[1]][[1]][[1]])
#my.models6 <- bind_rows(my.models6[[1]][[1]][[1]][[1]][[1]])
#my.models7 <- bind_rows(my.models7[[1]][[1]][[1]][[1]][[1]])
#my.models8 <- bind_rows(my.models8[[1]][[1]][[1]][[1]][[1]])
#my.models9 <- bind_rows(my.models9[[1]][[1]][[1]][[1]][[1]])
#my.models10 <- bind_rows(my.models10[[1]][[1]][[1]][[1]][[1]])
#my.models11 <- bind_rows(my.models11[[1]][[1]][[1]][[1]][[1]])
#my.models12 <- bind_rows(my.models12[[1]][[1]][[1]][[1]][[1]])
#my.models13 <- bind_rows(my.models13[[1]][[1]][[1]][[1]][[1]])
#my.models14 <- bind_rows(my.models14[[1]][[1]][[1]][[1]][[1]])
#my.models15 <- bind_rows(my.models15[[1]][[1]][[1]][[1]][[1]])
#my.models16 <- bind_rows(my.models16[[1]][[1]][[1]][[1]][[1]])
#my.models17 <- bind_rows(my.models17[[1]][[1]][[1]][[1]][[1]])
#my.models18 <- bind_rows(my.models18[[1]][[1]][[1]][[1]][[1]])
#my.models19 <- bind_rows(my.models19[[1]][[1]][[1]][[1]][[1]])
#my.models20 <- bind_rows(my.models20[[1]][[1]][[1]][[1]][[1]])



#my.models <- bind_rows(my.models1, my.models2, my.models3, my.models4, my.models5,
#                       my.models6, my.models7, my.models8, my.models9, my.models10,
#                       my.models11, my.models12, my.models13, my.models14, my.models15,
#                       my.models16, my.models17, my.models18, my.models19, my.models20)
#
#ncdc.forecast <- left_join(ncdc.both, my.models, by=c("ID", "Date"))

#save(ncdc.forecast, file="ncdcBigDataWithForecast.RData")





