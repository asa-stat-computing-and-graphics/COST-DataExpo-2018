##########################################
## A call function for a particular NCDC site
##  to extract some extra information for
##  plots and explanation in the paper.

library(tidyverse)
library(glmnet)

mySpecialLasso <- function(my.id) {
  predMatrix <- expo.data %>%
    dplyr::select(Date, AirPtCd, ObsMaxTempF) %>%
    distinct() %>%
    spread(AirPtCd, ObsMaxTempF)
  
  ncdc.tmax <- ncdc.all %>%
    drop_na(TMAXF) %>%
    group_by(ID) %>%
    filter(ID==my.id,
           n()>280) %>%
    ungroup()
  
  
  ben <- ncdc.tmax %>%
    dplyr::select(Date,ID,TMAXF)
  ben.dates <- ben %>% dplyr::select(Date)
  both <- left_join(ben, predMatrix, by="Date")
  blah <- both %>% dplyr::select(-Date, -ID)
  blah <- blah[, which(sapply(blah, function(x) !all(is.na(x))))]
  blah.no.na <- as.matrix(na.omit(blah))
  fit.lasso <- cv.glmnet(x=blah.no.na[,-1], y=blah.no.na[,1], nfolds = 5, alpha=1)
  coef.mat <- coef(fit.lasso, s=fit.lasso$lambda.1se, exact=TRUE)
  ind <- which(coef.mat != 0)
  ind <- ind[-1]
  
  const <- 1/min(abs(coef.mat[ind,]))
  hull.weights <- round(const*(abs(coef.mat[ind,])))
  temp <- names(blah)[ind]
  
  my.form <- as.formula(paste("TMAXF ~ ",paste(temp, collapse=' + ' ) ) )
  my.fit <- lm(my.form, data=both)
  list(temp, my.fit, fit.lasso, hull.weights)
}

