#####################################
## Some code that does some data
##   flattening/processing after
##   all the LASSO model fits.

library(tidyverse)

load("lassoForecast.RData")

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

my.models1 <- flattenlist(my.models1)
my.models1 <- bind_rows(my.models1)

my.models2 <- flattenlist(my.models2)
my.models2 <- bind_rows(my.models2)

my.models3 <- flattenlist(my.models3)
my.models3 <- bind_rows(my.models3)

my.models4 <- flattenlist(my.models4)
my.models4 <- bind_rows(my.models4)

my.models5 <- flattenlist(my.models5)
my.models5 <- bind_rows(my.models5)

my.models6 <- flattenlist(my.models6)
my.models6 <- bind_rows(my.models6)

my.models7 <- flattenlist(my.models7)
my.models7 <- bind_rows(my.models7)

my.models8 <- flattenlist(my.models8)
my.models8 <- bind_rows(my.models8)

my.models9 <- flattenlist(my.models9)
my.models9 <- bind_rows(my.models9)

my.models10 <- flattenlist(my.models10)
my.models10 <- bind_rows(my.models10)

my.models11 <- flattenlist(my.models11)
my.models11 <- bind_rows(my.models11)

my.models12 <- flattenlist(my.models12)
my.models12 <- bind_rows(my.models12)

my.models13 <- flattenlist(my.models13)
my.models13 <- bind_rows(my.models13)

my.models14 <- flattenlist(my.models14)
my.models14 <- bind_rows(my.models14)

my.models15 <- flattenlist(my.models15)
my.models15 <- bind_rows(my.models15)

my.models16 <- flattenlist(my.models16)
my.models16 <- bind_rows(my.models16)

my.models17 <- flattenlist(my.models17)
my.models17 <- bind_rows(my.models17)

my.models18 <- flattenlist(my.models18)
my.models18 <- bind_rows(my.models18)

my.models19 <- flattenlist(my.models19)
my.models19 <- bind_rows(my.models19)

my.models20 <- flattenlist(my.models20)
my.models20 <- bind_rows(my.models20)



my.models <- bind_rows(my.models1, my.models2, my.models3, my.models4, my.models5,
                      my.models6, my.models7, my.models8, my.models9, my.models10,
                      my.models11, my.models12, my.models13, my.models14, my.models15,
                      my.models16, my.models17, my.models18, my.models19, my.models20)

ncdc.forecast <- left_join(ncdc.both, my.models, by=c("ID", "Date"))

save(ncdc.forecast, file="ncdcBigDataWithForecast.RData")

