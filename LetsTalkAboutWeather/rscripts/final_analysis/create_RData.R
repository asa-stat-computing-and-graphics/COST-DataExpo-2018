# set working directory to source file location

library(fiftystater)


#==============================================================================
#           Identify files and variables for each figure and table
#==============================================================================

# Table 1 - note that this is the dataset with the ranges for Table 1. It is
# not Table 1. Further manipulation is needed to get that table. 
# tab1 <- read.csv("../../data/final_weather.csv")

# Figure 1
# cluster2 <- read.csv("../../data/summary_city.csv")

# Figure 2
# cluster2 <- read.csv("../../data/summary_city.csv")

# Figure 3
# cluster2 <- read.csv("../../data/clusterMapFinal.csv")

# Figure 4
# locations <- read.csv("../../data/locationsFinal.csv")

# Figure 5
# scatter <- read.csv("../../data/summary_lag_shiny.csv")
# clust <- read.csv("../../data/locationsFinal.csv")

# Figure 6
# weatherSum <- read.csv("../../data/summary_city_month.csv")
# locations <- read.csv("../../data/locationsFinal.csv")

# Figure 7
# rf.dat <- read.csv("../../data/summary_city_month_lag.csv")
# clusters <- read.csv("../../data/Cluster_Summary.csv")

# Figure 8
# No external data needed

#==============================================================================
#                       read in files that I need

# The following brings in the needed files and reassigns names that will
# be used in the RMD file. 

data("fifty_states")

weather <- read.csv("../../data/final_weather.csv")
city <- read.csv("../../data/summary_city.csv")
map <- read.csv("../../data/clusterMapFinal.csv")
locations <- read.csv("../../data/locationsFinal.csv")
forecast <- read.csv("../../data/summary_lag_shiny.csv")
monthSum <- read.csv("../../data/summary_city_month.csv")
cityForecast <- read.csv("../../data/summary_city_month_lag.csv")
clusters <- read.csv("../../data/Cluster_Summary.csv")
varNameLabels <- read.csv("../../data/varNameLabels.csv")


#==============================================================================
#                       Read in cluster functions

source("../exploration_explanation/cluster_functions.R")

save(
  fifty_states,
  weather,
  city,
  map,
  locations,
  forecast,
  monthSum,
  cityForecast,
  clusters,
  fastClust,
  clust.plot,
  clustPlot,
  varNameLabels,
  file = "../../weather.RData")

load("../../weather.RData")

save(fifty_states, file = "../../data/fifty_states.RData")

load("../../data/fifty_states.RData")


