#
#plot the map line based on stop and line csv files
#

setwd("c:/models/transit")


lines = read.csv("input/allFinal/linesV3.csv", sep = ";", dec = ".")
stationData = read.csv("input/allFinal/stations.csv", sep = ";", dec = ".")

#this functions use a environment data frame called stationData
source("C://code/matsimScaling/transitVerification/functions.R")


for (row in 1:nrow(lines)){
  lines$lat[row] = getLat(lines$stopId[row])
  lines$lon[row] = getLon(lines$stopId[row])
}

write.csv(x=lines, file = "input/lineFrequency/linesWithCoordinates.csv")
#subset(lines, lineId == 156578 )
