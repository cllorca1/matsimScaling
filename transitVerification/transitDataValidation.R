library(ggplot2)
library(dplyr)

library(ggmap)
library(maps)
library(mapdata)

setwd("C://models/transit/output/check")

stopOrderData = read.csv("stopOrder.csv")

setwd("C://models/transit/input/allFinal")
stationData = read.csv("stations.csv", sep = ";")

source("C://code/matsimScaling/transitVerification/functions.R")

stopOrderData$lat = 0
stopOrderData$lon = 0


for (row in 1:nrow(stopOrderData)){
  stopOrderData$lat[row] = getLat(stopOrderData$stopId[row])
  stopOrderData$lon[row] = getLon(stopOrderData$stopId[row])
}


lines = unique(stopOrderData$lineId)
alphaUp = 2 
alphaLow = 0.5


for (line in lines) {

  lineData = stopOrderData %>% filter (lineId == line)
  
  lineName = lineData$lineRef[1]
  
  
  
  if ("r" %in% lineData$accepted == TRUE){
  
    print(ggplot(lineData, aes(x=onLineDist, y = eucDist)) + geom_point(color = "red") + 
      geom_path(color = "red") + geom_abline(slope=alphaUp) + geom_abline(slope = alphaLow) +
      ggtitle(paste ("distances",line,lineName, sep = "-")) +  geom_text(aes(label=seq))
    )
    
    sbbox <- make_bbox(lon = lineData$lon, lat = lineData$lat, f = .5)
    sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
    
    print(
      ggmap(sq_map) + geom_point(color = "red", data = lineData, aes(x=lon, y=lat)) + 
            geom_path(color = "red", data = lineData, aes(x=lon, y=lat)) + 
            ggtitle(paste ("map",line,lineName, sep = "-")) + geom_text(size = 8, color = 'white', data = lineData, aes(x=lon, y=lat , label=seq)) 
            
    )
    
  }
  
}


#ggplot(stopOrderData, aes(x=onLineDist, y = eucDist, color = lineId, group = lineId)) + geom_line()


#functions------------------------------------------------------------------


