getLat = function(station){
  stationRow = subset(stationData, stopId == station)
  #add not found exception!
  return(stationRow$lat)
}

getLon = function(station){
  stationRow = subset(stationData, stopId == station)
  #add not found exception!
  return(stationRow$lon)
  
}