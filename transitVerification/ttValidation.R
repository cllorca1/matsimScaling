#
#compare travel times from transit
#Carlos Llorca 19.10.17
#


library(placement)
library(ggplot2)

#manually store the api key in the string variable called apiKey!!!!

#set wd to read matrices
setwd("C:/models/munich/data/withTrain")

#read matrices
source("C:/code/omx/api/r/omx2.R")


fileTotalTime = "C:/models/munich/data/withTrain/ttTransitTotaltransitTestCleanComplete.omx"
fileInTransitlTime = "C:/models/munich/data/withTrain/ttTransitIntransitTestCleanComplete.omx"
fileAccess = "C:/models/munich/data/withTrain/ttTransitAccesstransitTestCleanComplete.omx"
fileEgress = "C:/models/munich/data/withTrain/ttTransitEgresstransitTestCleanComplete.omx"
fileTransfers = "C:/models/munich/data/withTrain/ttTransitTransfertransitTestCleanComplete.omx"
fileInVehicle = "C:/models/munich/data/withTrain/ttTransitInVehicletransitTestCleanComplete.omx"

totalTimeM = readMatrixOMX(fileTotalTime, "mat1")
inTransitTimeM = readMatrixOMX(fileInTransitlTime, "mat1")
accessTimeM = readMatrixOMX(fileAccess, "mat1")
eggressTimeM = readMatrixOMX(fileEgress, "mat1")
transfersM = readMatrixOMX(fileTransfers, "mat1")
inVehicleM = readMatrixOMX(fileInVehicle, "mat1")

lookUp = readLookupOMX(fileTotalTime, "lookup1")$Lookup

#read zone coordinates

zonesFileName = "C:/projects/MATSim/Transit Munich/dataValidation/zoneCoordinates.csv"
zones = read.csv(zonesFileName)


#if subset for munich city or only city center
box = list(top = 48.20, bottom = 48.09, left = 11.45, right =11.72)
box = list(top = 48.1523, bottom = 48.1260, left = 11.5451, right =11.6125)

zones = subset(zones, lat < box$top & lat>box$bottom & lon > box$left & lon< box$right )

#create functions to look for lat/lon

getCoord = function(zoneId){
  stationRow = subset(zones, id == zoneId)
  #add not found exception!
  return(paste(stationRow$lat, stationRow$lon,sep=","))
}



#test the travel times

#define the size of the matrix
n = 30

origins = sample(x = zones$id, size = n, replace = F)
destinations = sample(x = zones$id, size = n, replace = F)

originCoordinates = lapply(origins, FUN = getCoord)
destCoordinates = lapply(destinations, FUN = getCoord)

results = data.frame()
emptyResult = data.frame()
for (i in 1:n){
  row = origins[i]
  for (j in 1:n){
    col = destinations[j]
    
    googleResult = drive_time(address = originCoordinates[[i]], dest = destCoordinates[[j]], 
                              auth = "standard_api", privkey = apiKey,
                              clean = FALSE, travel_mode = "transit", 
                              verbose = TRUE, add_date = "fuzzy")
    
    result = c(orig = row, dest = col, googleResult,  tt = totalTimeM[row,col] ,
                transfers = transfersM[row,col], inVehicle = inVehicleM[row,col] , inSystem = inTransitTimeM[row,col],
                access = accessTimeM[row,col], eggress = eggressTimeM[row,col])
    results = rbind(results, result)
    
    
    
  }
}

outputFileName = "C:/projects/MATSim/Transit Munich/dataValidation/outputTripsMunichOnlyCenter.csv"

write.csv(x = results, file = outputFileName)

ggplot(data = results, aes(x=time_mins, y=tt)) + geom_point() + geom_abline(slope = 1, color = "red") +
  xlab("googleTT (min)") + ylab("MATSim TT (min)") + xlim(0,50) + ylim(0,50)

ggplot(data = results, aes(x=time_mins, y=access + inVehicle + eggress)) + geom_point() + geom_abline(slope = 1, color = "red") +
  xlab("googleTT (min)") + ylab("MATSim TT (min)") + xlim(0,100) + ylim(0,100)

ggplot(data = results, aes(x=time_mins, y=inSystem)) + geom_point() + geom_abline(slope = 1, color = "red") +
 xlab("googleTT (min)") + ylab("MATSim TT (min)") + xlim(0,100) + ylim(0,100)







