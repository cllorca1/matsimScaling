analyzeLenght = function(folderName, subfolderName, length, aggr, maxVeh, scales){
  
  #initialize vector of scale factors
  
  
  #initialize data frames for events and for vehicles
  vehicle_data = data.frame()


  #define intervals for aggregation
  n = 60 / aggr
  binWidth = aggr*60
  earliest = 0
  latest = 24*3600
  bins = seq(earliest, latest,binWidth)

  #loop through scales
  for (scale in scales){
    countVehicles = 0
    replication = 1
    
    #initialize subset of the data frame
    vehicle_data_at_scale = data.frame()
    
    #loop throuh replications until a maxVeh number is already available
    while (countVehicles < maxVeh & replication < 6) {
      
      #read the input file
      runName = paste("scale",scale,"replication",replication,sep="")
      folder = "c:/models/simpleMatsimModel/output/"
      folder = paste(folder, subfolderName, "/", sep = "")
      fileEnding = ".csv"
      data = fread(paste(folder,folderName,runName,"/",runName,fileEnding, sep = ""))
      
      
      #subselect columns
      dataAux = data %>% select(id,action,time)
      
      #cast - collapse events into vehicles by id, calculate travel time
      dataCasted = cast(data = dataAux, id ~ action, value = "time")
      dataCasted$travelTime = dataCasted$leave - dataCasted$enter
      dataCasted$speed = length / dataCasted$travelTime * 3.6
      
      #convert scale from string to numeric to expand
      scaleNum = as.numeric(scale)
      
      #add scale and replications to vehicle data
      dataCasted$scale = scaleNum
      dataCasted$replication = replication
      vehicle_data_at_scale = rbind(vehicle_data_at_scale,dataCasted)
      
      print(paste("length: ", length, ", replication: ", replication, ", scale: ",scale, ", vehicles: ", countVehicles, sep = ""))
      
      countVehicles = countVehicles + nrow(dataCasted)
      replication = replication +1
      
      rm(data, dataCasted, dataAux)
    }
    
    #add number of replications considered
    vehicle_data_at_scale$replications = replication - 1

    #join the new rows to the data sets
    vehicle_data = rbind(vehicle_data, vehicle_data_at_scale)
  
}

  #assign time intervals to vehicles
  vehicle_data$time_interval = cut(vehicle_data$enter, bins)
  
  #calculate the average travel time by time interval
  summaryVehicles1 = vehicle_data %>% group_by(scale, time_interval) %>% 
    summarize(travelTime = mean(travelTime), speed = mean(speed))
  
  #restore the order of vehicles
  summaryVehicles1 = summaryVehicles1[order(summaryVehicles1$time_interval),]
  
  return(summaryVehicles1)
  
}


