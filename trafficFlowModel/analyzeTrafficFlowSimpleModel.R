analyzeLenght = function(folderName, length, aggr, maxVeh){


# link = "/1K/output"
  link = folderName
scales = c("1.0" , "5.0" , "10.0" , "20.0" , "50.0" , "100.0")

individual_data = data.frame()
vehicle_data = data.frame()
# interval_table = data.frame()

#aggregate by time interval
n = 60 / aggr
binWidth = aggr*60
earliest = 0
latest = 24*3600
bins = seq(earliest, latest,binWidth)

for (scale in scales){
  countVehicles = 0
  replication = 1
  
  individual_data_at_scale = data.frame()
  vehicle_data_at_scale = data.frame()
  interval_table_at_scale = data.frame()
  while (countVehicles < maxVeh & replication < 6) {
  #for(replication in 1:10){
    
    runName = paste("scale",scale,"replication",replication,sep="")
    folder = "c:/models/simpleMatsimModel/output/withHoles/"
    fileEnding = ".csv"
    data = fread(paste(folder,link,runName,"/",runName,fileEnding, sep = ""))
    
    dataAux = data %>% select(id,action,time)
    dataCasted = cast(data = dataAux, id ~ action, value = "time")
    dataCasted$travelTime = dataCasted$leave - dataCasted$enter
    dataCasted$speed = length / dataCasted$travelTime * 3.6
    
    scaleNum = as.numeric(scale)
    
    dataCasted$scale = scaleNum
    dataCasted$replication = replication
    vehicle_data_at_scale = rbind(vehicle_data_at_scale,dataCasted)
    
    
    # #time bins
    # binWidth = 30*60
    # earliest = 0
    # latest = 60*3600
    # bins = seq(earliest, latest,binWidth)
    # data$time_interval = cut(data$time, bins)
    
    data$scale = scaleNum
    data$replication = replication
    
    #in the same second chose the minimum number of vehicles registered
    data = data %>% group_by(scale, time) %>% summarize(vehicles = min(vehicles))
    
    #for each time calculate the time from the previous time
    times = data$time
    times = times[2:length(times)]
    times[length(times)+1] = 24 *3600
    data$next_time = times
    
    data = data %>% mutate(time_dif = next_time - time)
    data = data %>% mutate(time_dif = if_else(time_dif<0, true = time, false = time_dif))
    
    data$time_interval = cut(data$time, bins)
    data = data %>% group_by(scale, time_interval) %>% 
      summarize(vehicles = sum(vehicles*time_dif)/sum(time_dif))
    
    individual_data_at_scale = rbind(as.data.frame(data), as.data.frame(individual_data_at_scale))
    # 
    # table = data %>% group_by(time_interval) %>% 
    #   summarize(vehicles = mean(vehicles),
    #             enterFlow =3600 / mean(enterHeadway, na.rm = T),
    #             exitFlow =3600 / mean(exitHeadway,na.rm = T), 
    #             density = mean(density, na.rm = T))
    # 
    # table$scale = scale
    # table$replication = replication
    # interval_table = rbind(interval_table,table)
    
    
    print(paste("length: ", length, ", replication: ", replication, ", scale: ",scale, ", vehicles: ", countVehicles, sep = ""))
    
    countVehicles = countVehicles + nrow(dataCasted)
    replication = replication +1
    
    rm(data, dataCasted, dataAux)
  }
  individual_data_at_scale$replications = replication - 1
  vehicle_data_at_scale$replications = replication - 1
  individual_data_at_scale = individual_data_at_scale %>% group_by(replications, scale, time_interval) %>% summarize(vehicles = mean(vehicles))
  
  individual_data = rbind(as.data.frame(individual_data_at_scale), as.data.frame(individual_data))
  vehicle_data = rbind(vehicle_data, vehicle_data_at_scale)
  
}

vehicle_data$time_interval = cut(vehicle_data$enter, bins)
summaryVehicles1 = vehicle_data %>% group_by(scale, time_interval) %>% 
  summarize(travelTime = mean(travelTime), speed = mean(speed))

maxSpeed = max(summaryVehicles1$speed)
capacity = 20000

#ggplot(vehicle_data, aes(x=travelTime, color = as.factor(scale))) + stat_ecdf()
#ggplot(vehicle_data, aes(x=speed, color = as.factor(scale))) + stat_ecdf()

# ggplot(summaryVehicles1, aes(x=as.numeric(time_interval)*aggr/60, y=speed, color = as.factor(scale))) +
#   geom_path() + geom_point()
# ggplot(summaryVehicles1, aes(x=as.numeric(time_interval)*aggr/60, y=travelTime, color = as.factor(scale))) +
#   geom_path() + geom_point()
# ggplot(summaryVehicles1, aes(x=as.numeric(time_interval)*aggr/60, y=count/scale*100, color = as.factor(scale))) + geom_path()

summaryVehicles1 = merge(x=summaryVehicles1, y=individual_data, by=c("scale","time_interval"))

summaryVehicles1$density = summaryVehicles1$vehicles / summaryVehicles1$scale * 100/length*1000
summaryVehicles1$flow = summaryVehicles1$density * summaryVehicles1$speed

# ggplot(summaryVehicles1, aes(x=as.numeric(time_interval)*aggr/60, y=density, color = as.factor(scale))) +
#   geom_point() + geom_path()
# ggplot(summaryVehicles1, aes(x=as.numeric(time_interval)*aggr/60, y=flow, color = as.factor(scale))) +
#   geom_point() + geom_path()

summaryVehicles1 = summaryVehicles1[order(summaryVehicles1$time_interval),]

# ggplot(summaryVehicles1, aes(x=flow / capacity * n, y=speed/maxSpeed, color = as.factor(scale))) +
#   geom_point() + geom_path()
# 
# summaryVehicles = vehicle_data %>%
#   group_by(scale) %>% summarize(vehicles = n()/mean(replications), tt = mean(travelTime), speed = mean(speed)) 

# tt_ref = (summaryVehicles %>% filter(scale == 100) %>% select(tt))$tt

# ggplot(summaryVehicles, aes(x=scale, y=(tt - tt_ref)/tt_ref)) + geom_path() + geom_point()
# ggplot(summaryVehicles, aes(x=scale, y=speed)) + geom_path() + geom_point()
# ggplot(summaryVehicles, aes(x=scale, y=vehicles/scale*100)) + geom_path() + geom_point()

# summaryInterval = interval_table %>%
#   group_by(time_interval, scale) %>%
#   summarize(enter = mean(enterFlow), exit = mean(exitFlow))
# 
# ggplot(summaryInterval, aes(x=as.numeric(time_interval)/2, y=enter/scale*100, color = as.factor(scale))) +
#   geom_path() 
# ggplot(summaryInterval, aes(x=as.numeric(time_interval)/2, y=exit/scale*100, color = as.factor(scale))) +
#   geom_path()

return(summaryVehicles1)

}


