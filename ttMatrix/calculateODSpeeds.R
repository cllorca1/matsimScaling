calculateSpeeds = function(fileName, n, threshold, distanceMatrix, timeMatrix){
  
  library(ggplot2)
  source("C:/code/omx/api/r/omx2.R")
  
  matrix1 = readMatrixOMX(fileName, distanceMatrix)
  matrix2 = readMatrixOMX(fileName, timeMatrix)
  
  nZones = dim(matrix1)[1]
  nOrig = n
  randomOrigins = sample(1:nZones,nOrig,replace=F) 
  nDest = n
  randomDestinations = sample(1:nZones,nOrig,replace=F) 
  
  data = data.frame()
  counter=0;
  for (origin in randomOrigins){
    for (destination in randomDestinations){
      distance = matrix1[origin, destination]
      time = matrix2[origin, destination]
      if (!is.na(distance) & !is.na(time)){
        if (distance){
          newData = data.frame(origin = origin, destination = destination, distance = distance, time = time)
          data = rbind(data, newData)
        }
      }
    }
    print(paste("origin:",counter, sep = " "))
    counter = counter + 1
  }
  
  data$speed = data$distance/data$time * 3.6
  
  print(
    ggplot(data, aes(x=speed)) + 
      stat_ecdf() + 
      xlab("speed(km/h)") + ylab("cumulative probability")
  )
  
  print(
    ggplot(data, aes(x=speed)) + 
      geom_histogram() + 
      xlab("speed(km/h)") + ylab("frequency")
  )
  
  print("maximum speed:")
  print(max(data$speed))
  print("minimum speed:")
  print(min(data$speed))  
}
