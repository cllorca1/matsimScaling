compareMatrices = function(fileName1, fileName2, n, threshold, matrixName1, matrixName2, factor1, factor2){
  
  library(ggplot2)
  source("C:/code/omx/api/r/omx2.R")
  
  ttCongested = readMatrixOMX(fileName1, matrixName1)
  ttUncongested = readMatrixOMX(fileName2, matrixName2)
  
  ttCongested = ttCongested * factor1
  ttUncongested = ttUncongested * factor2
  
  nZones = dim(ttCongested)[1]
  nOrig = n
  randomOrigins = sample(1:nZones,nOrig,replace=F) 
  nDest = n
  randomDestinations = sample(1:nZones,nOrig,replace=F) 
  
  data = data.frame()
  counter=0;
  for (origin in randomOrigins){
    for (destination in randomDestinations){
      tC = ttCongested[origin, destination]
      tU = ttUncongested[origin+1, destination+1]
      if (!is.na(tC) & !is.na(tU)){
        if (tC < threshold){
          newData = data.frame(origin = origin, destination = destination, value1 = tC, value2 = tU)
          data = rbind(data, newData)
        }
      }
    }
    print(paste("origin:",counter, sep = " "))
    counter = counter + 1
  }
  
  print(
    ggplot(data, aes(x=value1, y = value2)) +
      geom_point(size = .3, alpha = .2) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      xlab(fileName1) + 
      ylab(fileName2)
  )
  print(
    ggplot(data, aes(x=value1-value2)) + 
      stat_ecdf() + 
      xlab(paste(fileName1,fileName2, sep="-"))
  )
  #return the data
  data
}


