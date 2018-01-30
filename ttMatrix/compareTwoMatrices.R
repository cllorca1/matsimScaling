


#set wd to read matrices
setwd("C:/models/munich/accessibility")

#matrix filenames for congested and uncongested
fileNameCongested = "tt10Congested.omx"
fileNameUncongested = "ttUncongested.omx"

compareMatrices(fileNameCongested, fileNameUncongested, 200, 9999999)

#matrix filenames for time or distance based distance matrices
fileNameDistance = "tdShortestPath.omx"
fileNameTime = "tdFastestPath.omx"

data = compareMatrices(fileNameDistance, fileNameTime, 300, 20000)


#matrix filenames for time or distance based distance matrices
fileNameDistance = "tdShortestPath.omx"
fileNameDistanceFine = "tdShortestPathFine.omx"

data = compareMatrices(fileNameDistance, fileNameDistanceFine, 300, 20000)

mean(data$value1)
mean(data$value2)


file1 = "tdTestIntrazonal.omx"
file2 = "tdTest.omx"

data = compareMatrices(file1, file2, 100, 150000)


#matrices for transit with and without 2SS
setwd("C:/models/munich/data/2ss_before_after")

#matrix filenames for congested and uncongested
fileName1 = "ttTransitTotaltest2SSafterComplete.omx"
fileName2 = "ttTransitTotaltest2SSbeforeComplete.omx"

compareMatrices(fileName1, fileName2, 200, 9999)



#matrices for transit with and without 2SS


#matrix filenames for congested and uncongested
fileName1 = "tdEuc.omx"
fileName2 = "tdTest.omx"

data = compareMatrices(fileName1, fileName2, 200, 50000)

ggplot(data, aes(x=value2)) + stat_ecdf()


listOMX(file2)





compareMatrices = function(fileName1, fileName2, n, threshold){
  
  library(ggplot2)
  source("C:/code/omx/api/r/omx2.R")
  
  ttCongested = readMatrixOMX(fileName1, "mat1")
  ttUncongested = readMatrixOMX(fileName2, "mat1")
  
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
      tU = ttUncongested[origin, destination]
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


