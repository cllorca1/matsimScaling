#
#compare travel times using different scaling factors
#Carlos Llorca 12.06.17
#

library(ggplot2)

#set wd to read matrices
setwd("C:/models/munich/data/1.000.75")

#read matrices
source("C:/code/omx/api/r/omx.R")

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50")
exponentCF = 1
exponentSF = 0.75
matrixList = list()

for (i in 1:6){
  for (j in 1:1){
    scaling = scalingVector[i]
    iterations = iterationsVector[j]
    capacity = paste(format(round(as.numeric(scaling)^as.numeric(exponentCF),2),nsmall = 2))
    storage = paste(format(round(as.numeric(scaling)^as.numeric(exponentSF),2),nsmall = 2))
    simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponentSF,"CFExp",exponentCF, "TEST", sep = "")
    fileName = paste("tt" , simulationName,".omx", sep ="")
    matrixList[[i]] = readMatrixOMX(fileName, "mat1")
  }
}

setwd("C:/projects/MATSim/scaling/analysis/ttODPairs")

nZones = dim(matrixList[[1]])[1]
randomOrigins = sample(1:nZones,20,replace=F) 

nDest = 15
for (origin in randomOrigins){
  tts = data.frame()
  #ttSubmatrix = matrix(nrow = nDest, ncol=6)
  randomDestinations = sample(1:nZones,nDest,replace=F) 
  for (i in 1:length(randomDestinations)){
    #i is the destination / row
    for (j in 1:6){
      #j is the scalingFactor / column
      row = data.frame(orig = origin, scaling = scalingVector[j], dest = randomDestinations[i], tt=matrixList[[j]][origin,randomDestinations[i]])
      tts = rbind(tts,row)
    }
  }
  print(ggplot(tts, aes(x=as.factor(dest), y=tt, color=scaling, group = scaling)) + geom_line()  + geom_point()+ 
          xlab("destination") +  ylab("travelTime"))
  
    
}





