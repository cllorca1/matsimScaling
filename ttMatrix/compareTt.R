#
#compare travel times using different scaling factors
#Carlos Llorca 12.06.17
#

library(ggplot2)
library(reshape2)

#set wd to read matrices
setwd("C:/models/munich/data/1.000.75")

#read matrices
source("C:/code/omx/api/r/omx.R")

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50")
exponentCF = 1
exponentSF = 0.75
matrixList = list()

#simple
setwd("C:/models/munich/data/1.000.75")
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

#full
setwd("C:/models/munich/data/1.000.75full")
for (i in 1:6){
  for (j in 1:1){
    scaling = scalingVector[i]
    iterations = iterationsVector[j]
    capacity = paste(format(round(as.numeric(scaling)^as.numeric(exponentCF),2),nsmall = 2))
    storage = paste(format(round(as.numeric(scaling)^as.numeric(exponentSF),2),nsmall = 2))
    simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponentSF,"CFExp",exponentCF, "TEST", sep = "")
    fileName = paste("tt" , simulationName,".omx", sep ="")
    matrixList[[6+i]] = readMatrixOMX(fileName, "mat1")
  }
}

#compare a set of origin-destination alternatives

setwd("C:/projects/MATSim/scaling/analysis/ttODPairs")

nZones = dim(matrixList[[1]])[1]
nOrig = 10
randomOrigins = sample(1:nZones,nOrig,replace=F) 
nDest = 20
plot = T

for (origin in randomOrigins){
  tts = data.frame()
  #ttSubmatrix = matrix(nrow = nDest, ncol=6)
  randomDestinations = sample(1:nZones,nDest,replace=F) 
  for (i in 1:length(randomDestinations)){
    #i is the destination / row
    for (j in 1:6){
      #j is the scalingFactor / column
      row = data.frame(network = as.factor("simple"), orig = origin, scaling = scalingVector[j], dest = randomDestinations[i], tt=matrixList[[j]][origin,randomDestinations[i]])
      tts = rbind(tts,row)
    }
    for (j in 1:6){
      #j is the scalingFactor / column
      row = data.frame(network = as.factor("full"), orig = origin, scaling = scalingVector[j], dest = randomDestinations[i], tt=matrixList[[6+j]][origin,randomDestinations[i]])
      tts = rbind(tts,row)
    }
  }
  if(plot){
    print(ggplot(subset(tts,network == "simple"), aes(x=as.factor(dest), y=tt, color=scaling, group = scaling)) + geom_line()  + geom_point()+ 
          xlab("destination") +  ylab("travelTime") + ggtitle(label = "simple"))
    print(ggplot(subset(tts,network == "full"), aes(x=as.factor(dest), y=tt, color=scaling, group = scaling)) + geom_line()  + geom_point()+ 
            xlab("destination") +  ylab("travelTime")+ ggtitle(label = "full"))
    
  }
    
}

#compare whole matrices

dif1 =   melt(as.data.frame(- matrixList[[1]] + matrixList[[6]]),measure.vars = 1:4953)
dif5 =   melt(as.data.frame(- matrixList[[2]] + matrixList[[6]]),measure.vars = 1:4953)
dif10 =  melt(as.data.frame(- matrixList[[3]] + matrixList[[6]]),measure.vars = 1:4953)
dif20 =  melt(as.data.frame(- matrixList[[4]] + matrixList[[6]]),measure.vars = 1:4953)
dif50 =  melt(as.data.frame(- matrixList[[5]] + matrixList[[6]]),measure.vars = 1:4953)

sum(dif1$value^2)/nrow(dif1)
sum(dif5$value^2)/nrow(dif1)
sum(dif10$value^2)/nrow(dif1)
sum(dif20$value^2)/nrow(dif1)
sum(dif50$value^2)/nrow(dif1)


dif1 =   melt(as.data.frame(- matrixList[[7]] + matrixList[[12]]),measure.vars = 1:4953)
dif5 =   melt(as.data.frame(- matrixList[[8]] + matrixList[[12]]),measure.vars = 1:4953)
dif10 =  melt(as.data.frame(- matrixList[[9]] + matrixList[[12]]),measure.vars = 1:4953)
dif20 =  melt(as.data.frame(- matrixList[[10]] + matrixList[[12]]),measure.vars = 1:4953)
dif50 =  melt(as.data.frame(- matrixList[[11]] + matrixList[[12]]),measure.vars = 1:4953)

sum(dif1$value^2)/nrow(dif1)
sum(dif5$value^2)/nrow(dif1)
sum(dif10$value^2)/nrow(dif1)
sum(dif20$value^2)/nrow(dif1)
sum(dif50$value^2)/nrow(dif1)

#plot all the errors
setwd("C:/projects/MATSim/scaling/analysis/ttODPairs")
png("dif1.png")
image((- matrixList[[1]] + matrixList[[6]]), axes = T, useRaster = T, col = rainbow(7))
dev.off()

png("dif50.png")
image((- matrixList[[5]] + matrixList[[6]]), axes = T, useRaster = T, col = rainbow(7))
dev.off()


