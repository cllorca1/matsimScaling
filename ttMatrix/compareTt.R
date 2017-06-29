#
#compare travel times using different scaling factors
#Carlos Llorca 12.06.17
#

library(ggplot2)
library(reshape2)
library(dplyr)

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

#compare whole matrices - COMPLETE SAMPLE IS THE REFERENCE

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

# dif1$run = "simple1"
# dif5$run = "simple5"
# dif10$run = "simple10"
# dif20$run = "simple20"
# dif50$run = "simple50"

##be carefull with runtime, this is too long

allDifSimple = rbind(dif1, dif5, dif10, dif20, dif50)

ggplot(allDifSimple,aes(x=value, fill=run, color=run)) + stat_density(alpha = .1)+ xlim(-100,100)


dif1f =   melt(as.data.frame(- matrixList[[7]] + matrixList[[12]]),measure.vars = 1:4953)
dif5f =   melt(as.data.frame(- matrixList[[8]] + matrixList[[12]]),measure.vars = 1:4953)
dif10f =  melt(as.data.frame(- matrixList[[9]] + matrixList[[12]]),measure.vars = 1:4953)
dif20f =  melt(as.data.frame(- matrixList[[10]] + matrixList[[12]]),measure.vars = 1:4953)
dif50f =  melt(as.data.frame(- matrixList[[11]] + matrixList[[12]]),measure.vars = 1:4953)

sum(dif1f$value^2)/nrow(dif1)
sum(dif5f$value^2)/nrow(dif1)
sum(dif10f$value^2)/nrow(dif1)
sum(dif20f$value^2)/nrow(dif1)
sum(dif50f$value^2)/nrow(dif1)




#plot all the errors // TEST ONLY
# setwd("C:/projects/MATSim/scaling/analysis/ttODPairs")
# 
# png("dif1.png")
# image((- matrixList[[1]] + matrixList[[6]]), axes = T, useRaster = T, col = rainbow(5))
# dev.off()
# 
# png("dif50.png")
# image((- matrixList[[5]] + matrixList[[6]]), axes = T, useRaster = T, col = rainbow(5))
# dev.off()

#caluclate average errors by origin: 

setwd("C:/projects/MATSim/scaling/analysis/ttODPairs")

dif1 = as.data.frame(- matrixList[[1]] + matrixList[[6]])
dif1Agg = dif1 %>% summarize_all(mean)
dif5 = as.data.frame(- matrixList[[2]] + matrixList[[6]])
dif5Agg = dif5 %>% summarize_all(mean)
dif10 = as.data.frame(- matrixList[[3]] + matrixList[[6]])
dif10Agg = dif10 %>% summarize_all(mean)
dif20 = as.data.frame(- matrixList[[4]] + matrixList[[6]])
dif20Agg = dif20 %>% summarize_all(mean)
dif50 = as.data.frame(- matrixList[[5]] + matrixList[[6]])
dif50Agg = dif50 %>% summarize_all(mean)

difs = rbind(dif1Agg, dif5Agg, dif10Agg, dif20Agg, dif50Agg)
row.names(difs) = c("s1", "s5", "s10", "s20", "s50")
difs = as.data.frame(t(difs))
write.csv(t(difs), file = "simpleNetworkErrors.csv")

dif1 = as.data.frame(- matrixList[[7]] + matrixList[[12]])
dif1Agg = dif1 %>% summarize_all(mean)
dif5 = as.data.frame(- matrixList[[8]] + matrixList[[12]])
dif5Agg = dif5 %>% summarize_all(mean)
dif10 = as.data.frame(- matrixList[[9]] + matrixList[[12]])
dif10Agg = dif10 %>% summarize_all(mean)
dif20 = as.data.frame(- matrixList[[10]] + matrixList[[12]])
dif20Agg = dif20 %>% summarize_all(mean)
dif50 = as.data.frame(- matrixList[[11]] + matrixList[[12]])
dif50Agg = dif50 %>% summarize_all(mean)

difs = rbind(dif1Agg, dif5Agg, dif10Agg, dif20Agg, dif50Agg)
row.names(difs) = c("s1f", "s5f", "s10f", "s20f", "s50f")
difs = as.data.frame(t(difs))
write.csv(difs, file = "fullNetworkErrors.csv")

difs$id = c(1:4953)
difsLong = melt(difs, id.vars = "id")
ggplot(difsLong,aes(x=value, fill=variable, color=variable)) + stat_density(alpha = .1)+ xlim(-10,10)


#compare across networks given the same number of agents



difAllAll = data.frame()
selectedRows = sample(1:totalRows,10000,replace=F) 

for (scalingFactorOrder in 1:6){
tt1s = melt(as.data.frame(matrixList[[scalingFactorOrder]]),measure.vars = 1:4953)
tt1f = melt(as.data.frame(matrixList[[scalingFactorOrder+1]]),measure.vars = 1:4953)

totalRows = 4953^2
dif1All = data.frame(id = c(1:totalRows), simple = tt1s$value, full = tt1f$value)
dif1All = subset(dif1All, id %in% selectedRows)
dif1All$scale = scalingVector[scalingFactorOrder]

print(
  ggplot(dif1All,aes(x=simple, y=full, color=scale)) + geom_point() + geom_abline(slope=1, intercept = 0, color = "blue") + 
  xlim(0,130) + ylim(0,130)
)

difAllAll = rbind(difAllAll, dif1All)
}


ggplot(difAllAll,aes(x=simple-full, color = scale, fill=scale)) +
  stat_density(alpha = 0.1) + 
  xlim(-10,10)

