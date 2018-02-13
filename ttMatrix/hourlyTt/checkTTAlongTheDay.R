
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)

folder = "c:/models/munich/data/"
file = "ttUncongested10.omx"
filePath = paste(folder,file,sep="")

source("C:/code/omx/api/R/omx2.R")

hours = seq(1L,24L,1L)

tt=list()

for (hour in hours){
  matrixName = paste("tt",hour,sep="")
  tt[[hour]] = readMatrixOMX(filePath, matrixName)
  print(hour)  
}

zonesFileName = "c:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(zonesFileName)
regions = unique(zones$JUR_NAME)
regions
subZonesMunich = subset(zones, JUR_NAME=="München")$Zone

nOrig = 50
randomOrigins = sample(zones$Zone,nOrig,replace=F) 
nDest = 50
randomDestinations = sample(zones$Zone, nDest,replace=F) 

origins = list()
destinations = list()
times = matrix(nrow = length(randomOrigins)*length(randomDestinations), ncol = 24)
counter=0;
for (origin in randomOrigins){
  for (destination in randomDestinations){
    counter = counter + 1
    for (hour in hours){
      origins[counter] = origin
      destinations[counter] = destination
      matrix = tt[[hour]]
      time = matrix[origin,destination]
      times[counter,hour] = time
    }
    print(counter)
  }
}

data = as.data.frame(times)


ggplot(data, aes(x=V1, y=V16)) +
  geom_point(alpha=0.1) +
  geom_abline(intercept=0, slope=1, color="red") +
  xlim(0,150) + ylim(0,150) + 
  xlab("travel time at 1:00 (min)") + 
  ylab("travel time at 16:00 (min)")

averageTime = list()
maxTime = list()
minTime = list()
for (hour in hours){
  averageTime[[hour]] = mean(times[,hour])
  maxTime[[hour]]=quantile(times[,hour],0.90)
  minTime[[hour]]=quantile(times[,hour],0.10)
}

summary = data.frame(hour = hours , avg = as.numeric(averageTime), min = as.numeric(minTime), max = as.numeric(maxTime))
summary = melt(summary,id = "hour")

ggplot(summary, aes(x= hour, y=value, color = as.factor(variable))) + geom_line()

#obtain the most common od pairs

fileName = "C:/models/munich/sp/output/plansAuto.csv"

plans = fread(fileName)

summaryOfPlans = plans %>% group_by(origZone, destZone) %>% summarize(count=n())

summaryOfPlans = subset(summaryOfPlans, count > 200)

origs = summaryOfPlans$origZone
dests = summaryOfPlans$destZone

times = matrix(nrow = length(origs), ncol = 24)
counter = 0
for (i in 1:length(origs)){
    counter = counter + 1
    for (hour in hours){
      matrix = tt[[hour]]
      time = matrix[origs[i],dests[i]]
      times[counter,hour] = time
    }
    print(counter)
}

data = data.frame(times)
names(data) = hours

data = cbind(data, odpair = seq(1,32,1))
data = melt(data, id = 25)

ggplot(data, aes(x=as.numeric(variable), y=value, color = as.factor(odpair))) +
  geom_line() + 
  xlab("time of day (h)") + 
  ylab("travel time (min)")

