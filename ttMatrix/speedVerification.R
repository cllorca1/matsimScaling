
source("c:/code/matsimScaling/ttMatrix/calculateODSpeeds.R")

folder = "C:/models/munich/data/"

fileName = paste(folder,"skimsAllIntrazonal.omx", sep="")

#check random 1M od pairs and calculate speeds based on shortest path
calculateSpeeds(fileName, 200, 100000, "distanceByDistance", "timeByDistance")

#check random 1M od pairs and calculate speeds based on fastest path
calculateSpeeds(fileName, 200, 100000, "distanceByTime", "timeByTime")

####test intrazonals
data = data.frame()
for (i in 1:4953){
  row = data.frame(time = matrix2[i,i], distance = matrix1[i,i])
  data = rbind(data, row)
}

data$speed = data$distance / data$time * 3.6

summary(data$speed)
