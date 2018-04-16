library(dplyr)
library(ggplot2)
library(reshape)

#set wd to read matrices
setwd("C:/models/munich/data/")
source("C:/code/omx/api/r/omx2.R")


distanceSkim = readMatrixOMX("c:/models/munich/data/skimsAllIntrazonalRevised.omx", "distanceByDistance")


subfolders = c("skimsTransit/", "bus_tram_metro/", "only_bus/")

modes = c("train", "metro", "bus")

files = c("total_t_", "in_vehicle_t_", "access_t_", "in_transit_t_", "eggress_t_")

matrices = list()
matrices[["distance"]] = distanceSkim
for (i in 1:length(modes)){
  for (j in 1:length(files)){
    name = paste(files[j],modes[i],sep="")
    path = paste(subfolders[i],files[j],modes[i],".omx",sep = "" )
    matrices[[name]] = readMatrixOMX(path, "mat1")
  }
}

fileNameZones = "C:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(fileNameZones)
zonesInMunich = (zones %>% filter(GEORef == 9162))$Zone

#read od pairs

zonesInMunichSubset = sample(x = zonesInMunich, replace = F, size = 100 )

nameOfVariables = names(matrices)
data = data.frame()
counter = 0
for (i in 1:length(zonesInMunichSubset)){
  for (j in i:length(zonesInMunichSubset)){
    row = list(origin =zonesInMunichSubset[i], destination = zonesInMunichSubset[j]  )
    row[["distance"]] = matrices[["distance"]][zonesInMunichSubset[i],zonesInMunichSubset[j]]
    for (variable in nameOfVariables){
      row[[variable]] = matrices[[variable]][zonesInMunichSubset[i],zonesInMunichSubset[j]]
    }
    row = data.frame(row)
    data = rbind(data,row)
    counter = counter + 1 
    if (counter%%1000 == 0){
      print(paste("completed ", counter, " of " ,length(zonesInMunichSubset)^2/2, sep =""))
    }
  }
}

#clean NAs
data[is.na(data)] = -1

#analyze/plot the data

#verify correct calculation of the three components

data = data %>% rowwise() %>% mutate(sum_train = total_t_train - in_transit_t_train - access_t_train - eggress_t_train)
summary(data$sum_train)

data = data %>% rowwise() %>% mutate(sum_metro = total_t_metro - in_transit_t_metro - access_t_metro - eggress_t_metro)
summary(data$sum_metro)

data = data %>% rowwise() %>% mutate(sum_bus = total_t_bus - in_transit_t_bus - access_t_bus - eggress_t_bus)
summary(data$sum_bus)

#time mode vs. mode

ggplot(data, aes(x=total_t_bus, y = total_t_metro)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, intercept = 0 , color = "red") + 
  xlim(-1,200) + ylim(-1,200)

ggplot(data, aes(x=total_t_bus, y = total_t_train)) +
  geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, intercept = 0 , color = "red") + 
  xlim(-1,200) + ylim(-1,200)


#waiting times (include also transfers)

data = data %>% rowwise() %>% mutate(waiting_t_train = in_transit_t_train - in_vehicle_t_train)

data = data %>% rowwise() %>% mutate(waiting_t_metro = in_transit_t_metro - in_vehicle_t_metro)

data = data %>% rowwise() %>% mutate(waiting_t_bus = in_transit_t_bus - in_vehicle_t_bus)

ggplot(data, aes(x = waiting_t_train)) + geom_histogram() + xlim(0,50)
ggplot(data, aes(x = waiting_t_metro)) + geom_histogram() + xlim(0,50)
ggplot(data, aes(x = waiting_t_bus)) + geom_histogram() + xlim(0,50)

#how time changes with distance

data$distanceBin = cut(data$distance/1000, breaks = c(0,2,4,6,8,10,12,14,16,18,20,99999))

timeByDistance = data %>% group_by(distanceBin) %>% 
  summarize(train = mean(total_t_train), bus = mean(total_t_bus), metro = mean(total_t_metro))

timeByDistance$distanceBin = as.numeric(timeByDistance$distanceBin)*2

melted = melt(data = as.data.frame(timeByDistance), id.vars =  "distanceBin")

ggplot(melted, aes(x=distanceBin, y=value, color = variable, group = variable)) + geom_line()
