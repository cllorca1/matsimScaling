##get differences in link volumes and store them in a file

library(ggplot2)
library(dplyr)
library(reshape2)
setwd("C:/projects/MATSim/scaling/analysis/links")

path = "C:/models/munich/output/1.000.75"

exponentCF = 1

exponentSF = 0.75

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
#iterationsVector = c("10","50","100")

iterationsVector = c("50")

dataAll = list()

for (scaling in scalingVector){
  for (iterations in iterationsVector){
    capacity = paste(format(round(as.numeric(scaling)^as.numeric(exponentCF),2),nsmall = 2))
    storage = paste(format(round(as.numeric(scaling)^as.numeric(exponentSF),2),nsmall = 2))
    simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponentSF,"CFExp",exponentCF, "TEST", sep = "")
    lastIterationPath = paste("it.",iterations, sep="")
    fileName = paste("scalingSFExp",exponentSF,"CFExp",exponentCF,"TEST_2016.",iterations,".linkstats.txt.gz",sep = "")
    pathToFile = paste(path,simulationName,"ITERS",lastIterationPath,fileName,sep = "/")
    
    data = read.csv(pathToFile, sep = "\t", header=TRUE)
    #plots the AADT
    rescalingFactor = 1/as.numeric(scaling)
    
    
    data2 = data %>% select(LINK, HRS7.8avg,  HRS17.18avg, TRAVELTIME7.8avg, TRAVELTIME17.18avg )
    
    data2$HRS7.8avg = data2$HRS7.8avg*rescalingFactor
    data2$HRS17.18avg = data2$HRS17.18avg*rescalingFactor
    
    dataAll[[which(scaling == scalingVector)]] = data2
    print(scaling)
  }
}

#with the last data frame created: 
#define groups by capacity and speed
data$capacityCut = cut(data$CAPACITY,
                             breaks = c(-Inf,1250,2500,3750,5000,6125,7500,8750,10000,Inf),
                             labels = c(0,1,2,3,4,5,6,7,8))
data$speedCut = cut(data$FREESPEED,
                          breaks = c(-Inf,5,10,15,20,25,30,Inf),
                          labels = c(0,1,2,3,4,5,6))

differenceTable = data %>% select(LINK, capacityCut, speedCut)


differenceTable$morning1 = dataAll[[1]]$HRS7.8avg
differenceTable$morning5 = dataAll[[2]]$HRS7.8avg
differenceTable$morning10 = dataAll[[3]]$HRS7.8avg
differenceTable$morning20 = dataAll[[4]]$HRS7.8avg
differenceTable$morning50 = dataAll[[5]]$HRS7.8avg
differenceTable$morning100 = dataAll[[6]]$HRS7.8avg

differenceTable$evening1 = dataAll[[1]]$HRS17.18avg
differenceTable$evening5 = dataAll[[2]]$HRS17.18avg
differenceTable$evening10 = dataAll[[3]]$HRS17.18avg
differenceTable$evening20 = dataAll[[4]]$HRS17.18avg
differenceTable$evening50 = dataAll[[5]]$HRS17.18avg
differenceTable$evening100 = dataAll[[6]]$HRS17.18avg

differenceTable$d1m = differenceTable$morning100 - differenceTable$morning1
differenceTable$d5m = differenceTable$morning100 - differenceTable$morning5
differenceTable$d10m = differenceTable$morning100 - differenceTable$morning10
differenceTable$d20m = differenceTable$morning100 - differenceTable$morning20
differenceTable$d50m = differenceTable$morning100 - differenceTable$morning50

differenceTable$d1e = differenceTable$evening100 - differenceTable$evening1
differenceTable$d5e = differenceTable$evening100 - differenceTable$evening5
differenceTable$d10e = differenceTable$evening100 - differenceTable$evening10
differenceTable$d20e = differenceTable$evening100 - differenceTable$evening20
differenceTable$d50e = differenceTable$evening100 - differenceTable$evening50

summary(differenceTable)

ggplot(differenceTable, aes(capacityCut)) + geom_bar()


write.csv(x=differenceTable, file = "networkMap/netowrkDifferences.csv", row.names = FALSE)


differenceTableSelect = differenceTable %>% select(LINK, capacityCut, speedCut, d1m, d5m, d10m, d20m, d50m)

differenceTableSelect = differenceTable %>% select(LINK, capacityCut, speedCut, d1e, d5e, d10e, d20e, d50e)

differenceTableLong = melt(differenceTableSelect, variable.name = "variable", id.vars = c("LINK", "capacityCut", "speedCut"), value.name = "value")

ggplot(differenceTableLong, aes(x=value, by=variable, color=variable)) + stat_ecdf() + xlim(-250,250)
ggplot(differenceTableLong, aes(x=value, by=variable, fill=variable, color=variable)) + geom_density(alpha = .1) + xlim(-250,250)

ggplot(differenceTableLong, aes(x=value, by=variable, fill=variable, color=variable)) + geom_density(alpha = .1, position="fill") + xlim(-250,250)


#calculate squared errors

sqrt(sum((differenceTable$d1m)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d5m)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d10m)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d20m)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d50m)^2)/nrow(differenceTable))

sqrt(sum((differenceTable$d1e)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d5e)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d10e)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d20e)^2)/nrow(differenceTable))
sqrt(sum((differenceTable$d50e)^2)/nrow(differenceTable))


