library(ggplot2)
library(dplyr)
setwd("C:/projects/MATSim/scaling/analysis/links")


#Analyze network link characteristics (static)
linksTable = read.csv("networkMap/linksList.csv")

setwd("C:/projects/MATSim/scaling/analysis/links")
linksTableFull = fread("networkMap/linksListDense.csv")

linksTable$laneCapacity = linksTable$CAPACITY / linksTable$LANES
linksTableFull$laneCapacity = linksTableFull$CAPACITY / linksTableFull$LANES

bins = c(0,100,500,1000,5000,50000)
linksTable$l_bin = cut(linksTable$LENGTH, breaks = bins)
linksTableFull$l_bin = cut(linksTableFull$LENGTH, breaks = bins)

bins = c(0,501,1001,1501,2001,50000)
linksTable$c_bin = cut(linksTable$laneCapacity, breaks = bins)
linksTableFull$c_bin = cut(linksTableFull$laneCapacity, breaks = bins)




linksTable %>%
  group_by(l_bin, c_bin) %>%
  summarize(length = sum(LENGTH)) %>% 
  tidyr::spread(c_bin, length)
linksTableFull %>%
  group_by(l_bin, c_bin) %>%
  summarize(length = sum(LENGTH)) %>% 
  tidyr::spread(c_bin, length)


linksTable$capacityCut = cut(linksTable$CAPACITY,
                             breaks = c(-Inf,1250,2500,3750,5000,6125,7500,8750,10000,Inf),
                             labels = c(0,1,2,3,4,5,6,7,8))
linksTable$speedCut = cut(linksTable$FRSPEED,
                             breaks = c(-Inf,5,10,15,20,25,30,Inf),
                             labels = c(0,1,2,3,4,5,6))

summaryNetwork = linksTable %>% group_by(capacityCut, speedCut) %>% summarize(number = n())

summaryNetwork = linksTable %>% group_by(capacityCut, speedCut) %>% summarize(number = n(), sumL = sum(LENGTH))

ggplot(summaryNetwork, aes(x=capacityCut, y=speedCut, fill= number, colorRampPalette(colors = red))) + geom_bin2d()

ggplot(linksTable, aes(x=CAPACITY, y=FRSPEED)) + geom_point() + geom_jitter(width = 30, height = 1)



path = "C:/models/munich/output/1.000.75"

exponentCF = 1

exponentSF = 0.75

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("10","50","100")

iterationsVector = c("50")

dataAll = data.frame()

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
    
    data$sf = as.numeric(scaling)
    dataAll = rbind(dataAll, data)
    print(fileName)
  }
}

write.csv (x=dataAll, file="dataAllLinks.csv",row.names = FALSE)

##after having read the raw data:

dataAll = read.csv(file="dataAllLinks.csv")

routeTable = read.csv("a99Route.csv")
routeTable = read.csv("feld2Perl.csv")
routeTable = read.csv("a8Route.csv")
routeTable = read.csv("MUC2Graf.csv")
routeTable = read.csv("hbf2MunFre.csv")

listOfLinks = routeTable$links
listOfLinkNames = routeTable$order

listOfLinks = c(56921, 7777, 22549, 67260,49001,114344,66950)
listOfLinks = c(108899, 107460, 33532, 67260,49001,6060,70732) #alternative
listOfLinkNames = c("A99ring", "Schelei?heimerstr,", "Dachauerstr.", "St2063 am Starnberg See","local road in Ellgau","Arcisstr.","A8 Augsburg - MUC")




smallSample = subset(dataAll, LINK %in% listOfLinks)
smallSample$streetName = listOfLinkNames[match(smallSample$LINK, listOfLinks)]

##to get the station
listOfStations = list()
pk = 0
for (i in 1:length(listOfLinks)){
  pk = pk + smallSample$LENGTH[(match(listOfLinks[i],smallSample$LINK))]
  listOfStations[i] = pk
}
smallSample$station = listOfStations[match(smallSample$LINK, listOfLinks)]


#plots with x=station
ggplot(smallSample, aes(x=station, y=CAPACITY, group = sf, color = sf)) + geom_line(size = 1.5) + ylim(0,12500) + xlab("length (m)") + ylab("capacity (vph)")
ggplot(smallSample, aes(x=station, y=FREESPEED*3.6, group = sf, color = sf)) + geom_line(size = 1.5) + ylim(0,125) + xlab("length (m)") + ylab("free flow speed (km/h)")
ggplot(smallSample, aes(x=station, y=HRS8.9avg /sf, group = sf, color = sf)) + geom_line(size = 1.5) + ylim(0,12500)+ xlab("length (m)") + ylab("volume at 8 h (vph)")
ggplot(smallSample, aes(x=station, y=HRS8.9avg/CAPACITY /sf, group = sf, color = sf)) + geom_line(size = 1.5)+ xlab("length (m)") + ylab("v/c ratio at 8 h")
ggplot(smallSample, aes(x=station, y=LENGTH/TRAVELTIME8.9avg*3.6, group = sf, color = sf)) + geom_line(size = 1.5) + ylim(0,125)+ xlab("length (m)") + ylab("speed at 8 h (km/h)")



#plots with x = sf
ggplot(smallSample, aes(x=sf, y=HRS8.9avg /sf, group = LINK, color = streetName)) + geom_line(size = 1.5) + geom_point(size = 3)
ggplot(smallSample, aes(x=sf, y=HRS8.9max /sf/CAPACITY, group = LINK, color = streetName)) + geom_line(size = 1.5)+ geom_point(size = 3)

ggplot(smallSample, aes(x=sf, y=LENGTH/TRAVELTIME8.9avg*3.6, group = LINK, color = streetName)) + geom_line(size = 1.5)+ geom_point(size = 3)

ggplot(smallSample, aes(x=sf, y=FREESPEED*3.6, group = LINK, color = streetName)) + geom_line(size = 1.5)+ geom_point(size = 3)