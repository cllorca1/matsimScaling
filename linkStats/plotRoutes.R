
##r scipt to show the routes

library(ggplot2)
library(dplyr)
library(png)
setwd("C:/projects/MATSim/scaling/analysis/links")

dataAll = read.csv(file="dataAllLinks.csv")

listOfFileNames = c("a99Route.csv", "feld2Perl.csv", "a8Route.csv", "MUC2Graf.csv", "hbf2MunFre.csv")


colorsForPlot = c("#FFD700" ,"#a1dab4", "#41b6c4" ,"#2c7fb8", "#253494", "#000000")

for(fileName in listOfFileNames){
  
  print(fileName)  
  
  routeTable = read.csv(fileName)
  
  
  listOfLinks = routeTable$links
  listOfLinkNames = routeTable$order
  
  
  smallSample = subset(dataAll, LINK %in% listOfLinks)
  smallSample$streetName = listOfLinkNames[match(smallSample$LINK, listOfLinks)]
  
  ##to get the station
  listOfStations = list()
  pk = 0
  for (i in 1:length(listOfLinks)){
    pk = pk + smallSample$LENGTH[(match(listOfLinks[i],smallSample$LINK))]
    listOfStations[i] = pk
  }
  smallSample$station = as.numeric(listOfStations[match(smallSample$LINK, listOfLinks)])
  
   
  
  
  #plots with x=station
  print(ggplot(smallSample, aes(x=station, y=CAPACITY, group = as.factor(sf), color = "static")) 
        + geom_line(size = 1.5)  + xlab("length (m)") + ylab("capacity (vph)")
        + theme_light()
        #+ ylim(0,12500)
          )
  print(ggplot(smallSample, aes(x=station, y=FREESPEED*3.6, group = as.factor(sf), color = "static")) 
        + geom_line(size = 1.5)  + xlab("length (m)") + ylab("free flow speed (km/h)")
        + theme_light()
        + ylim(0,125)
        )
  print(ggplot(smallSample, aes(x=station, y=HRS8.9avg /sf, group = as.factor(sf), color = as.factor(sf)))
        + geom_line(size = 1.5) + xlab("length (m)") + ylab("volume at 8 h (vph)") 
        + scale_colour_manual(values = colorsForPlot) + theme_light()
        #+ ylim(0,12500)
        )
  print(ggplot(smallSample, aes(x=station, y=HRS8.9avg/CAPACITY /sf, group = as.factor(sf), color = as.factor(sf)))
        + geom_line(size = 1.5)+ xlab("length (m)") + ylab("v/c ratio at 8 h")
        + scale_colour_manual(values = colorsForPlot) + theme_light())
  print(ggplot(smallSample, aes(x=station, y=LENGTH/TRAVELTIME8.9avg*3.6, group = as.factor(sf), color = as.factor(sf))) 
        + geom_line(size = 1.5) + xlab("length (m)") + ylab("speed at 8 h (km/h)")
        + scale_colour_manual(values = colorsForPlot) + theme_light()
        + ylim(0,125)
        )
  
  
  
  
  
}
