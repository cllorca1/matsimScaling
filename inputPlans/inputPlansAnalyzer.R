library(data.table)
library(dplyr)
library(ggplot2)


folder = "c:/models/munich/sp"

setwd(folder)

#read trips by mode
data0 = fread("plansAuto.csv")
data0$mode = "auto"

data1 = fread("plansWalk.csv")
data1$mode = "walk"

data2 = fread("plansCycle.csv")
data2$mode = "cycle"

data3 = fread("plansTransit.csv")
data3$mode = "transit"


#comments: 
#modes: 0:auto, 1:walk, 2: bicycle, 3: transit.
#mode choice model based on trip distance - do not fit overall modal shares
#purposes: h-w home to work; w-h work to home, h-o home to other, o-h other to home.
#for auto --> each line is a trip of one vehicles - not persons trips. This uses car occupancies (average) from MiD. 
#car occupancies: 1.13 for work, 2.76 for other


#for other modes --> each line is a person trip.

#trips with travel distance > 80 km are discarded 




trips = rbind(data0, data1, data2, data3)


trips %>% group_by(mode) %>% summarise(count = n(), distance = mean(travelDistance))
ggplot(trips, aes(x=travelDistance, color = as.factor(mode))) + stat_ecdf() + xlim(0,100e3)



