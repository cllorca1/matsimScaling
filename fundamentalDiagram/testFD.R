library(ggplot2)
library(data.table)
library(dplyr)
library(reshape)

setwd("C:/models/mito/muc/mitoMunich/output/trafficAssignment/output/ITERS/it.50")

data = read.csv("mito_assignment.50.linkstats.txt.gz", sep = "\t")

scaleFactor = 0.05


# data$q_max = 13.5/3.6 * data$FREESPEED * 1/7.5 / (13.5/3.6 + data$FREESPEED) * 3600
# 
# 
# 
# #ggplot(data, aes(x=CAPACITY, y=q_max)) + 
# # geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")
# 
# 
# names(data)
# 
# hourlyVolumesMax = data %>% select(1:7,155,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79)
# 
# hourlyVolumesMax$maxVolume = apply(hourlyVolumesMax[,9:32],1, FUN = max)
# 
# 
# ggplot(hourlyVolumesMax, aes(x = CAPACITY*scaleFactor, y=maxVolume)) + 
#   geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")
# 
# ggplot(hourlyVolumesMax, aes(x = q_max, y=maxVolume*20)) + 
#   geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")


hourlyVolumesMax = data %>% select(1:7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79)
travelTimesMin = data %>% select(1:7,83,86,89,92,95,98,101,104,107,110,113,116,119,122,125,128,131,134,137,140,143,146,149,152)

melted1 = melt(hourlyVolumesMax, id.vars = 1:7)
melted2 = melt(travelTimesMin, id.vars = 1:7)

speed = 120/3.6

melted1 = melted1 %>%
  filter(FREESPEED == speed) %>%
  select(LINK, FREESPEED, CAPACITY, LENGTH, hour =variable, volume = value)
melted1$hour = as.integer(melted1$hour)
melted2 = melted2 %>% filter(FREESPEED == speed) %>% select(LINK, hour =variable, travelTime = value)
melted2$hour = as.integer(melted2$hour)

meltedAll = merge(x=melted1, y=melted2, by = c("LINK","hour"))
meltedAll$speed = meltedAll$LENGTH/meltedAll$travelTime
meltedAll$density = meltedAll$volume/ meltedAll$speed / 3600
maxDensity = 1/7.5 *scaleFactor ^ 0.75
meltedAll$rel_density = meltedAll$density/ maxDensity
meltedAll$q_max = 13.5/3.6 * meltedAll$FREESPEED * maxDensity / (13.5/3.6 + meltedAll$FREESPEED)
meltedAll$vc = meltedAll$volume / scaleFactor / meltedAll$CAPACITY

ggplot(meltedAll, aes(x=vc)) + geom_histogram()
summary(meltedAll$vc)

toPlot =  meltedAll %>% filter(LENGTH > 100)


ggplot(toPlot, aes(x=density, y=volume/3600)) + geom_point() + 
  geom_abline(intercept = 0, slope = speed, color = "green") +
  geom_abline(intercept = maxDensity*13.5/3.6, slope = -13.5/3.6, color = "red") + 
  ylab("volume in veh/s") + 
  xlab("density in veh/m") + 
  ylim(0,0.1) + xlim(0,maxDensity)

ggplot(toPlot, aes(x=rel_density, y=vc)) +
  geom_point() 
