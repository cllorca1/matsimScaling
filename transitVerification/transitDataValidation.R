library(ggplot2)

setwd("C://models/transit/output/check")

stopOrderData = read.csv("stopOrder.csv")


ggplot(stopOrderData, aes(x=onLineDist, y = eucDist, color = lineId, group = lineId)) + geom_line() + xlim (0,0.5) + ylim (0,0.5)

