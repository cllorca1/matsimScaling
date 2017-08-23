

library(ggplot2)

setwd("C:/projects/MATSim/scaling/analysis/scoring")

score = read.csv("scoring.csv")
score$grouping = as.factor(paste(score$network,score$scale,sep="."))

score$network = as.factor(score$network)
score$scale = as.factor(score$scale)

ggplot(score, aes(x=ITERATION, y=avg..AVG, group = grouping, color = scale, linetype = network))+
  geom_line(size = 1)+
  theme_light()+
  xlab("iteration")+
  ylab("average MATSim plan score (points)")
