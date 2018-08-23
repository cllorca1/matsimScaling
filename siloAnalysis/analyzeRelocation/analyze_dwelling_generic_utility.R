library(dplyr)
library(data.table)
library(ggplot2)


#source("siloAnalysis/readZonesAndRegions.R")

#read dwelling generic utilities

path = "C:/models/silo/muc/"
file_name = "genericUtilitiesForDwellings.csv"

data = fread(paste(path,file_name,sep = ""))


ggplot(data, aes(x=as.factor(hhType), y = genericUtil)) + geom_point()
ggplot(data, aes(x=price, y = genericUtil)) + geom_point()


ggplot(data, aes(x=price, y = genericUtil, color = as.factor(quality))) + 
  geom_point() + facet_grid(.~hhType) + facet_wrap(facets = "hhType", ncol = 4) + xlim(0,5000)
