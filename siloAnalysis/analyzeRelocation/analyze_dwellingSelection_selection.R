library(dplyr)
library(data.table)
library(ggplot2)


#source("siloAnalysis/readZonesAndRegions.R")

#read dwelling generic utilities

path = "C:/models/silo/muc/"
file_name = "dwellingSelection.csv"

data = fread(paste(path,file_name,sep = ""))

years = c(0,2025)

for (year in years){
data1Year = data %>% filter(year == 0) 
print(ggplot(data1Year, aes(x=sum_commuting_time, y=genericUtility)) +
        geom_point(alpha = 0.1) + 
        ggtitle(year) + 
        xlab("sum of commuting time in the household") + 
        ylab("generic utilitiy for this hhType and dd"))
print(ggplot(data1Year, aes(x=sum_commuting_time, y=workDistanceUtility)) +
        geom_point(alpha = 0.1) + 
        ggtitle(year) + 
        xlab("sum of commuting time in the household") + 
        ylab("multiplicativeFactor"))
print(ggplot(data1Year, aes(x=sum_commuting_time, y=utility)) +
        geom_point(alpha = 0.1) + 
        ggtitle(year) + 
        xlab("sum of commuting time in the household") + 
        ylab("final utilitiy for choice of dwelling or hh satisfaction"))

}
