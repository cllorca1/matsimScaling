library(dplyr)
library(data.table)
library(ggplot2)


source("siloAnalysis/readZonesAndRegions.R")

#read relocation data

path = "C:/projects/SILO/integrationTests/integration_full_20180623/"
file_name = "listOfSelectedRegions.csv"

data = fread(paste(path,file_name, sep = ""))
data = merge(data, regions, by.x = "region", by.y = "region_id")


data = merge(data, zonesWithRegionName, by.x= "workZone", by.y = "zone")

commutingDistances = data %>%
  group_by(region_name,year) %>%
  summarize(time = mean(timeRegionToWork)) %>%
  tidyr::spread(year, time)


commutingFlows = data %>%
  group_by(year,region_name,work_region_name) %>%
  summarize(n = n(), time = mean(timeRegionToWork)) 


regionsToAnalyze = c(2, 5, 7)

for (thisRegionIndex in regionsToAnalyze){
  
  thisRegionName = as.character((regions %>% filter(region_id == thisRegionIndex))$region_name)
  commutingFlowsThisRegion = commutingFlows %>% filter(region_name == thisRegionName)
  
  distancesFromThisRegion = commutingFlowsThisRegion %>% group_by(work_region_name) %>% summarize(time = mean(time))
  levels = distancesFromThisRegion$work_region_name[order(distancesFromThisRegion$time)]
  
  commutingFlowsThisRegion$work_region_name = 
    factor(commutingFlowsThisRegion$work_region_name, levels = levels)
  
  plot1 = ggplot(commutingFlowsThisRegion, aes(x=work_region_name, y=n, color = year )) + 
    geom_point() + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
    ggtitle(paste("count of workers living in",thisRegionName))
  
  plot2 = ggplot(commutingFlowsThisRegion, aes(x=work_region_name, y=time, color = year )) + 
    geom_point() + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
    ggtitle(paste("average time (regional) of workers living in",thisRegionName))
  
  print(plot1)
  print(plot2)
  
}

