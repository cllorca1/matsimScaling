library(dplyr)
library(data.table)
library(ggplot2)


#read zonal data
fileNameZones = "C:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(fileNameZones) %>% select(zone = Zone,region_name = JUR_NAME)

fileRegion = "C:/models/silo/muc/input/regionDefinition.csv"
zonesWithRegion = read.csv(fileRegion) %>% select(zone = Zone, region_id = Region)


zones = merge(x= zones, y= zonesWithRegion, by = "zone")


regions = zones %>% select(region_name, region_id) %>% distinct()
zonesWithRegionName = zones %>% select(zone, work_region_name = region_name)