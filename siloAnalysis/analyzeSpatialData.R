pacman::p_load(data.table, ggplot2, dplyr)

#read spatial data (readable) with year as a column

path = "c:/projects/SILO/integrationTests/integration_full_skim_20180622/integration_full_20180621/"
fileName = "spatialResultsReadable.csv"

spatialData = read.csv(paste(path,fileName,sep = ""))


source("siloAnalysis/readZonesAndRegions.R")

spatialData = merge(spatialData, zones, by = "zone")

regionalData = spatialData %>% group_by(year,region_id, region_name) %>%
  summarize(pp = sum(population), hh = sum(households), dd = sum(dd_SFD) + sum(dd_SFA) + sum(dd_MF234) + sum(dd_MF5plus),
            availableLand = sum(availLand), price = mean(avePrice), jobs = sum(jobs))



ggplot(regionalData, aes(x=year, y=pp, group = as.factor(region_id), color=region_name, size = pp)) +
  geom_path()

ggplot(regionalData, aes(x=year, y=pp)) +
  geom_bar(stat = "sum")

ggplot(regionalData, aes(x=year, y=hh, group = as.factor(region_id), color=region_name)) + geom_path()
ggplot(regionalData, aes(x=year, y=availableLand, group = as.factor(region_id), color=region_name)) +
  geom_path()


ggplot(regionalData, aes(x=year, y=price, group = as.factor(region_id), color=region_name, size = pp)) +
  geom_path()


ggplot(regionalData, aes(x=year, y=dd, group = as.factor(region_id), color=region_name)) + geom_path()


ggplot(regionalData, aes(x=year, y=hh/dd, group = as.factor(region_id), color=region_name)) + geom_path()
