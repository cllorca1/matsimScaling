setwd("C:/models/silo/muc/microData")

# install.packages("data.table")
# install.packages("dplyr")
# install.packages("ggplot2")


library(data.table)
library(dplyr)
library(ggplot2)

#read 2011 files
pp11 = fread("pp_2011.csv")
jj11 = fread("jj_2011.csv")

#read 2050 files
pp50 = fread("futureYears/pp_2050.csv")


jj50 = fread("futureYears/jj_2050.csv")
jj50 = jj50 %>% filter (personId > -1)

hh50 = fread("futureYears/hh_2050.csv")


#populationByCounty

source("c:/code/matsimScaling/siloAnalysis/readZonesAndRegions.R")

pp11 = merge(pp11, zonesWithRegionName, by.x = "homeZone", by.y = "zone")

aux = hh50 %>% select(id, homeZone = zone)
aux = merge(pp50, aux, by.x = "hhID", by.y = "id")
aux = merge(aux, zonesWithRegionName, by.x = "homeZone", by.y = "zone")

summary11 = pp11 %>% group_by(region = work_region_name) %>% summarize(count = n())
summary50 = aux %>% group_by(region = work_region_name) %>% summarize(count = n())

summary11$year = 2011
summary50$year = 2050

ggplot(rbind(summary11, summary50), aes(y=count, x = region, group = as.factor(year)
                                        , fill = as.factor(year))) +
  geom_bar(stat = "identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("population by region")



#2011
#filter to workers
pp11 = pp11 %>% filter(workplace>0)

#merge pp and jj to get workzones
pp11$jobMergeId = pp11$workplace
jj11$jobMergeId = jj11$id

ppjj11 = merge(pp11, jj11, by="jobMergeId")

#store workZone in the right field
ppjj11$workZone = ppjj11$zone

#check if there is only one worker or more than one in the hh
workersByhh11 = ppjj11 %>% group_by (hhid) %>% summarize (hhworkers = n())

#and assign this value to the ppjj11 table
ppjj11 = merge(ppjj11, workersByhh11, by="hhid")


#select only what need for comparison
ppjj11  = ppjj11 %>% select(homeZone, workZone, id.x, hhworkers) 

#2050

pp50 = pp50 %>% filter(workplace>0)
#merge pp and jj
pp50$jobMergeId = pp50$workplace
jj50$jobMergeId = jj50$id

ppjj50 = merge(pp50, jj50, by = "jobMergeId")

#store workZone in the right place
ppjj50$workZone = ppjj50$zone

#merge pp and hh
hh50$hhMergeId = hh50$id
ppjj50$hhMergeId = ppjj50$hhID

ppjjhh50 = merge(ppjj50, hh50, by = "hhMergeId")

#store homeZone in the right place
ppjjhh50$homeZone = ppjjhh50$zone.y


#check if there is only one worker or more than one in the hh
workersByhh50 = ppjjhh50 %>% group_by(hhID) %>% summarize (hhworkers = n())

#and assign this value to the ppjj11 table
ppjjhh50 = merge(ppjjhh50, workersByhh50, by="hhID")


#select the useful columns only
ppjjhh50  = ppjjhh50 %>% select(homeZone, workZone, id.x, hhworkers) 


#read zonal data
fileNameZones = "C:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(fileNameZones)

#plot jobs by location





#analyze 1-worker-hh

print(nrow(ppjj11))
print(nrow(ppjjhh50))

#calculate totals of employment by county
#run the script analyze_region_selection.R first

ppjj11 = merge(ppjj11, zonesWithRegionName, by.x = "workZone", by.y = "zone")
ppjjhh50 = merge(ppjjhh50, zonesWithRegionName, by.x = "workZone", by.y = "zone")

summary11 = ppjj11 %>% group_by(work_region_name) %>% summarize(count = n())
summary50 = ppjjhh50 %>% group_by(work_region_name) %>% summarize(count = n())
 
summary11$year = 2011
summary50$year = 2050

ggplot(rbind(summary11, summary50), aes(y=count, x = work_region_name, group = as.factor(year)
                                        , fill = as.factor(year))) +
  geom_bar(stat = "identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  ggtitle("employers by region - at the work location")





#Munich and Augsburg
counties = c(9162,9772)

for (county in counties){
  
  #filter to Munich == 9162  (if Ausgburg just change to 9772, otherwise look at zone data frame)
  zonesMunich = zones %>% filter (ID_county == county)
  inMunich = zonesMunich$Zone
  
  #subset SP files to people working in the selected area
  sp11Muc = ppjj11 %>% filter(workZone %in% inMunich)
  sp50Muc = ppjjhh50 %>% filter(workZone %in% inMunich)
  
  #merge with zone file
  zones$homeZone = zones$Zone
  
  sp11Muc = merge(sp11Muc, zones, by= "homeZone")
  sp50Muc = merge(sp50Muc, zones, by= "homeZone")
  
  #group by home zone - here jur_name
  
  list11 = sp11Muc %>% group_by(JUR_NAME) %>% summarize(workers = n())
  list50 = sp50Muc %>% group_by(JUR_NAME) %>% summarize(workers = n())
  
  #add year column
  list11$year = 11
  list50$year = 50
  
  #join years
  list = rbind(list11, list50)
  
  #plot
  print(ggplot(list, aes(x=as.factor(JUR_NAME), y=workers,group = year, fill = as.factor(year))) +
          geom_bar(stat = "identity", position=position_dodge()) +
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          ggtitle(paste("home location of workers of ",county,sep="")) + 
          scale_y_continuous(labels = scales::comma)
  )
}

