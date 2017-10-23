setwd("C:/models/silo/muc/microData")

install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")


library(data.table)
library(dplyr)
library(ggplot2)

#read 2011 files
pp11 = fread("pp_2011.csv")
jj11 = fread("jj_2011.csv")

#filter to workers
pp11 = pp11 %>% filter(workplace>0)

#merge pp and jj to get workzones
pp11$jobMergeId = pp11$workplace
jj11$jobMergeId = jj11$id

ppjj11 = merge(pp11, jj11, by="jobMergeId")

#store workZone in the right field
ppjj11$workZone = ppjj11$zone

#select only what need for comparison
ppjj11  = ppjj11 %>% select(homeZone, workZone, id.x) 


#read 2050 files
pp50 = fread("pp_2050.csv")
pp50 = pp50 %>% filter(workplace>0)

jj50 = fread("jj_2050.csv")
jj50 = jj50 %>% filter (personId > -1)

hh50 = fread("hh_2050.csv")

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

#select the useful columns only
ppjjhh50  = ppjjhh50 %>% select(homeZone, workZone, id.x) 


#read zonal data
fileNameZones = "C:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(fileNameZones)

#plot jobs by location


#filter to Munich == 9162  (if Ausgburg just change to 9772, otherwise look at zone data frame)
zonesMunich = zones %>% filter (ID_county == 9162)
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
ggplot(list, aes(x=as.factor(JUR_NAME), y=workers,group = year, color = as.factor(year))) +
  geom_path() +  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("home location of Rosenheim workers (uses MATSim)") + 
  scale_y_continuous(labels = scales::comma)

