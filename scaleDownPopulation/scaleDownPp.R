setwd("C:/models/silo/muc/microData")

library(data.table)
library(dplyr)

scaleFactor = 0.01
expansionFactor = 100


#read 2011 full-size files
pp11 = fread("pp_2011.csv")
jj11 = fread("jj_2011.csv")
hh11 = fread("hh_2011.csv")
hh1dd11 = fread("dd_2011.csv")

numberOfMarriedByHh = pp11 %>% filter(relationShip == "married") %>%  group_by(hhid) %>% summarize(count = n())
summary(as.factor(numberOfMarriedByHh$count))


vacant_dd11 = dd11 %>% filter(hhID == -1)
vacant_jj11 = jj11 %>% filter(personId == -1)

hh11 = hh11 %>% rowwise() %>% mutate (random = as.integer(sample(1:expansionFactor, size=1)))

hh11_subset = hh11 %>% filter(random == 1)

listOfHouseholds = hh11_subset$id

pp11_subset = pp11 %>% filter(hhid %in% listOfHouseholds)
dd11_subset_filled = dd11 %>% filter(hhID %in% listOfHouseholds)

listOfPersons = pp11_subset$id

jj11_subset_filled = jj11 %>% filter(personId %in% listOfPersons)

#add vacants randomly

vacant_dd11 = vacant_dd11 %>%
  rowwise() %>%
  mutate (random = as.integer(sample(1:expansionFactor, size=1)))
vacant_jj11 = vacant_jj11 %>%
  rowwise() %>%
  mutate (random = as.integer(sample(1:expansionFactor, size=1)))

#double of vacants is added... 
vacant_dd11_subset = vacant_dd11 %>% filter(random < 3)
vacant_jj11_subset = vacant_jj11 %>% filter(random < 3)


vacant_dd11_subset = vacant_dd11_subset %>% select(1:(ncol(vacant_dd11_subset) - 1))
vacant_jj11_subset = vacant_jj11_subset %>% select(1:(ncol(vacant_jj11_subset) - 1))


jj11_subset = rbind(jj11_subset_filled, vacant_jj11_subset)
dd11_subset = rbind(dd11_subset_filled, vacant_dd11_subset)


hh11_subset = hh11_subset %>% select(1:(ncol(hh11_subset)-1))

#write out files
fwrite(pp11_subset,"ppS_2011.csv")
fwrite(hh11_subset,"hhS_2011.csv")
fwrite(dd11_subset,"ddS_2011.csv")
fwrite(jj11_subset,"jjS_2011.csv")



