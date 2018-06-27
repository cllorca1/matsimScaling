
setwd("c:/models/mito/muc/mitoMunich/microData")

library(data.table)
library(dplyr)

scaleFactor = 0.01
expansionFactor = 100


#read 2011 full-size files
pp11 = fread("pp_2011.csv")
jj11 = fread("jj_2011.csv")
hh11 = fread("hh_2011.csv")


hh11 = hh11 %>% rowwise() %>% mutate (random = as.integer(sample(1:expansionFactor, size=1)))

hh11_subset = hh11 %>% filter(random == 1)

listOfHouseholds = hh11_subset$id

pp11_subset = pp11 %>% filter(hhid %in% listOfHouseholds)

listOfPersons = pp11_subset$id

jj11_subset_filled = jj11 %>% filter(personId %in% listOfPersons)

fwrite(pp11_subset,"ppS_2011.csv")
fwrite(hh11_subset,"hhS_2011.csv")
fwrite(jj11_subset_filled,"jjS_2011.csv")
