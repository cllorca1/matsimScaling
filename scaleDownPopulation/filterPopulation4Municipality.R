setwd("C:/models/silo/muc/microData")

# install.packages("data.table")
# install.packages("dplyr")
# install.packages("ggplot2")


library(data.table)
library(dplyr)
library(ggplot2)

#read 2011 files
pp11 = fread("pp_2011.csv")
hh11 = fread("hh_2011.csv")


hh11_new = hh11 %>% filter(zone == 1257 |zone == 1251|zone == 1258|zone == 1253 )
p11_new = pp11 %>% filter(homeZone == 1257 |homeZone == 1251|homeZone == 1258|homeZone == 1253)


write.csv(p11_new, "persons.csv")
write.csv(hh11_new, "households.csv")
