pacman::p_load(data.table, dplyr, ggplot2)

path = "c:/projects/SILO/integrationTests/integration_full_20180623/mitoOutput/"
years = c(2012,2030,2040)
fileEnding = "/trips.csv"

allYears = data.frame()

for (year in years){
  dataThisYear = fread(paste(path,year,fileEnding, sep = ""))
  summary = dataThisYear %>% group_by(purpose, mode) %>% summarize(count = n(),
                                                                   distance = mean(distance),
                                                                   time = mean(time_auto))
  summary$year = year
  
  summary[is.null(summary)] = "null"
  summary[is.na(summary)] = 0
  summary = as.data.frame(summary)
  allYears = as.data.frame(rbind(allYears,summary))
}







  

