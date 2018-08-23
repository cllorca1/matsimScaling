pacman::p_load(data.table, dplyr, ggplot2)

path = "c:/projects/SILO/integrationTests/integration_full_20180707/mitoOutput/"
#path = "c:/models/silo/muc/mitoOutput/"
years = c(2024,2037,2050)
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


#put columns in order # to paste in Excel
allYears = allYears %>% select(purpose, mode, count, distance, year, time)

#calculate modal shares by distance
distanceBins = seq(0,50,2)

for (year in years){
  dataThisYear = fread(paste(path,year,fileEnding, sep = ""))
  dataThisYear$distance_bin = cut(x=dataThisYear$distance, breaks = distanceBins)

  summaryThisYear = dataThisYear %>% group_by(mode, distance_bin) %>% summarize(trips = n())
  total_trips = nrow(dataThisYear)
  
  summaryThisYear = summaryThisYear %>% filter(mode != "null")
  
  print(
    ggplot() + 
      geom_area(data = summaryThisYear, aes(x=distance_bin, y=trips, group = as.factor(mode), fill = as.factor(mode)), position = "fill") + 
      ggtitle(paste("Shares by distance in",year)) + 
      scale_fill_manual(values = c("#64A0C8", "#003359", "#A5A5A5", "#FFC000","#E37222", "#74390F", "#92D050" ))
  )
  print(
    ggplot(dataThisYear %>% filter(mode == "autoDriver"), aes(x=distance, color = as.factor(purpose))) +
      geom_freqpoly(size = 1.5) + 
      ggtitle(paste("Distance distribution by purpose for car trips in",year))
  )
  print(
    ggplot(dataThisYear %>% filter(mode == "autoDriver"), aes(x=distance)) +
      geom_freqpoly(size = 1.5) +
      ggtitle(paste("Distance distribution for car trips in",year))
  )
}


