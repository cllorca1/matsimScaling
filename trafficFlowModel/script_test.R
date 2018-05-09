library(ggplot2)
library(dplyr)

data = data.frame()

flows = c(200, 1000, 2000)
speed = 50


scales = c(0.01, 0.05, 0.1, 0.2, 0.5)

for (flow in flows){
  for(scale in scales){
      minHeadway = 3600/flow
      id = 0
      currentVehicleTime = 0
      time = 0
      aux = data.frame()
      while (time < 24*3600){
        time = time + minHeadway
        id = id + 1
        if (runif(1) < scale){
          selected = 1
          headway = time - currentVehicleTime
          currentVehicleTime = time
          aux = rbind(aux, data.frame(time = time, headway = headway, scale = scale, flow = flow))
        } else {
        }
      }
    
    message = paste("scale", scale, "flow", flow, sep = " ")
    print(message)
    data = rbind(data,aux)
  }
}
# actualScale = nrow(data) / flow / 24 
# meanHeadway = mean(data$headway)

bin = 1*3600
time_bins = seq(0,24*3600,bin)

data = data %>% mutate(time_interval = cut(time,breaks = time_bins))
aggregated = data %>% group_by(flow,scale, time_interval) %>% summarize(count = n())
aggregated = aggregated %>% mutate(actualFlow = count / scale )

ggplot(aggregated, aes(x= scale, y=actualFlow, group = as.factor(scale))) +
  stat_boxplot() + facet_grid(as.factor(flow)~., scales = "free_y")


aggregated = aggregated %>% filter(!is.na(time_interval)) %>%
  group_by(flow,scale) %>% summarize(average = mean(actualFlow), sd = sd(actualFlow))

ggplot(aggregated, aes(x=scale, y = average, group = as.factor(flow), color = as.factor(flow))) + 
  geom_path() + geom_point() 

ggplot(aggregated, aes(x=scale, y = sd/average, group = as.factor(flow), color = as.factor(flow))) + 
  geom_path() + geom_point()

ggplot(aggregated, aes(x=scale, y = average, ymin = average - sd,
                       ymax = average +sd, fill= as.factor(flow), group = as.factor(flow)
                       )) + 
  geom_path(color = as.factor(flow), size = 1) + 
  geom_ribbon(alpha = 0.1)

