pacman::p_load("dplyr", "reshape", "ggplot2", "data.table")

link_capacities = c("100.0","500.0","1000.0","2000.0")
link_lengths = c("50000.0","5000.0","1000.0","500.0","100.0")

source("c:/code/matsimScaling/trafficFlowModel/analyzeTrafficFlowSimpleModel.R")

summary = data.frame()

for (link_length in link_lengths){
  for (capacity in link_capacities){
  folder = paste("C=",capacity,"/L=",link_length,"/",sep = "")
    run_data = analyzeLenght(folder, as.numeric(link_length),60,50000)
  # tt_ref = (run_data %>% filter(scale == 100) %>% select(tt))$tt
  # run_data$tt_ref = tt_ref
  # speed_ref = (run_data %>% filter(scale == 100) %>% select(speed))$speed
  # run_data$speed_ref = speed_ref
  run_data$length = link_length
  run_data$capacity = capacity
  summary = rbind(run_data, summary)
  }
}


tts_ref = summary %>% filter(scale == 100) %>% select(time_interval, length, capacity, tt_ref = travelTime, speed_ref = speed, vehicles_ref = vehicles)

summary = merge(summary, tts_ref, by = c("time_interval", "length", "capacity"))

summary = summary[order(summary$length, summary$capacity, summary$scale, summary$time_interval),]

plot = ggplot(summary %>% filter(scale != 20, scale != 100), aes(x=as.numeric(time_interval), y=speed -speed_ref, color = as.factor(scale),fill = as.factor(scale))) + 
  geom_line(size = 1) +
  facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length))) +
  scale_colour_manual(values = c("red", "darkorange", "brown", "black"))  +
  scale_fill_manual(values = c("red", "darkorange", "brown", "black")) + 
  theme_bw() + 
  xlab("Time of day (hour)") + ylab("Speed difference respect of 100% simulation (km/h)")

print(plot)

plot = ggplot(summary %>% filter(scale != 20, scale != 100), aes(x=as.numeric(time_interval), y=(travelTime -tt_ref)/tt_ref, color = as.factor(scale),fill = as.factor(scale))) + 
  geom_line(size = 1) +
  facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length)), scales = "free_y") +
  scale_colour_manual(values = c("red", "darkorange", "brown", "black"))  +
  scale_fill_manual(values = c("red", "darkorange", "brown", "black")) + 
  theme_bw() + 
  xlab("Time of day (hour)") + ylab("Travel time difference respect of 100% simulation (relative)")

print(plot)

plot = ggplot(summary %>% filter(scale != 20, scale != 100), aes(x=as.numeric(time_interval), y=(vehicles/scale * 100 - vehicles_ref) / vehicles_ref, color = as.factor(scale), fill = as.factor(scale))) + 
  geom_line(size = 1) +
  facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length))) +
  scale_colour_manual(values = c("red", "darkorange", "brown", "black")) + 
  scale_fill_manual(values = c("red", "darkorange", "brown", "black")) + 
  theme_bw() + 
  xlab("Time of day (hour)") + ylab("Relative error in the number of vehicles \n in the link, respect to the 100% simulation (%)")

print(plot)

plot = ggplot(summary %>% filter(scale != 20, scale != 100), aes(x=as.numeric(time_interval), y=(vehicles/scale * 100 - vehicles_ref), color = as.factor(scale), fill = as.factor(scale))) + 
  geom_line(size = 1) +
  facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length)), scales = "free_y") +
  scale_colour_manual(values = c("red", "darkorange", "brown", "black")) + 
  scale_fill_manual(values = c("red", "darkorange", "brown", "black")) + 
  theme_bw() + 
  xlab("Time of day (hour)") + ylab("Absolute error in the number of vehicles \n in the link, respect to the 100% simulation (vehicles)")

print(plot)



# ggplot(summary, aes(x=scale, y=sqrt((tt - tt_ref)^2/6)/tt_ref , color = as.factor(length))) +
  # geom_path() + geom_point() + xlab("scale") + ylab("travel time relative error")



