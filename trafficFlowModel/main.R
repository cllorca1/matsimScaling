pacman::p_load("dplyr", "reshape", "ggplot2", "data.table")

link_capacities = c("100.0","500.0","1000.0","2000.0")
link_lengths = c("50000.0","5000.0","1000.0","500.0","100.0")


link_capacities = c("500.0","1000.0")
link_lengths = c("100.0","1000.0", "5000.0")
simulationNames = c("withHoles","withHoles0.95","withHoles0.90","withHoles0.85", "withHoles0.75")

source("c:/code/matsimScaling/trafficFlowModel/analyzeTrafficFlowSimpleModel.R")

all = data.frame()

for (simulationName in simulationNames){
  summary = data.frame()
  
  for (link_length in link_lengths){
    for (capacity in link_capacities){
      folder = paste("C=",capacity,"/L=",link_length,"/",sep = "")
      run_data = try(analyzeLenght(folder,simulationName, as.numeric(link_length),60,50000))
      # tt_ref = (run_data %>% filter(scale == 100) %>% select(tt))$tt
      # run_data$tt_ref = tt_ref
      # speed_ref = (run_data %>% filter(scale == 100) %>% select(speed))$speed
      # run_data$speed_ref = speed_ref
      if(is.data.frame(run_data)){
        run_data$length = link_length
        run_data$capacity = capacity
        summary = rbind(as.data.frame(run_data), as.data.frame(summary))
        rm(run_data)
      }
      
    }
  }
  
  #Obtain the reference values at 100% simulation
  tts_ref = summary %>% filter(scale == 100) %>%
    select(time_interval, length, capacity, tt_ref = travelTime, speed_ref = speed)
  
  summary = merge(summary, tts_ref, by = c("time_interval", "length", "capacity"))
  summary = summary[order(summary$length, summary$capacity, summary$scale, summary$time_interval),]
  
  
  # #plot vs. time
  # plot = ggplot(summary %>% filter(), 
  #               aes(x=as.numeric(time_interval), y=speed, 
  #                   color = as.factor(scale),fill = as.factor(scale))) + 
  #   geom_line(size = 1) +
  #   facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length))) +
  #   theme_bw() + 
  #   xlab("Time of day (hour)") + ylab("Speed (km/h)")
  # 
  # print(plot)
  # 
  # plot = ggplot(summary %>% filter(scale != 20, scale != 100), aes(x=as.numeric(time_interval),
  #                                                                  y=travelTime, 
  #                                                                  color = as.factor(scale),
  #                                                                  fill = as.factor(scale))) + 
  #   geom_line(size = 1) +
  #   facet_grid(as.factor(as.numeric(capacity))~as.factor(as.numeric(length)), scales = "free_y") +
  #   scale_colour_manual(values = c("red", "darkorange", "brown", "black"))  +
  #   scale_fill_manual(values = c("red", "darkorange", "brown", "black")) + 
  #   theme_bw() + 
  #   xlab("Time of day (hour)") + ylab("Travel time")
  # 
  # print(plot)
  
  
  global_summary = summary %>%
    filter(as.numeric(time_interval) > 10) %>%
    group_by(scale, length, capacity) %>%
    summarize(tt = mean(travelTime), tt_ref = mean(tt_ref), 
              speed = mean(speed),speed_ref = mean(speed_ref))
  
  global_summary$length = as.numeric(global_summary$length)
  
  #until here different according to the exponent of the capacity
  
  global_summary$exponent = simulationName
  
  #all = data.frame()
  all = rbind(as.data.frame(all),as.data.frame(global_summary))
  
}

labs = c("L = 100 m", "L = 500 m", "L = 1,000 m", "L = 5,000 m","L = 50,000 m" )
names(labs) = c(100,500,1000,5000,50000)

labs2 = c("C = 1,000 veh/h" , "C = 2,000 veh/h" )
names(labs2) = c("500.0","1000.0")


ggplot(all, aes(x=scale, y= tt/tt_ref*100, color = as.factor(exponent), group = as.factor(exponent))) +
  geom_point(size = 1) +
  geom_path(size = 1) + 
  facet_grid(capacity~length, labeller = labeller(length = labs, capacity = labs2)) +
  scale_x_log10() +
  xlab("Scaling factor (%) (log-scale)") + 
  ylab("Relative error of travel time \n respect of 100% simulation (%)") +
  theme_bw() + theme(legend.position = "bottom") + 
  geom_hline(yintercept = 100, size = 1) + coord_cartesian(ylim=c(0, 500))
 

ggplot(all, aes(x=scale, y= speed/speed_ref*100,
                           color = as.factor(exponent), group = as.factor(exponent))) +
  geom_point(size = 2) +
  geom_path(size = 1) + 
  facet_grid(capacity~length, labeller = labeller(length = labs, capacity = labs2)) +
  xlab("Scaling factor (%) (log-scale)") + 
  ylab("Relative error of speed \n respect of 100% simulation (%)") +
  theme_bw() + theme(legend.position = "bottom") + labs(color = "Link capacity (veh/h)") + 
  geom_hline(yintercept = 100, size = 1)
