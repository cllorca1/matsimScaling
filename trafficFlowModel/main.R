pacman::p_load("dplyr", "reshape", "ggplot2", "data.table")

link_capacities = c("100.0","500.0","1000.0","2000.0")
link_lengths = c("50000.0","5000.0","1000.0","500.0","100.0")



#paper
link_capacities = c("750.0","1000.0","1250.0")
link_lengths = c("100.0","1000.0", "10000.0")
simulationNames = c("withHoles2.00-1.00","withHoles2.00-.95",
                    "withHoles2.00-.90","withHoles2.00-.85",
                    "withHoles2.00-.80", "withHoles2.00-.75",
                    "withHoles2.00-.70", "withHoles2.00-.65")
maxVehicle = 50000
scales = c("1.0" , "5.0" , "10.0" , "20.0" , "50.0" , "100.0")
#end paper


#paper
link_capacities = c("750.0","1000.0","1250.0")
link_lengths = c("100.0","1000.0", "10000.0")
simulationNames = c("withHoles1.00-1.00","withHoles1.00-.95",
                    "withHoles1.00-.90","withHoles1.00-.85",
                    "withHoles1.00-.80", "withHoles1.00-.75",
                    "withHoles1.00-.70", "withHoles1.00-.65")
maxVehicle = 1000
scales = c("1.0" , "5.0" , "10.0" , "20.0" , "50.0" , "100.0")
#end paper

#paper
link_capacities = c("750.0","1000.0","1250.0")
link_lengths = c("100.0","1000.0", "10000.0")
simulationNames = c("withHoles1.50-1.00","withHoles1.50-.95",
                    "withHoles1.50-.90", "withHoles1.50-.85",
                    "withHoles1.50-.80", "withHoles1.50-.75",
                    "withHoles1.50-.70", "withHoles1.50-.65")
maxVehicle = 1000
scales = c("1.0" , "5.0" , "10.0" , "20.0" , "50.0" , "100.0")
#end paper


link_capacities = c("700.0", "710.0", "720.0","730.0",
                    "740.0", "750.0","760.0","770.0","780.0","790.0","800.0")
link_lengths = c("1000.0")
simulationNames = c("testsSmallCapacity")
scales = c("100.0")
maxVehicle = 1


########################

link_capacities = c("750.0","1000.0","1250.0")
link_lengths = c("1000.0")
simulationNames = c("tests.50-1.00",
                    "tests.75-1.00",
                    "tests1.00-1.00",
                    "tests1.25-1.00",
                    "tests1.50-1.00",
                    "tests1.75-1.00",
                    "tests2.00-1.00",
                    "tests2.75-1.00",
                    "tests3.25-1.00",
                    "tests4.00-1.00")

maxVehicle = 1
scales = c("100.0")

################

link_capacities = c("750.0","1000.0","1250.0")
link_lengths = c("1000.0")
simulationNames = c("testsUpstream10K.50-1.00",
                    "testsUpstream10K1.00-1.00",
                    "testsUpstream10K1.50-1.00",
                    "testsUpstream10K2.00-1.00",
                    "testsUpstream10K2.50-1.00",
                    "testsUpstream10K3.00-1.00")

maxVehicle = 1
scales = c("100.0")

################

source("c:/code/matsimScaling/trafficFlowModel/analyzeTrafficFlowSimpleModel.R")

all = data.frame()

for (simulationName in simulationNames){
  summary = data.frame()
  
  for (link_length in link_lengths){
    for (capacity in link_capacities){
      folder = paste("C=",capacity,"/L=",link_length,"/",sep = "")
      run_data = try(analyzeLenght(folder,simulationName, as.numeric(link_length),60*24,maxVehicle, scales))
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
              speed = mean(speed),speed_ref = mean(speed_ref), count = sum(count))
  
  global_summary$length = as.numeric(global_summary$length)
  global_summary$capacity = as.numeric(global_summary$capacity)
  
  #until here different according to the exponent of the capacity
  
  global_summary$exponent = simulationName
  
  #all = data.frame()
  all = rbind(as.data.frame(all),as.data.frame(global_summary))
  
}

#test since something is not working
all$speed = all$length / all$tt * 3.6
all$speed_ref = all$length / all$tt_ref * 3.6

### temporary lines to aggregate different runs!
# all$factor = 2
# all_all = all

# all$factor = 1
# all_all = rbind(all_all, all)

all$factor = 1.5
all_all = rbind(all_all, all)



write.csv(file = "c:/models/file.csv", x = all_all, row.names = F)


#######

labs = c("L = 100 m", "L = 500 m", "L = 1,000 m", "L = 5,000 m","L = 10,000 m" )
names(labs) = c(100,500,1000,5000,10000)

labs2 = c("C = 1,000 veh/h" ,"C = 1500 veh/h", "C=1600 veh/h","C=1800 veh/h", "C = 2,000 veh/h", "C = 2,500 veh/h" )
names(labs2) = c(500,750, 800, 900, 1000,1250)

all$exponent[all$exponent=="withHoles2.00-1.00"] = 1
all$exponent[all$exponent=="withHoles2.00-.95"] = 0.95
all$exponent[all$exponent=="withHoles2.00-.90"] = 0.90 
all$exponent[all$exponent=="withHoles2.00-.85"] = 0.85
all$exponent[all$exponent=="withHoles2.00-.80"] = 0.80
all$exponent[all$exponent=="withHoles2.00-.75"] = 0.75
all$exponent[all$exponent=="withHoles2.00-.70"] = 0.70
all$exponent[all$exponent=="withHoles2.00-.65"] = 0.65


ggplot(all, aes(x=scale, y= tt/tt_ref*100, color = as.factor(exponent), group = as.factor(exponent))) +
  geom_point(size = 1) +
  geom_path(size = 1) + 
  facet_grid(capacity~length, labeller = labeller(length = labs, capacity = labs2)) +
  scale_x_log10() +
  xlab("Scaling factor (%) (log-scale)") + 
  ylab("Relative error of travel time \n respect of 100% simulation (%)") +
  theme_bw() + theme(legend.position = "bottom") + 
  geom_hline(yintercept = 100, size = 1) + coord_cartesian(ylim=c(0, 500)) + 
  labs(color = "Exponent of the storage capacity factor")
 

ggplot(all, aes(x=scale, y= length / tt*3.6, color = as.factor(exponent), group = as.factor(exponent))) +
  geom_path(size = 1) +
  geom_point(shape = 21, size = 3, fill = "white") +
  facet_grid(capacity~length, labeller = labeller(length = labs, capacity = labs2)) +
  xlab("Scaling factor (%)") + 
  ylab("Average speed (km/h)") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(color = "Exponent of the storage capacity factor") + 
  scale_color_brewer(palette = "Paired")


ggplot(all, aes(x=scale, y= speed/speed_ref*100,
                           color = as.factor(exponent), group = as.factor(exponent))) +
  geom_hline(yintercept = 100, size = 1) + 
  geom_path(size = 1) + 
  geom_point(shape = 21, size = 3, fill = "white") +
  facet_grid(capacity~length, labeller = labeller(length = labs, capacity = labs2)) +
  xlab("Scaling factor (%) (log-scale)") + 
  ylab("Relative error of speed \n respect of 100% simulation (%)") +
  theme_bw() + theme(legend.position = "bottom") + labs(color = "Link capacity (veh/h)") + 
  
  scale_color_brewer(palette = "Paired")
