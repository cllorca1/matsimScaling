folder = "c:/projects/MATSim/scaling/analysis/ttDistribution/"

data = read.csv(paste(folder,"averageTTs.csv", sep = ""))



ggplot(data %>% filter(storEcp == 0.75, it == 50), aes(x=100*scale, y=tt/60, linetype = as.factor(network))) + 
  geom_path(size = 1) +
  geom_point(shape = 21, size = 3, fill = "white") +
  ylab("Average travel time in minutes") + xlab("Scale factor (%)") + 
  labs(linetype = "Network")  + 
  theme_bw() + theme(legend.position = "bottom")


ggplot(data %>% filter(it == 50, network == "coarse"), aes(x=100*scale, y=tt/60,
                                                                  color = as.factor(storEcp))) + 
  
  labs(color = "Exponent of the \nstorage capacity \nfactor") + 
  scale_color_manual(values = c("black", "green", "blue")) + 
  geom_path(size = 1) +
  geom_point(shape = 21, size = 3, fill = "white") +
  ylab("Average travel time (min)") +
  xlab("Scale factor (%)") + 
  theme_bw() + theme(legend.position = "bottom")



ggplot(data %>% filter(storEcp == 0.75, network == "coarse"), aes(x=100*scale, y=tt/60,
                                                           size = as.factor(it))) + 
  scale_size_manual(values= c(1,1.5,4)) + 
  geom_path() +
  geom_point(shape = 21, size = 3, fill = "white") +
  ylab("Average travel (min)") + xlab("Scale factor (%)") + 
  labs(size = "Iterations") +   
  theme_bw() + theme(legend.position = "bottom")
