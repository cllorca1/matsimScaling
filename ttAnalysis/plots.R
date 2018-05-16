folder = "c:/projects/MATSim/scaling/analysis/ttDistribution/"

data = read.csv(paste(folder,"averageTTs.csv", sep = ""))



ggplot(data %>% filter(storEcp == 0.75, it == 50), aes(x=100*scale, y=tt/60, color = as.factor(network))) + 
  geom_point() + geom_path() + ylab("Average travel time in minutes") + xlab("Scale factor (%)") + 
  labs(color = "Network")  + 
  theme_bw() + theme(legend.position = "bottom")


ggplot(data %>% filter(it == 50, network == "coarse"), aes(x=100*scale, y=tt/60,
                                                                  color = as.factor(storEcp))) + 
  geom_point() + geom_path() + ylab("Average travel time in minutes") + xlab("Scale factor (%)") + 
  labs(color = "Exponent of the \nstorage capacity \nfactor") + 
  theme_bw() + theme(legend.position = "bottom")



ggplot(data %>% filter(storEcp == 0.75, network == "coarse"), aes(x=100*scale, y=tt/60,
                                                           color = as.factor(it))) + 
  geom_point() + geom_path() + ylab("Average travel time in minutes") + xlab("Scale factor (%)") + 
  labs(color = "Number of iterations") +   
  theme_bw() + theme(legend.position = "bottom")
