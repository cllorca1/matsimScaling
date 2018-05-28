library(ggplot2)
library(dplyr)
library(chron)


setwd("C:/projects/MATSim/scaling/analysis/scoring")

#coarsenetwork
folder = "c:/projects/scaling_matsim_data/matsim_outputs/1.000.75"


scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50","100","200")

allData = data.frame()
for (scaling in scalingVector){
  for (iteration in iterationsVector){
    
    subfolder = paste("TF",scaling,
                      "CF",format(round(as.numeric(scaling)^1,digits = 2),nsmall=2),
                      "SF",format(round(as.numeric(scaling)^0.75,digits = 2),nsmall=2),
                      "IT",iteration,
                      "scalingSFExp0.75CFExp1TEST", sep = "")
    #this is because base year was set up differently
    if(iteration != "50"){
      file = "scalingSFExp0.75CFExp1TEST_2011.stopwatch.txt"
    } else {
      file = "scalingSFExp0.75CFExp1TEST_2016.stopwatch.txt"
    }
    pathToFile = paste(folder,subfolder,file,sep = "/")
    data = try(read.csv(file = pathToFile, sep = "\t"))
    if (is.data.frame(data)){
      data$scalingFactor = scaling
      data$iterations = iteration
      allData = rbind(allData,data)
    }
    
    
  }
  
}
summary(as.factor(allData$iterations))

#with the full network
folder = "c:/projects/scaling_matsim_data/matsim_outputs/1.000.75Full"


scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50")

allDataFull = data.frame()
for (scaling in scalingVector){
  for (iteration in iterationsVector){
    
    subfolder = paste("TF",scaling,
                      "CF",format(round(as.numeric(scaling)^1,digits = 2),nsmall=2),
                      "SF",format(round(as.numeric(scaling)^0.75,digits = 2),nsmall=2),
                      "IT",iteration,
                      "scalingSFExp0.75CFExp1TEST", sep = "")
    #this is because base year was set up differently
    if(iteration != "50"){
      file = "scalingSFExp0.75CFExp1TEST_2011.stopwatch.txt"
    } else {
      file = "scalingSFExp0.75CFExp1TEST_2016.stopwatch.txt"
    }
    pathToFile = paste(folder,subfolder,file,sep = "/")
    data = try(read.csv(file = pathToFile, sep = "\t"))
    if (is.data.frame(data)){
      data$scalingFactor = scaling
      data$iterations = iteration
      allDataFull = rbind(allDataFull,data)
    }
    
    
  }
  
}

#merge both networks

allData$network = "coarse"
allDataFull$network = "fine"

allData = rbind(allData, allDataFull)

allData$group = as.factor(paste(allData$scalingFactor,allData$iterations,sep = "-"))
allData$iterations = as.factor(as.numeric(allData$iterations))

labs = c("50 iterations", "100 iterations", "200 iterations")
names(labs) = c(50,100,200)

allData = allData %>%
  rowwise() %>% 
  mutate(iterationRuntime = as.chron.ITime(iteration)*3600*24)

summary = allData %>%
  group_by(group, network, iterations, scalingFactor) %>%
  summarize(runtime = sum(iterationRuntime)/3600)

summary = summary %>% filter(iterations != 100)


ggplot(summary %>% filter(iterations == 50), aes(x=as.numeric(scalingFactor)* 100, y=runtime,
                    group = as.factor(network),
                    linetype = as.factor(network))) +
  geom_line(size = 1) + 
  geom_point(shape = 21, size = 3, fill = "white") +
  theme_light() +
  xlab("Scale factor (%)")+
  ylab("Runtime (h)") + 
  labs(color = "Scaling factor (%)") + 
  theme(legend.position = "bottom") + 
  labs(group = "Network", linetype = "Network") + ylim(0,80)

ggplot(summary %>% filter(network == "coarse"), aes(x=as.numeric(scalingFactor)* 100, y=runtime,
                                                    group = as.factor(iterations),
                                                 size = as.factor(iterations))) +
  geom_path() + 
  geom_point(shape = 21, size = 3, fill = "white")+
  theme_light() +
  scale_size_discrete(range = c(1,3)) + 
  xlab("Scale factor (%)") +
  ylab("Runtime (h)") + 
  labs(color = "Scaling factor (%)") + 
  theme(legend.position = "bottom") + 
  labs(group = "Iterations", size = "Iterations")
  
