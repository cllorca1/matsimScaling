#script to analyze travel time distributions
library(dplyr)
library(ggplot2)
#1. test different scales for the same network and iterations

folder = "c:/projects/scaling_matsim_data/matsim_outputs/1.000.75/"

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
    pathToFile = paste(folder,subfolder,"/ITERS/it.",iteration,"/", sep="")
    
    if(iteration != "50"){
      file = paste("scalingSFExp0.75CFExp1TEST_2011.",iteration,".tripdurations.txt", sep="")
    } else {
      file = paste("scalingSFExp0.75CFExp1TEST_2016.",iteration,".tripdurations.txt", sep="")
    }
    pathToFile = paste(pathToFile,file,sep="")
    data = try(read.csv(file = pathToFile, sep = "\t"))
    
    if (is.data.frame(data)){
      data$scale = scaling
      data$iterations = iteration
      data$network = "coarse"
      allData = rbind(data,allData)
    }
  }
}


allData = allData %>% filter(!grepl("average",pattern))

melted = melt(allData, id.vars = c("pattern", "iterations", "scale", "network"))


intervals = as.vector(unique(melted$variable))
intervals2 = seq(2.5, 62.5,5)

getIntervalFromCode = function(code){
  intervals2[match(code, intervals)]
}

melted = melted %>% rowwise() %>% mutate(time = getIntervalFromCode(variable))

melted = melted %>% group_by(iterations, network, scale, time) %>% summarize(value = sum(value))

melted$iterations = as.factor(as.numeric(melted$iterations))


it_labs = c("50 iterations", "100 iterations", "200 iterations")
names(it_labs) = c(50,100,200)

ggplot(melted, aes(x=time, y=value/as.numeric(scale), color = as.factor(scale))) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 3, fill = "white") +
  facet_grid (.~ iterations, labeller = labeller(iterations = it_labs)) + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  scale_color_manual(values= c("red", "pink", "blue", "lightblue","green4","darkgray")) + 
  labs(color = "Scale factor") + ylab("Frequency (#trips)") + 
  xlab("travel time (min)")

# 2. test different networks

folder = "c:/projects/scaling_matsim_data/matsim_outputs/"

subfolders = c("1.000.75", "1.000.75full")
networks = c("coarse", "fine")

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50")

allData = data.frame()
for (scaling in scalingVector){
  for (iteration in iterationsVector){
    for (n in 1:2){
      main_subfolder = subfolders[n]
    
    
    subfolder = paste("TF",scaling,
                      "CF",format(round(as.numeric(scaling)^1,digits = 2),nsmall=2),
                      "SF",format(round(as.numeric(scaling)^0.75,digits = 2),nsmall=2),
                      "IT",iteration,
                      "scalingSFExp0.75CFExp1TEST", sep = "")
    pathToFile = paste(folder,main_subfolder,"/",subfolder,"/ITERS/it.",iteration,"/", sep="")
    
    
    file = paste("scalingSFExp0.75CFExp1TEST_2016.",iteration,".tripdurations.txt", sep="")
    
    pathToFile = paste(pathToFile,file,sep="")
    data = try(read.csv(file = pathToFile, sep = "\t"))
    
    if (is.data.frame(data)){
      data$scale = scaling
      data$iterations = iteration
      data$network = networks[n]
      allData = rbind(data,allData)
    }
    }
  }
}


allData = allData %>% filter(!grepl("average",pattern))

melted = melt(allData, id.vars = c("pattern", "iterations", "scale", "network"))


intervals = as.vector(unique(melted$variable))
intervals2 = seq(2.5, 62.5,5)

getIntervalFromCode = function(code){
  intervals2[match(code, intervals)]
}

melted = melted %>% rowwise() %>% mutate(time = getIntervalFromCode(variable))

melted = melted %>% group_by(iterations, network, scale, time) %>% summarize(value = sum(value))


ggplot(melted, aes(x=time, y=value/as.numeric(scale), color = as.factor(network))) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~as.factor(as.numeric(scale)*100), nrow = 2) + 
  theme(legend.position = "bottom")

