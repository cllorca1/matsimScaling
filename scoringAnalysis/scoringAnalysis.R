

library(ggplot2)

setwd("C:/projects/MATSim/scaling/analysis/scoring")


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
      file = "scalingSFExp0.75CFExp1TEST_2011.scorestats.txt"
    } else {
      file = "scalingSFExp0.75CFExp1TEST_2016.scorestats.txt"
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

allData$group = as.factor(paste(allData$scalingFactor,allData$iterations,sep = "-"))
allData$iterations = as.factor(as.numeric(allData$iterations))

labs = c("50 iterations", "100 iterations", "200 iterations")
names(labs) = c(50,100,200)

ggplot(allData, aes(x=ITERATION, y=avg..AVG,
                    group = group,
                    color = as.factor(100*as.numeric(scalingFactor)))) +
  geom_line(size = 1)+
  theme_bw()+
  xlab("Iteration")+
  ylab("Average MATSim plan score (points)") + 
  labs(color = "Scaling factor (%)") + 
  scale_color_manual(values= c("red", "pink", "blue", "lightblue","green4","darkgray")) +
  facet_grid(.~iterations, scale = "free_y", labeller = labeller(iterations= labs)) + 
  theme(legend.position = "bottom")
  
