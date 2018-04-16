

library(ggplot2)

setwd("C:/projects/MATSim/scaling/analysis/scoring")


folder = "c:/projects/scaling_matsim_data/matsim_outputs/1.000.75"


scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50","200")

allData = data.frame()
for (scaling in scalingVector){
  for (iteration in iterationsVector){
    
    subfolder = paste("TF",scaling,
                      "CF",format(round(as.numeric(scaling)^1,digits = 2),nsmall=2),
                      "SF",format(round(as.numeric(scaling)^0.75,digits = 2),nsmall=2),
                      "IT",iteration,
                      "scalingSFExp0.75CFExp1TEST", sep = "")
    if(iteration == 200){
      file = "scalingSFExp0.75CFExp1TEST_2011.scorestats.txt"
    } else {
      file = "scalingSFExp0.75CFExp1TEST_2016.scorestats.txt"
    }
    pathToFile = paste(folder,subfolder,file,sep = "/")
    data = read.csv(file = pathToFile, sep = "\t" )
    data$scalingFactor = scaling
    data$iterations = iteration
    allData = rbind(allData,data)
  }
}


allData$group = as.factor(paste(allData$scalingFactor,allData$iterations,sep = "-"))


ggplot(allData, aes(x=ITERATION, y=avg..AVG,
                    group = group,
                    color = as.factor(scalingFactor),
                    linetype = as.factor(iterations)))+
  geom_line(size = 1)+
  theme_light()+
  xlab("iteration")+
  ylab("average MATSim plan score (points)")
