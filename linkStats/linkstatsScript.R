library(ggplot2)
setwd("C:/projects/MATSim/scaling/analysis/links")


path = "C:/models/munich/output"

exponent = 1
scalingVector = c("0.01","0.05","0.10")
iterationsVector = c("10","50","100")

iterationsVector = c("100")
for (scaling in scalingVector){
  for (iterations in iterationsVector){
    capacity = scaling
    storage = scaling
    simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponent,"CFExp",exponent, sep = "")
    lastIterationPath = paste("it.",iterations, sep="")
    fileName = paste("scalingSFExp",exponent,"CFExp",exponent,"_2016.",iterations,".linkstats.txt.gz",sep = "")
    pathToFile = paste(path,exponent,simulationName,"ITERS",lastIterationPath,fileName,sep = "/")
    
    data = read.csv(pathToFile, sep = "\t", header=TRUE)
    #plots the AADT
    rescalingFactor = 1/as.numeric(scaling)
    plot = ggplot(data, aes(x=HRS0.24avg*rescalingFactor))+ stat_ecdf() + xlim(0,20000) + ylim(0,1)
    print(plot)
    plotFileName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponent,"CFExp",exponent,"plotH024.png",sep = "")
    
    ggsave(plotFileName)
    
    #plots a peak hour hourly traffic volume
    plot = ggplot(data, aes(x=HRS7.8avg*rescalingFactor))+ stat_ecdf() + xlim(0,2000) + ylim(0,1)
    print(plot)
    plotFileName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponent,"CFExp",exponent,"plotH78.png",sep = "")
    
    ggsave(plotFileName)
  }
}




 

