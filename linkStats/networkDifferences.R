##get differences in link volumes between different scaling factors (re-scaled) 
##and store them in a file to plot them in a map

library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
setwd("C:/projects/MATSim/scaling/analysis/links")


#manual!!!
paths = c("C:/projects/scaling_matsim_data/matsim_outputs/1.000.75",
          "C:/projects/scaling_matsim_data/matsim_outputs/1.000.75full")

exponentCF = 1

exponentSF = 0.75

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
#iterationsVector = c("10","50","100")

iterationsVector = c("50")

dataAll = list()

#loop to read all the linkstat tables for all the simulated cases - edit for full network

for (networkIndex in 1:2) {
  
  
  path = paths[networkIndex]
  
  
  for (scaling in scalingVector){
    for (iterations in iterationsVector){
      capacity = paste(format(round(as.numeric(scaling)^as.numeric(exponentCF),2),nsmall = 2))
      storage = paste(format(round(as.numeric(scaling)^as.numeric(exponentSF),2),nsmall = 2))
      simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponentSF,"CFExp",exponentCF, "TEST", sep = "")
      lastIterationPath = paste("it.",iterations, sep="")
      fileName = paste("scalingSFExp",exponentSF,"CFExp",exponentCF,"TEST_2016.",iterations,".linkstats.txt.gz",sep = "")
      pathToFile = paste(path,simulationName,"ITERS",lastIterationPath,fileName,sep = "/")
      
      data = read.csv(pathToFile, sep = "\t" )
      #fread does not work with some of the linkstats files
      rescalingFactor = 1/as.numeric(scaling)
      
      
      data2 = data %>% select(LINK, FROM, TO, HRS7.8avg,  HRS17.18avg, TRAVELTIME7.8avg, TRAVELTIME17.18avg )
      
      data2$HRS7.8avg = data2$HRS7.8avg*rescalingFactor
      data2$HRS17.18avg = data2$HRS17.18avg*rescalingFactor
      
      dataAll[[which(scaling == scalingVector)]] = data2
      print(scaling)
    }
  }
  
  #with the last data frame created: 
  #define groups by capacity and speed
  data$capacityCut = cut(data$CAPACITY,
                         breaks = c(-Inf,1250,2500,3750,5000,6125,7500,8750,10000,Inf),
                         labels = c(0,1,2,3,4,5,6,7,8))
  data$speedCut = cut(data$FREESPEED,
                      breaks = c(-Inf,5,10,15,20,25,30,Inf),
                      labels = c(0,1,2,3,4,5,6))
  
  totalLength = sum(data$LENGTH)
  
  data %>% group_by(capacityCut) %>% summarize(weight = sum(LENGTH))
  
  data %>% group_by(capacityCut) %>% summarize(weight = sum(LENGTH)/totalLength)
  
  differenceTable = data %>% select(LINK, TO, FROM, CAPACITY, capacityCut, speedCut, LENGTH)
  
  #analyze the aggregated congestion level of the links in all the simulations
  
  differenceTable$dailyVC1 = dataAll[[1]]$HRS7.8avg / differenceTable$CAPACITY
  differenceTable$dailyVC5 = dataAll[[2]]$HRS7.8avg / differenceTable$CAPACITY
  differenceTable$dailyVC10 = dataAll[[3]]$HRS7.8avg/ differenceTable$CAPACITY
  differenceTable$dailyVC20 = dataAll[[4]]$HRS7.8avg/ differenceTable$CAPACITY
  differenceTable$dailyVC50 = dataAll[[5]]$HRS7.8avg/ differenceTable$CAPACITY
  differenceTable$dailyVC100 = dataAll[[6]]$HRS7.8avg/ differenceTable$CAPACITY
  
  differenceTableLong = melt(differenceTable, variable.name = "variable", id.vars = c("LINK", "FROM", "TO", "capacityCut", "speedCut" , "CAPACITY", "LENGTH"), value.name = "value")
  differenceTableLong2 = melt(differenceTable, variable.name = "variable", id.vars = c("LINK", "FROM", "TO", "capacityCut", "speedCut" , "CAPACITY", "LENGTH", "dailyVC100"), value.name = "value")
  
  
  
  summary = differenceTableLong %>% group_by(variable, capacityCut) %>% summarise(vcRatio = mean(value)) %>% tidyr::spread(capacityCut, vcRatio)
  
  
  labs_cap = c("1250","2500","3750","5000","6125","7500","8750","10000","15000")
  names(labs_cap) = c(0,1,2,3,4,5,6,7,8)
  
  
  differenceTableLong2 = differenceTableLong2 %>% filter(differenceTableLong2$LINK %% 10 == 0)
  
  labs_scales = c("1%","5%","10%","20%","50%")
  names(labs_scales) = c("dailyVC1","dailyVC5","dailyVC10","dailyVC20","dailyVC50")
  
  ggplot(differenceTableLong2, aes(x=dailyVC100, y=value, color=variable)) +
    geom_point(size = 1, alpha= 0.1) + theme_bw() +
    xlab("100% link v/c") +
    ylab("scaled link v/c") + 
    geom_abline(intercept = 0, slope = 1) + 
    theme(legend.position = "none") + 
    scale_color_manual(values= c("red", "pink", "blue", "lightblue","green4","darkgray")) +
    facet_wrap("variable", ncol = 2, labeller = labeller(variable = labs_scales))
  
  
  if(networkIndex == 1){
    coarseNetwork = differenceTableLong2 %>% filter (variable == "dailyVC1")
    coarseNetwork$network = "coarse" 
  } else {
    fineNetwork = differenceTableLong2 %>% filter (variable == "dailyVC1")
    fineNetwork$network = "fine" 
  }
}








#combine
bothNetworks = rbind(coarseNetwork, fineNetwork)
bothNetworks = bothNetworks %>% select(dailyVC100, LINK, network)
bothNetworks = cast(bothNetworks, dailyVC100 ~ network, fun.aggregate = sum)

ggplot(bothNetworks, aes(x=dailyVC100, linetype = network)) +
  stat_ecdf(size = 1) +
  labs(linetype = "Network")  + 
  theme_bw() + theme(legend.position = "bottom") + 
  xlab("Network link v/c") +
  ylab("Cumulative frequency") 
  

#compare the same links in coarse and in fine --> 


coarse1 = coarseNetwork %>% select(FROM, TO, v_c_coarse = dailyVC100)
fine1 = fineNetwork %>% select(FROM, TO, v_c_fine = dailyVC100)

merged = merge(x=coarse1, y=fine1, by =c("FROM", "TO"))

ggplot(merged, aes(x=v_c_coarse, y = v_c_fine)) +
  geom_point(size = 1, alpha= 0.1) + 
  theme_bw() +
  xlab("Coarse newtork link v/c") + ylab("Fine newtork link v/c") +
  geom_abline(intercept = 0, slope = 1) 
  
ggplot(merged, aes(x=v_c_fine)) +
  stat_ecdf(size = 1) +
  theme_bw() + theme(legend.position = "bottom") + 
  xlab("Network link v/c") +
  ylab("Cumulative frequency") 



# 
# 
# #analyze differences in flow
# 
differenceTable$morning1 = dataAll[[1]]$HRS7.8avg
differenceTable$morning5 = dataAll[[2]]$HRS7.8avg
differenceTable$morning10 = dataAll[[3]]$HRS7.8avg
differenceTable$morning20 = dataAll[[4]]$HRS7.8avg
differenceTable$morning50 = dataAll[[5]]$HRS7.8avg
differenceTable$morning100 = dataAll[[6]]$HRS7.8avg

differenceTable$evening1 = dataAll[[1]]$HRS17.18avg
differenceTable$evening5 = dataAll[[2]]$HRS17.18avg
differenceTable$evening10 = dataAll[[3]]$HRS17.18avg
differenceTable$evening20 = dataAll[[4]]$HRS17.18avg
differenceTable$evening50 = dataAll[[5]]$HRS17.18avg
differenceTable$evening100 = dataAll[[6]]$HRS17.18avg

differenceTable$d1m = (differenceTable$morning100 - differenceTable$morning1)/differenceTable$morning100
differenceTable$d5m = (differenceTable$morning100 - differenceTable$morning5)/differenceTable$morning100
differenceTable$d10m = (differenceTable$morning100 - differenceTable$morning10)/differenceTable$morning100
differenceTable$d20m = (differenceTable$morning100 - differenceTable$morning20)/differenceTable$morning100
differenceTable$d50m = (differenceTable$morning100 - differenceTable$morning50)/differenceTable$morning100

differenceTable$d1e = (differenceTable$evening100 - differenceTable$evening1)/differenceTable$evening100
differenceTable$d5e = (differenceTable$evening100 - differenceTable$evening5)/differenceTable$evening100
differenceTable$d10e = (differenceTable$evening100 - differenceTable$evening10)/differenceTable$evening100
differenceTable$d20e = (differenceTable$evening100 - differenceTable$evening20)/differenceTable$evening100
differenceTable$d50e = (differenceTable$evening100 - differenceTable$evening50)/differenceTable$evening100

summary(differenceTable)





# 
# ggplot(differenceTable, aes(capacityCut)) + geom_bar()
# 
# 
write.csv(x=differenceTable, file = "networkMap/netowrkDifferences.csv", row.names = FALSE)
# header = names(differenceTable)
# header[1] = "Integer"
# for (i in 1:length(header)){
#   header[i] = "Real"
# }
# write.table(x=(header), file = "networkMap/netowrkDifferences.csvt", row.names = F)


