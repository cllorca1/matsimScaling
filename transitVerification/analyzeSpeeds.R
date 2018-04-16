library(ggplot2)

folder = "C:/models/transit/output/verification_results/"
file = "speedCheck.csv"
folderFile = paste(folder,file,sep = "")

data = read.csv(folderFile)

ggplot(data, aes(x=speed)) + stat_ecdf()

summary(data$speed)
