#
#compare travel times using different scaling factors
#Carlos Llorca 12.06.17
#

#set wd to read matrices
setwd("C:/models/munich/data/1.000.75")

#read matrices
source("C:/code/omx/api/r/omx.R")

scalingVector = c("0.01","0.05","0.10", "0.20", "0.50", "1.00")
iterationsVector = c("50")
exponentCF = 1
exponentSF = 0.75
matrixList = list()


for (i in 1:6){
  for (j in 1:1){
    scaling = scalingVector[i]
    iterations = iterationsVector[j]
    capacity = paste(format(round(as.numeric(scaling)^as.numeric(exponentCF),2),nsmall = 2))
    storage = paste(format(round(as.numeric(scaling)^as.numeric(exponentSF),2),nsmall = 2))
    simulationName = paste("TF",scaling,"CF",capacity,"SF",storage,"IT",iterations,"scalingSFExp",exponentSF,"CFExp",exponentCF, "TEST", sep = "")
    fileName = paste("tt" , simulationName,".omx", sep ="")
    matrixList[[i]] = readMatrixOMX(fileName, "mat1")
  }
}

n = dim(matrixList[[1]])[1]
randomOrigins = sample(1:n,10,replace=F) 

for (origin in randomOrigins){
  
}


