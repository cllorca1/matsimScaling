#add intrazonals to matrices

source("C:/code/omx/api/r/omx2.R")

folder = "c:/projects/MATSim/intraZonals/"
fileName = "IntraZonalDistances_netOrGeom.csv"
intrazonals = read.csv(paste(folder,fileName,sep=""))

omxFileName = "c:/models/munich/data/skimsAllIntrazonal2.omx"
listOMX(OMXFileName = omxFileName)

matrixNames = c("distanceByDistance","distanceByTime", "timeByDistance", "timeByTime")

for (i in 1:nrow(intrazonals)){
    id = intrazonals$id[i]
    intrazonalDistance = intrazonals$intrazonalDistance[i]
    intrazonalTime = intrazonals$intrazonalTime[i]
    
    writeMatrixOMX(OMXFileName = omxFileName, Matrix = intrazonalDistance, 
                   MatrixSaveName = matrixNames[1], RowIndex = id, ColIndex = id, Replace = T)
    writeMatrixOMX(OMXFileName = omxFileName, Matrix = intrazonalDistance, 
                   MatrixSaveName = matrixNames[2], RowIndex = id, ColIndex = id, Replace = T)
    writeMatrixOMX(OMXFileName = omxFileName, Matrix = intrazonalTime, 
                   MatrixSaveName = matrixNames[3], RowIndex = id, ColIndex = id, Replace = T)
    writeMatrixOMX(OMXFileName = omxFileName, Matrix = intrazonalTime, 
                   MatrixSaveName = matrixNames[4], RowIndex = id, ColIndex = id, Replace = T)
    print (paste("completed:",i))
    
}
  
