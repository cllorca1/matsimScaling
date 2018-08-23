


#set wd to read matrices
setwd("C:/models/munich/accessibility")

#matrix filenames for congested and uncongested
fileNameCongested = "tt10Congested.omx"
fileNameUncongested = "ttUncongested.omx"

compareMatrices(fileNameCongested, fileNameUncongested, 200, 9999999)

#matrix filenames for time or distance based distance matrices
fileNameDistance = "tdShortestPath.omx"
fileNameTime = "tdFastestPath.omx"

data = compareMatrices(fileNameDistance, fileNameTime, 300, 20000)


#matrix filenames for time or distance based distance matrices
fileNameDistance = "tdShortestPath.omx"
fileNameDistanceFine = "tdShortestPathFine.omx"

data = compareMatrices(fileNameDistance, fileNameDistanceFine, 300, 20000)

mean(data$value1)
mean(data$value2)


file1 = "tdTestIntrazonal.omx"
file2 = "tdTest.omx"

data = compareMatrices(file1, file2, 100, 150000)


#matrices for transit with and without 2SS
setwd("C:/models/munich/data/2ss_before_after")

#matrix filenames for congested and uncongested
fileName1 = "ttTransitTotaltest2SSafterComplete.omx"
fileName2 = "ttTransitTotaltest2SSbeforeComplete.omx"

compareMatrices(fileName1, fileName2, 200, 9999)



#matrices for transit with and without 2SS


#matrix filenames for congested and uncongested
fileName1 = "tdEuc.omx"
fileName2 = "tdTest.omx"

data = compareMatrices(fileName1, fileName2, 200, 50000)

ggplot(data, aes(x=value2)) + stat_ecdf()


listOMX(file2)

path = "c:/models/silo/muc/skims/"

fileName1 = paste(path, "skimsAllIntrazonal.omx", sep = "")
fileName2 = paste(path,"travelTimeAuto.omx", sep = "")

data = compareMatrices(fileName1, fileName2, 100,50,"timeByTime", "HOVTime", 1/60, 1) 
data = compareMatrices(fileName1, fileName2, 200,500000,"distanceByTime", "distanceByTime", 1, 1) 


path = "c:/models/silo/muc/"

fileName1 = paste(path, "skims/skimsAllIntrazonal.omx", sep = "")
fileName2 = paste(path,"mitoOutput/2050/outputCarSkim.omx", sep = "")

data = compareMatrices(fileName1, fileName2, 50,500,"timeByTime", "timeByTime", 1/60, 1) 

data = compareMatrices(fileName1, fileName2, 200,500000,"distanceByTime", "distanceByTime", 1, 1) 




