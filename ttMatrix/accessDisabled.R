#time for disabled 

library(ggplot2)

pathToOmx = "C:/code/omx/api/r/omx2.R"
source(pathToOmx)

pathToZoneFile = "c:/models/munich/input/plans/centroids_id_new.csv"
zones = read.csv(pathToZoneFile)

matrixFolder = "c:/models/munich/data/"

#auto travel time
matrixFileName = "skimsAllIntrazonal.omx"
matrixName = "timeByTime"
auto_tt = readMatrixOMX(OMXFileName = paste(matrixFolder, matrixFileName, sep=""), MatrixName = matrixName)
auto_tt = auto_tt / 60


#transit
maxValueInMinutes = 10000

#transit total travel time
matrixFileName = "skimsTransit/total_t.omx"
matrixName = "mat1"
transit_tt = readMatrixOMX(OMXFileName = paste(matrixFolder, matrixFileName, sep=""), MatrixName = matrixName)

#transit access time
matrixFileName = "skimsTransit/access_t.omx"
transit_access = readMatrixOMX(OMXFileName = paste(matrixFolder, matrixFileName, sep=""), MatrixName = matrixName)

#transit egress time
matrixFileName = "skimsTransit/eggress_t.omx"
transit_egress = readMatrixOMX(OMXFileName = paste(matrixFolder, matrixFileName, sep=""), MatrixName = matrixName)

#transit transfers
matrixFileName = "skimsTransit/transfers.omx"
transit_transfers = readMatrixOMX(OMXFileName = paste(matrixFolder, matrixFileName, sep=""), MatrixName = matrixName)


#accessibility parameters (general)
alpha = 1.25
beta = -0.1

#accessibility base case
idVector = zones$id
populationVector = zones$population
transit_tt[is.na(transit_tt)] = maxValueInMinutes
acc = (exp(beta * auto_tt)) %*% (populationVector^alpha)
accTransit = (exp(beta * transit_tt)) %*% (populationVector^alpha)

summaryAccessibilities = data.frame(id = idVector, auto_accessibility = acc, transit_accessibility = accTransit)

ggplot(summaryAccessibilities, aes(x=auto_accessibility)) + geom_histogram()
ggplot(summaryAccessibilities, aes(x=auto_accessibility, y = transit_accessibility)) + geom_point()


#transformations for reduce mobility users
additionalAccessToCar = 3 #min

speedForAccessToTransit = 2.5 #km/h
extraTimeAtTransfer = 1 #min

auto_tt_rm = auto_tt + 2* additionalAccessToCar

transit_access_rm_dif = transit_access * 5 / speedForAccessToTransit - transit_access
transit_egress_rm_dif = transit_egress * 5 / speedForAccessToTransit - transit_access
transit_transfer_rm_dif = transit_transfers*extraTimeAtTransfer
transit_tt_rm = transit_tt + transit_access_rm_dif + transit_egress_rm_dif +transit_transfer_rm_dif

#accessibility for reduced mobility users
transit_tt_rm[is.na(transit_tt_rm)] = maxValueInMinutes
acc = (exp(beta * auto_tt_rm)) %*% (populationVector^alpha)
accTransit = (exp(beta * transit_tt_rm)) %*% (populationVector^alpha)

summaryAccessibilities_rm = data.frame(id = idVector, auto_accessibility_rm = acc, transit_accessibility_rm = accTransit)

summary = merge(x=summaryAccessibilities, y = summaryAccessibilities_rm, by = "id")

ggplot(summary, aes(x=auto_accessibility, y = auto_accessibility_rm)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red")


ggplot(summary, aes(x=transit_accessibility, y = transit_accessibility_rm)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red")




