library(data.table)


#read and subsample the population

folderSP = "c:/models/silo/muc/microData/"
filePP = "pp_2011.csv"
fileJJ = "jj_2011.csv"

pp = fread(paste(folderSP, filePP, sep = ""))
jj = fread(paste(folderSP, fileJJ, sep = ""))

pp = merge(pp, jj, by.x = "workplace", by.y="id" )


pp = pp[sample(nrow(pp), 1000), ]


#read a travel time matrix: 

source("C:/code/omx/api/R/omx2.R")
ttMatrix = readMatrixOMX("C:/models/silo/muc/skims/tt10Congested.omx", "mat1")

#assign travel time to each row: 
for(person in 1:nrow(pp)){
  pp$tt[person] = ttMatrix[pp$homeZone[person], pp$zone[person]]
  print(person)
}

#print out the file to work in Excel later
fwrite(pp, "c:/models/matsimScheduleTests/input/subPopulation.csv")
