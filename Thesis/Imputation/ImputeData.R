#Preliminary Setup
require(mice)
require(VIM)
require(Hmisc)
require(corrplot)
require(glmnet)
require(plyr)
require(data.table)
require(lars)
require(parallel)

setwd("C:/Users/eli.crane/Documents/School/Thesis")

#Pull in Data
resume = 1
if (resume == 1){
  dtWork <- read.csv(file="Data/WorkingData/Combined_Imputed_Complete.csv", header=T, sep = ",")
  dropCols <- c("X","rowNames")
}else{
  dtWork <- read.csv(file="Data/TransformedData/WorldBankData_ReadyForImputation_MiddleEastSouthAsiaSouthEastAsia_1966_2013.csv", header=T, sep = ",")
  dropCols <- c("Key","X")
}

dtWork <- dtWork[,!(names(dtWork) %in% dropCols)]


#Find Initial Missing Pattern
dfMissPatt <- md.pattern(dtWork)

#Copy dataframe for final imputation
dfImpute <- dtWork
dfImpute$rowNames <- row.names(dfImpute)

#Set InitialVars
noCols = ncol(dfMissPatt)
skipRow = 8
z = 0
startTime <- Sys.time()
totalImputes = sum(data.frame(dfMissPatt[(2+skipRow):(nrow(dfMissPatt)-1),noCols]))
#totalImputes <- 36
totalImputesComplete = 0

imputesRemain <- totalImputes
totalPossibleVars = 0
totalMaxVars = 0
numIters = 500

#Garbage Collection
rm(list=c("dropCols"))

while (imputesRemain > 0)
{


#Print Summary
z <- z + 1

print(sprintf("###############Imputation: %s###############",z))
print(sprintf("Current Time: %s",as.POSIXlt(Sys.time())))
timeDiff <- difftime(Sys.time(),startTime,units="mins")
print(sprintf("Time Running: %f minutes", timeDiff))
print(sprintf("Imputed Values: %s",totalImputesComplete))
print(sprintf("Non Imputed Values: %s",imputesRemain))
print(sprintf("Number of Iterations: %s",numIters))
# if (totalImputesComplete > 0)
# {
#   avgPerc = (totalMaxVars/totalPossibleVars) * 100
#   print(sprintf("Average Percent of Vars Used Per Imputation: %s",avgPerc))
# }
print(sprintf("SkipRow: %s", skipRow))
if (z > 1)
{  
  timeRemain <- (timeDiff/(totalImputesComplete)) * (imputesRemain)
  print(sprintf("Estimated Time Remaining: %f minutes", timeRemain))
}
curImputes = dfMissPatt[2+skipRow,noCols]



#Create Missing Pattern
write.csv(md.pattern(dtWork), file = "Data/WorkingData/wbMissingPattern.csv")



#Determine if there are complete cases
missingVars = 0
completeCase = 0
#for (j in 1:(ncol(dfMissPatt)-1)){
#  if (dfMissPatt[1,j] == 0){
#    missingVars = missingVars + 1
#  }
#}

if(missingVars==0){completeCase=1}

#Garbage Collection
rm(list=c("missingVars"))

#If There are complete cases, proceed
listCompleteCases = c(0)
if (completeCase==1)
{
  row = 1
  for (i in 1:nrow(dtWork))
    {
    isComplete = T
    for (j in 1:ncol(dtWork))
      {
      if (is.na(dtWork[i,j]))
      {
        isComplete = F
        break
      }     
    }
    
    if (isComplete)
    {
      listCompleteCases = c(listCompleteCases,row)
    }
    row = row + 1
  }
}
listCompleteCases <- listCompleteCases[-c(1)] 

dfComplete <- dtWork[listCompleteCases,]

#Garbage Collection
rm(list=c("completeCase","row","isComplete","i","j"))

#Find Missing Variable(s) from next set
colNamesMiss <- colnames(dfMissPatt)
missCols <- c(0)
for (j in 1:(ncol(dfMissPatt) - 1))
{
  if (dfMissPatt[(2+skipRow),j] == 0){
    missCols <- c(missCols,j)
  }  
}
missCols <- missCols[2:length(missCols)]

colNames <- colnames(dtWork)
missColsTrans <- c(0)
for (h in 1:length(missCols))
{
  d = match(colNamesMiss[missCols[h]],colNames)
  missColsTrans <- c(missColsTrans, d)
}
missColsTrans <- missColsTrans[2:length(missColsTrans)]

listImputeCases = c(0)
row = 1
for (i in 1:nrow(dtWork))
{
  rowCase = 0
  isCase = T
  for (j in 1:ncol(dtWork))
  {
    
    if (is.na(dtWork[i,j]))
    {
      if (!(j %in% missColsTrans)){
      isCase = F
      break
      } else {
        rowCase = rowCase + 1
      }
    }     
  }
  if (isCase && !(i %in% listCompleteCases) && rowCase == curImputes)
  {
    listImputeCases = c(listImputeCases,row)
  }
  row = row + 1
}
listImputeCases = listImputeCases[2:length(listImputeCases)]



#Create Train and Predict data set based on NAs
dtTrain <- dtWork[listCompleteCases,]
dtPred <- dtWork[listImputeCases,]

#Garbage Collection
rm(list=c("isCase","d","h","i","j","row","colNamesMiss"))#,"dfMissPatt"))

#Loop through and impute all missing columns for the subset of data
if (length(missColsTrans) == 1){
  imputeSet <- sapply(1:length(missColsTrans),imputation, missColsTrans, colNames, dtTrain, dtPred, numIters)
} else{
  #Cluster SetUp
  cl <- makeCluster(4)
  clusterExport(cl,c('imputation'))
  
  imputeSet <- parSapply(cl, 1:length(missColsTrans),imputation, missColsTrans, colNames, dtTrain, dtPred, numIters)
  stopCluster(cl)
}

#Determine if Unsolvable for row skipping
unsolvable = 0
if (!is.matrix(imputeSet)){
  unsolvable = 1
} else{
  for (n in 1:ncol(imputeSet))
  {
    if (length(imputeSet[,n]) == 1){    
    } else{
      #totalMaxVars = totalMaxVars + maxVars
      dfImput <- imputeSet[,n]
      #Assign missing column number
      missCol <- missColsTrans[n]
      
        #Create Average from Prediction
      impNames <- summary(dfImput$imp)
      impPosition <- 0
      for (i in 1:nrow(impNames))
      {
        if (as.integer(impNames[i,1]) > 0)
        {    
          impPosition <- i
        }
      }
      
      
      dfAvgImp <- data.frame(dfImput$imp[impPosition])
      dfAvgImp$Avg <- 0
      
      for (i in 1:nrow(dfAvgImp)){
        Avg <- 0
        for (j in 1:(ncol(dfAvgImp) - 1)){
          Avg <- dfAvgImp[i,j] + Avg
        }
        Avg <- Avg/(ncol(dfAvgImp)-1)
        dfAvgImp[i,ncol(dfAvgImp)] = Avg
      }
      
      dfAvgImp$rowNames <- row.names(dfAvgImp)
      rowNames <- row.names(dfAvgImp) 
      
      #Impute Variable into missing rows
      for (i in 1:length(listImputeCases))
      {
        dfImpute[dfImpute$rowNames==listImputeCases[i],missCol] <- dfAvgImp[dfAvgImp$rowNames==rowNames[i],c("Avg")]
      }
      
      #Fix work
      dtWork <- dfImpute[,-c(ncol(dfImpute))]
      
      #Garbage Collection
      rm(list=c("Avg","dfImput","i","impNames","impPosition","j","rowNames"))
    }
  }
}

#Set final variables
if (unsolvable == 1)
{
  skipRow = skipRow + 1
}
dfMissPatt <- md.pattern(dtWork)
totalImputesComplete = totalImputesComplete + curImputes
imputesRemain <- totalImputes - totalImputesComplete

#Write DataTable
write.csv(dfImpute, file = "Data/WorkingData/Combined_Imputed_Complete.csv")

#Garbage Collection
rm(list=c("missColsTrans","dfComplete","missCols",#"predFactor",
          "colNames","imputeSet"))
rm(list=c("listImputeCases","listCompleteCases"))
}

#Write DataTable
write.csv(dfImpute, file = "Data/TransformedData/Combined_Imputed_Complete.csv")
