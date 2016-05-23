#Preliminary Setup
require(mice)
require(VIM)
require(Hmisc)
require(corrplot)
require(glmnet)
require(plyr)
require(data.table)
require(lars)
require(compiler)

#Define File Paths
#workingPath = "Data/WorkingData/"
#transformedPath = "Data/TransformedData/"


setwd("~")

#Pull in Data
dtWork <- read.csv(file="OriginalData/WorldBankData_ReadyForImputation_MiddleEastSouthAsiaSouthEastAsia_1966_2013.csv", header=T, sep = ",")
dropCols <- c("Key","X")
dtWork <- dtWork[,!(names(dtWork) %in% dropCols)]

#Find Initial Missing Pattern
dfMissPatt <- md.pattern(dtWork)

#Copy dataframe for final imputation
dfImpute <- dtWork
dfImpute$rowNames <- row.names(dfImpute)

#Set InitialVars
noCols = ncol(dfMissPatt)
totalImputes = sum(data.frame(dfMissPatt[,noCols]))
totalImputesComplete = 0
skipRow = 0
z = 0
startTime <- Sys.time()
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
if (totalImputesComplete > 0)
{
  avgPerc = (totalMaxVars/totalPossibleVars) * 100
  print(sprintf("Average Percent of Vars Used Per Imputation: %s",avgPerc))
}
print(sprintf("SkipRow: %s", skipRow))
if (z > 1)
{  
  timeRemain <- (timeDiff/(totalImputesComplete)) * (imputesRemain - totalImputesComplete)
  print(sprintf("Estimated Time Remaining: %f minutes", timeRemain))
}
curImputes = dfMissPatt[2+skipRow,noCols]



#Create Missing Pattern
write.csv(md.pattern(dtWork), file = "WorkingData/wbMissingPattern.csv")



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
  isCase = T
  for (j in 1:ncol(dtWork))
  {
    
    if (is.na(dtWork[i,j]) && !(j %in% missColsTrans))
    {
      isCase = F
      break
    }     
  }
  if (isCase && !(i %in% listCompleteCases))
  {
    listImputeCases = c(listImputeCases,row)
  }
  row = row + 1
}
listImputeCases = listImputeCases[2:length(listImputeCases)]

rm(list=c("isCase","d","h","i","j","row","colNamesMiss","dfMissPatt"))

#Create Train and Predict data set based on NAs
dtTrain <- dtWork[listCompleteCases,]
dtPred <- dtWork[listImputeCases,]

#Garbage Collection
rm(list=c("listImputeCases","listCompleteCases"))

#Loop through and impute all missing columns for the subset of data
#Determine if Unsolvable for row skipping
unsolvable = 0

for (n in 1:length(missColsTrans))
  {
  
    #n <- 1
    #Assign missing column number
    missCol <- missColsTrans[n]
    
    #Create Inverted Matrix for Lasso Model
    x <- as.matrix(dtTrain[,-missColsTrans])
    y <- as.matrix(dtTrain[,missCol])
    xfactors <- model.matrix(y ~ x)[,-1]
    
    #Run Lasso and find best Lambda
    glmmod <- glmnet(xfactors,y=y,alpha=1,family='gaussian')
    cv.glmmod <- cv.glmnet(xfactors,y=y,alpha=1)
    bestLambda <- cv.glmmod$lambda.min
    bestModel <- match(bestLambda, as.vector(glmmod$lambda))
    
    #Get Lasso Coeffs
    lassoCoeffs <- data.frame(coef(glmmod)[,bestModel])
    setnames(lassoCoeffs,"coeffs")
    lassoCoeffs <- data.frame(row.names(lassoCoeffs),lassoCoeffs$coeffs)
    setnames(lassoCoeffs,"row.names.lassoCoeffs.","feature")
    setnames(lassoCoeffs,"lassoCoeffs.coeffs","coeffs")
    
    #Garbage Collection
    rm(list=c("bestLambda","bestModel"))
    
    #Set predict model
    dtPred[,missCol] <- 0
    #x <- as.matrix(dtPred[,-missColsTrans])
    #y <- as.matrix(dtPred[,missCol])
    #predFactor <- model.matrix(y ~ x)[,-1]
    #fit <- predict(cv.glmmod,predFactor)
    
    #Garbage Collection
    rm(list=c("x","xfactors","y","cv.glmmod","glmmod"))
    
    #Mixed model Approach
    dfNZ <- as.vector(lassoCoeffs[abs(lassoCoeffs$coeff) > 0,]$feature)
    totalPossibleVars = totalPossibleVars + length(dfNZ)
    
    for (h in 1:length(dfNZ))
    {
      subStr = substr(dfNZ[length(dfNZ) + 1 - h],2,nchar(dfNZ[length(dfNZ) +1-h]))
      dfNZ = dfNZ[-c(length(dfNZ) + 1 - h)]
      dfNZ = c(dfNZ,subStr)
    }
    
    dfNZ <- dfNZ[2:length(dfNZ)]
    dfNZ <- c(as.vector(dfNZ),colNames[missCol])
    seed = 0
    maxVars <- 0
    iter = 0
    dtPred[,missCol] <- NA
    dfAll <- rbind(dtTrain, dtPred)
    
    #Perform at least 1000 iterations to solve for the maximum number of randomly selected variables
    while(iter < numIters)#length(dfNZ)^2)
    {
      seed = seed + 1
      set.seed(seed)
      numVars <- sample((maxVars+1):(length(dfNZ)-1),1)
      if (numVars > maxVars && maxVars != (length(dfNZ)-1))
      {
        #print("hi")
        iter <- iter + 1
        
        #print "hi"
        
        varVector <- sample(1:(length(dfNZ)-1),numVars,replace=F)
        varVector <- c(varVector,length(dfNZ))
        
        listVars <- dfNZ[varVector]
        dtWorkImput <- dfAll[,(names(dfAll) %in% listVars)]
        
        #Catch error and continue if equation cannot be solved with given variables
        capture.output(tryCatch({
          dfImput <- mice(dtWorkImput,m=10,maxit=10, method="pmm",seed = 3)
          if (numVars > maxVars)
          {
            maxVars <- numVars
          }
          successVector = varVector
          },
            error = function(e)
            {
              #next
            }))
          
      } 
      else if (maxVars == (length(dfNZ)-1))
      {
        break
      }
      
    }
    
    totalMaxVars = totalMaxVars + maxVars
    if (maxVars == 0)
    {
      unsolvable = 1
    }
    
    else
    {
    
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
      for (i in 1:length(rowNames))
      {
        dfImpute[dfImpute$rowNames==rowNames[i],missCol] <- dfAvgImp[dfAvgImp$rowNames==rowNames[i],c("Avg")]
      }
      
      #Fix work
      dtWork <- dfImpute[,-c(ncol(dfImpute))]
      
      #Garbage Collection
      rm(list=c("Avg","dfImput","i","impNames","impPosition","j","rowNames",
                "successVector"))
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
write.csv(dfImpute, file = "WorkingData/Combined_Imputed_Complete.csv")

#Garbage Collection
rm(list=c("maxVars","numVars","listVars","iter","h","missColsTrans","n",
          "seed","subStr","varVector","dfNZ","dtWorkImput",
          "dfComplete","dfAll","lassoCoeffs","missCols",#"predFactor",
          "missCol","colNames"))
}

#Write DataTable
write.csv(dfImpute, file = "TransformedData/Combined_Imputed_Complete.csv")
