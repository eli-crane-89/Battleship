#Preliminary Setup
require(mice)
require(VIM)
require(Hmisc)
require(corrplot)
require(glmnet)
require(plyr)
require(data.table)
require(lars)

setwd("~/Python")

#Pull in Data
dtWork <- read.csv(file="Final_Train_AfterCorr.csv", header=T, sep = ",")
dropCols <- c("Key")
dtWork <- dtWork[,!(names(dtWork) %in% dropCols)]

#Garbage Collection
rm(list=c("dropCols"))

for (z in 1:4)
{
  
print(z)

#Copy dataframe for final imputation
dfImpute <- dtWork
dfImpute$rowNames <- row.names(dfImpute)

#Create Missing Pattern
write.csv(md.pattern(dtWork), file = "wbMissingPattern.csv")

#Find Missing Pattern
dfMissPatt <- md.pattern(dtWork)

#Determine if there are complete cases
missingVars = 0
completeCase = 0
for (j in 1:(ncol(dfMissPatt)-1)){
  if (dfMissPatt[1,j] == 0){
    missingVars = missingVars + 1
  }
}

if(missingVars==0){completeCase=1}

#Garbage Collection
rm(list=c("missingVars","j"))

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
  if (dfMissPatt[2,j] == 0){
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

listCases = c(0)
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
    listCases = c(listCases,row)
  }
  row = row + 1
}
listCases = listCases[2:length(listCases)]

rm(list=c("isCase","d","h","i","j","row","colNamesMiss","dfMissPatt"))

#Create Train and Predict data set based on NAs
dtTrain <- dtWork[listCompleteCases,]
dtPred <- dtWork[listCases,]

#Garbage Collection
rm(list=c("listCases","listCompleteCases"))

#Loop through and impute all missing columns for the subset of data
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
    x <- as.matrix(dtPred[,-missColsTrans])
    y <- as.matrix(dtPred[,missCol])
    predFactor <- model.matrix(y ~ x)[,-1]
    #fit <- predict(cv.glmmod,predFactor)
    
    #Garbage Collection
    rm(list=c("x","xfactors","y","cv.glmmod","glmmod"))
    
    #Mixed model Approach
    dfNZ <- as.vector(lassoCoeffs[abs(lassoCoeffs$coeff) > 0,]$feature)
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
    while(iter < 1000 || maxVars == 0)#length(dfNZ)^2)
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
        tryCatch({
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
            })
          
      } 
      else if (maxVars == (length(dfNZ)-1))
      {
        break
      }
      
    }
    
    
    
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
  }


#Garbage Collection
rm(list=c("Avg","dfImput","i","impNames","impPosition","j","rowNames",
          "missCol","colNames"))

rm(list=c("maxVars","numVars","listVars","iter","h","missColsTrans","n",
          "seed","subStr","successVector","varVector","dfNZ","dtWorkImput",
          "dfComplete","dfAll","lassoCoeffs","missCols","predFactor"))
}

#Write DataTable
write.csv(dfImpute, file = "dtImpute.csv")
