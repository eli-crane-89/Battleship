imputation <- function(n, missColsTrans, colNames, dtTrain, dtPred, numIters)
{
  #Required packages
  require(mice)
  require(VIM)
  require(Hmisc)
  require(corrplot)
  require(glmnet)
  require(plyr)
  require(data.table)
  require(lars)
  
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
  
  #Garbage Collection
  rm(list=c("x","xfactors","y","cv.glmmod","glmmod"))
  
  #Mixed model Approach
  dfNZ <- as.vector(lassoCoeffs[abs(lassoCoeffs$coeff) > 0,]$feature)
  #totalPossibleVars = totalPossibleVars + length(dfNZ)
  
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
        
      }))
      
    } 
    else if (maxVars == (length(dfNZ)-1))
    {
      break
    }
    
  }
  
  if (exists("dfImput"))
  {
    return(dfImput)
  }
  else
  {
    dfImput <- c(0)
    return(dfImput)
  }
}

#Cluster SetUp
#cl <- makeCluster(3)
#clusterExport(cl,c('imputation'))

#Testing
# print(sprintf("Current Time: %s",as.POSIXlt(Sys.time())))
# imputeSet <- sapply(1:length(missColsTrans),imputation, missColsTrans, colNames, dtTrain, dtPred, numIters)
# print(sprintf("Current Time: %s",as.POSIXlt(Sys.time())))
# 
# print(sprintf("Current Time: %s",as.POSIXlt(Sys.time())))
# imputeSet <- parSapply(cl, 1:length(missColsTrans),imputation, missColsTrans, colNames, dtTrain, dtPred, numIters)
# print(sprintf("Current Time: %s",as.POSIXlt(Sys.time())))
# stopCluster(cl)
# 

