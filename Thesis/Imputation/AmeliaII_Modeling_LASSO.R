#Preliminary Setup
require(Amelia)
require(lars)
require(glmnet)
require(plyr)
require(data.table)
require(psych)

#Define Parameters
perc <- 90
empri <- "00"
nLassos<- 25

dropCols <- c("key")
lasso.coeffs.all <- NULL
result.set <- data.frame(matrix(NA,nrow=78,ncol=nLassos))

#Set Working Directory
setwd("/Users/eli.crane/Documents/School/Thesis")

#Load Data
load(paste("Data/ModelingData/ModelingData_Final_",perc,"perc_empri",empri,"_25imputes.RData",sep=""))

#Get Initial Number of Features
featuresInit <- length(colnames(imp.model$imputations[[1]])) - 2

#Perform Lasso Models
for (impNo in 1:25)
{
  imp.model.data <- imp.model$imputations[[impNo]]
  
  
  data.train <- subset(imp.model.data,as.integer(substr(imp.model.data$key,4,8)) <= 2010)
  data.test  <- subset(imp.model.data,as.integer(substr(imp.model.data$key,4,8)) >= 2011)
  
  #Correlation
  # dropCols <- c("key","nkill_percap")
  # imp.model.corr <- data.train[,!names(data.train) %in% dropCols]
  # dtCor <- cor(imp.model.corr)
  # 
  # #Remove highly correlated variables
  # protectList <- list('nkill_percap')
  # removeList <- list()
  # 
  # for (j in 1:ncol(dtCor))
  # {
  #   pname <- colnames(dtCor)[j]
  #   if (!pname %in% removeList)
  #   {
  #     for (i in 1:nrow(dtCor))
  #     {
  #       if (abs(dtCor[i,j]) >= .90 && i != j)
  #       {
  # 
  #         if (!pname %in% protectList & !pname %in% removeList)
  #         {
  #           protectList <- c(protectList,pname)
  #         }
  # 
  #         rname <- row.names(dtCor)[i]
  #         if (!rname %in% protectList & !rname %in% removeList)
  #         {
  #           removeList <- c(removeList,rname)
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  # data.train <- data.train[,!names(imp.model.data) %in% removeList ]
  # data.test <- data.test[,!names(imp.model.data) %in% removeList]
  # 
  # #Garbage Collection
  # rm(list=c("dropCols","i","dtCor","pname","protectList",
  #           "removeList","rname","j","imp.model.corr"))
  
  ##LASSO Model
  
  #Set Seed
  set.seed(perc+impNo)
  
  #Create Inverted Matrix for Lasso Model
  dropCols <- ("key")
  dtLasso <- data.train[,!names(data.train) %in% dropCols]
  lasso.x <- as.matrix(dtLasso[,1:(ncol(dtLasso) - 1)])
  lasso.y <- as.matrix(dtLasso[,ncol(dtLasso)])
  xfactors <- model.matrix(lasso.y ~ lasso.x)[,-1]
  
  #Run Lasso and find best Lambda
  glmmod <- glmnet(xfactors,y=lasso.y,alpha=1,family='gaussian')
  cv.glmmod <- cv.glmnet(xfactors,y=lasso.y,alpha=1)
  bestLambda <- cv.glmmod$lambda.min
  bestModel <- match(bestLambda, as.vector(glmmod$lambda))
  
  #Predict
  dtLasso.test <- data.test[,!names(data.test) %in% dropCols]
  lasso.x.test <- as.matrix(dtLasso.test[,1:(ncol(dtLasso.test)-1)])
  lasso.pred <- predict(cv.glmmod,s=bestLambda,newx=lasso.x.test)
  result.set[,impNo] <- lasso.pred
  
  #Get Lasso Coeffs
  lassoCoeffs <- data.frame(coef(glmmod)[,bestModel])
  setnames(lassoCoeffs,"coeffs")
  lassoCoeffs <- data.frame(row.names(lassoCoeffs),lassoCoeffs$coeffs)
  setnames(lassoCoeffs,"row.names.lassoCoeffs.","feature")
  setnames(lassoCoeffs,"lassoCoeffs.coeffs","coeffs")
  lassoCoeffs <- lassoCoeffs[lassoCoeffs$feature != "(Intercept)" & lassoCoeffs$coeffs != 0,]
  lassoCoeffs$feature <- substr(lassoCoeffs$feature,8,length(lassoCoeffs$feature))
  

  #Final Bind
  lasso.coeffs.all <- rbind(lasso.coeffs.all,lassoCoeffs)
  
  #Garbage Collection
  rm(list=c("dtLasso","lasso.x","lasso.y","xfactors","glmmod","cv.glmmod",
            "bestLambda","bestModel","lassoCoeffs","lasso.x.test",
            "lasso.pred","imp.model.data","data.train"))
  
}

#Get summary
keepCols <- c('item','group1','n','mean','median','sd','min','max','range')
sum <- describeBy(lasso.coeffs.all$coeffs, lasso.coeffs.all$feature, mat = TRUE) 
meanFeatures <- sum(sum$n)/25
sum <- subset(sum,n==25)
sum <- sum[,names(sum) %in% keepCols]
setnames(sum,"group1","feature")
write.csv(sum,file=paste("Data/ModelingData/FinalFeatures_",perc,"perc.csv",sep=""))

#Add real values and difference
result.set$values <- dtLasso.test$nkill_percap
result.set$obs <- 1:78
result.set.compare <- result.set

result.set.compare.all <- data.frame(matrix(NA,nrow=78,ncol=2))
result.set.compare.all[,1] <- result.set.compare[,27]
result.set.compare.all[,2] <- result.set.compare[,1]

result.set.add <- data.frame(matrix(NA,nrow=78,ncol=2))
for (i in 2:25)
{
  result.set.compare[,i] <- result.set[,i] - result.set[,26]
  result.set.add[,1] <- result.set.compare[,27]
  result.set.add[,2] <- result.set.compare[,i]
  
  result.set.compare.all <- rbind(result.set.compare.all,result.set.add)
}

#Get Key Numbers
SSE <- sum((rowMeans(result.set[,1:25]) - result.set[,26])^2)
featuresFinal <- nrow(sum)

#Create Plot
plot(result.set.compare.all$X1,result.set.compare.all$X2,
     ylab = "Number of Terrorism Deaths Per 100,000", xlab="Test Case",
     main=paste("Difference Predicted Against Observed - ",100-perc,"% Missing",sep=""))
abline(h=0)

#Garbage Collection
rm(list=c("result.set.compare","result.set.add","result.set.compare.all","i",
          "data.test","dtLasso.test","keepCols","dropCols","lasso.coeffs.all"))