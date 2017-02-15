#Preliminary Setup
require(Amelia)
require(lars)
require(Zelig)
require(glmnet)
require(plyr)
require(data.table)
require(psych)

#Define Parameters
perc <- 90
empri <- "00"
nLassos<- 25
cutOff <- 25


keepCols <- c("key","SP.POP.TOTL")
dropCols <- c("year","country","country_code","SP.POP.TOTL")

#Set Working Directory
setwd("/Users/eli.crane/Documents/School/Thesis")

#Load Data
load(paste("Data/ModelingData/ModelingData_",perc,"perc_empri",empri,"_25imputes.RData",sep=""))
dtTer <- read.csv(file="Data/TransformedData/TerrorismData_Fixed.csv",header = T,sep=",")

dtTer$nkill_percap <- 0

wdPop <- imp.out.final$imputations[[1]]
wdPop$key <- paste(as.character(wdPop$country), 
                   wdPop$year,sep="")
wdPop <- wdPop[,names(wdPop) %in% keepCols]

dtTer <- merge(wdPop,dtTer,by.x="key",by.y="key",all.x=F,all.y=F)
dtTer$nkill_percap <- dtTer$nkill/dtTer$SP.POP.TOTL * 100000
dtTer$key <- paste(substr(dtTer$key,1,3),dtTer$year-1,sep="")


#dtTerFinal <- subset(dtTer,year<2011 & year > 1969)
keepCols <- c("key","nkill_percap")
dtTer <- dtTer[,names(dtTer) %in% keepCols]

rm(list=c("keepCols","wdPop"))

imp.model <- imp.out.final


#make country list
countryList <- as.character(imp.model$imputations[[1]]$country)
dtCheck <- data.frame(matrix(NA,nrow=1215,ncol=2))
#Append Data
for (i in 1:25)
{
  imp.model$imputations[[i]]$key <- paste(as.character(imp.model$imputations[[i]]$country), 
                                          imp.model$imputations[[i]]$year,sep="")
  imp.model$imputations[[i]] <- merge(imp.model$imputations[[i]],
                                          dtTer,by.x="key",by.y="key",all.x=F,all.y=F)
  imp.model$imputations[[i]] <- imp.model$imputations[[i]][!is.na(imp.model$imputations[[i]]$nkill_percap),]
  #dtCheck[,1] <- imp.model$imputations[[i]]$year.x
  #dtCheck[,2] <- as.character(imp.model$imputations[[i]]$country)
  imp.model$imputations[[i]] <- imp.model$imputations[[i]][,
                                          !names(imp.model$imputations[[i]]) %in% dropCols ]
}

#Save new dataset
save(imp.model, file=paste("Data/ModelingData/ModelingData_Final_",perc,"perc_empri",empri,"_25imputes.RData",sep=""))

names(imp.model$imputations[[i]])
#Correlation
dtCor <- cor(imp.model$imputations[[1]],use="complete.obs")
for (i in 2:25)
{
  dtCor <- ((dtCor * (i-1)) + cor(imp.model$imputations[[i]],use="complete.obs"))/i
}
#write.csv(dtCor,file="Data/WorkingData/LassoCorrs.csv")

#Remove highly correlated variables
protectList <- list('nkill_percap')
removeList <- list()

for (j in 1:ncol(dtCor))
{
  pname <- colnames(dtCor)[j]
  if (!pname %in% removeList )
  {
    for (i in 1:nrow(dtCor))
    {
      if (abs(dtCor[i,j]) >= .90 && i != j)
      {
        
        if (!pname %in% protectList & !pname %in% removeList)
        {
          protectList <- c(protectList,pname)
        }
        
        rname <- row.names(dtCor)[i]
        if (!rname %in% protectList & !rname %in% removeList)
        {
          removeList <- c(removeList,rname)
        }
      }
    }
  }
}

for (i in 1:25)
{

  imp.model$imputations[[i]] <- imp.model$imputations[[i]][,
                                                      !names(imp.model$imputations[[i]]) %in% removeList ]
}

#Garbage Collection
rm(list=c("dropCols","dtTer","i","dtCor","pname","protectList","removeList","rname","j"))

#Set Seed
set.seed(perc)

#Lasso down coefficients
lasso.coeffs.all <- NULL
for (i in 1:nLassos)
{
  #Create Inverted Matrix for Lasso Model
  dtLasso <- imp.model$imputations[[i]]
  dtLasso <- subset(dtLasso,!is.na(nkill_percap))
  lasso.x <- as.matrix(dtLasso[,1:(ncol(dtLasso) - 1)])
  lasso.y <- as.matrix(dtLasso[,ncol(dtLasso)])
  xfactors <- model.matrix(lasso.y ~ lasso.x)[,-1]
  
  #Run Lasso and find best Lambda
  glmmod <- glmnet(xfactors,y=lasso.y,alpha=1,family='gaussian')
  cv.glmmod <- cv.glmnet(xfactors,y=lasso.y,alpha=1)
  bestLambda <- cv.glmmod$lambda.min
  bestModel <- match(bestLambda, as.vector(glmmod$lambda))
  
  #Get Lasso Coeffs
  lassoCoeffs <- data.frame(coef(glmmod)[,bestModel])
  setnames(lassoCoeffs,"coeffs")
  lassoCoeffs <- data.frame(row.names(lassoCoeffs),lassoCoeffs$coeffs)
  setnames(lassoCoeffs,"row.names.lassoCoeffs.","feature")
  setnames(lassoCoeffs,"lassoCoeffs.coeffs","coeffs")
  lassoCoeffs <- lassoCoeffs[lassoCoeffs$feature != "(Intercept)" & lassoCoeffs$coeffs != 0,]
  lassoCoeffs$feature <- substr(lassoCoeffs$feature,8,length(lassoCoeffs$feature))
  

  lasso.coeffs.all <- rbind(lasso.coeffs.all,lassoCoeffs)
}

rm(list=c("dtLasso","lasso.x","lasso.y","xfactors","i","glmmod","cv.glmmod","bestLambda","bestModel",
          "lassoCoeffs"))

#Get Lasso Coeff Counts
coeffs.counts <- count(lasso.coeffs.all,'feature')
coeffs.counts <- coeffs.counts[coeffs.counts$freq >= cutOff,1]
#coeffs.counts <- coeffs.counts[coeffs.counts$freq >= cutOff,1]

keepList <- "nkill_percap"
for (i in coeffs.counts)
{
  keepList <- c(keepList,i)
}

#Get summary
sum <- describeBy(lasso.coeffs.all$coeffs, lasso.coeffs.all$feature, mat = TRUE) 
sum <- subset(sum,n==25)

#Retain relevant variables
#imp.zelig <- imp.model
#for (i in 1:25)
#{
#  imp.zelig$imputations[[i]] <- imp.model$imputations[[i]][,
#                                names(imp.model$imputations[[i]]) %in% keepList]
#  imp.zelig$imputations[[i]]$country <- dtCheck$X2
#}

#Save Data
write.table(sum,file=paste("Data/ModelingData/Lasso_",perc,".csv",sep=""),sep=",")

#Save Data
#save(imp.zelig, file=paste("Data/ModelingData/Zelig_FixedEffects_",perc,".RData",sep=""))



