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





