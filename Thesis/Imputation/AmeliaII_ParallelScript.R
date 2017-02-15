#Preliminary Setup
require(Amelia)
require(plyr)
require(parallel)

#Set key variables
workingDir <-"C:/Users/eli.crane/Documents/School/Thesis/Data/TransformedData/"

perc <- 50
nimputes <- 25
empri <- .1

if (empri == 0){
  empri100 <- "00"
}else if (empri < .01){
  empri100 <- as.character(paste("00",as.integer(empri*1000),sep=""))
}else if(empri < .1){
  empri100<- as.character(paste("0",as.integer(empri*100),sep=""))
}else{
  empri100<- as.character(as.integer(empri*100))
}

#Set Working Directory
setwd(paste(workingDir,sep=""))

#Detect Number of Cores
numCore <- detectCores()-1

#Load Data
dtWork <- read.csv(file=paste("WorldBankData_ReadyForImputation_MiddleEastSouthAsia_1970_2014_",perc,"perc.csv",sep=""), 
                   header=T, sep = ",")

#Define drop cols
dropCols <- c("Key","DT.DOD.MDRI.CD", "DT.NFL.NEBR.CD") #50%

#Drop cols
dtWork <- dtWork[,!(names(dtWork) %in% dropCols)]

#Define parallel function
embarassParallel <- function(n,dtWork,empri,perc,nimputes,empri100)
{
  require(Amelia)
  
  Sys.time()
  imp.out <- amelia(dtWork,m=1,ts="year",cs="country",empri=empri*nrow(dtWork))
  Sys.time()
  save(imp.out, file=paste("ImputationParts/imputation_",perc,"perc_empri",empri100,"_p",n[1],"of",nimputes,
                           ".RData",sep=""))
}
  

#Impute in Parallel
cl <- makeCluster(numCore)
clusterExport(cl,c('embarassParallel'))

sysTimeBefore <- Sys.time()
print(paste("System time before imputation:",sysTimeBefore))
imp.out<-parLapply(cl,1:nimputes,embarassParallel,dtWork,empri,perc,nimputes,empri100)
sysTimeAfter <- Sys.time()

stopCluster(cl)

#####Combined and create final dataset#####

#Load Data Sets
load(paste("ImputationParts/imputation_",perc,"perc_empri",empri100,"_p",1,"of",nimputes,".RData",sep=""))
imp.out.final <- ameliabind(imp.out)



for(i in 2:nimputes)
{
  load(paste("ImputationParts/imputation_",perc,"perc_empri",empri100,"_p",i,"of",nimputes,".RData",sep=""))
  imp.out.final <- ameliabind(imp.out.final,imp.out)
}


#Save new dataset
save(imp.out.final, file=paste("imputation_",perc,"perc_empri",empri100,"_",nimputes,"imputes.RData",sep=""))

#Get Time
print(paste("System time after impuation:",sysTimeAfter))
print(paste("The imputation for ",perc,"% complete data with an empri of ",empri, " creating ", nimputes, 
            " datasets took ", round(difftime(sysTimeAfter,sysTimeBefore , units = c("mins")),2), " minutes.", sep=""))
