############Get Packages#####################
require(quantmod)
require(xts)
require(MASS)
require(Hmisc)
require(TTR)

############Parameters########################
Stck = "COF"
FromDate = "2011-01-01"
ToDate = "2014-05-31" #Set one day ahead
numt <- 100 #number of test observations

getSymbols(Stck, from = FromDate, to = ToDate,  src="yahoo")

STCK.df <- data.frame(date=index(COF), coredata(COF)) #Replace symbol twice
rm(COF) #Replace with symbol

############StockDataFunction###################
colnames(STCK.df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
numr <- nrow(STCK.df)

STCK.dates <- data.frame(STCK.df[,1])
colnames(STCK.dates) <- c("Date")

#Calculate same day direction
STCK.df$CloseYes[2:numr] <- STCK.df$Close[1:(numr-1)]
STCK.df$Dir[2:numr] <- "Up"
STCK.df$Dir[STCK.df$CloseYes > STCK.df$Close]= "Down"
STCK.df$Dir <- as.factor(STCK.df$Dir)

#Calculate next day direction
STCK.df$DirNDay <- "Up"
for (i in 1:(nrow(STCK.df)-1))
{
  if (STCK.df$Dir[i+1] == "Down")
  {
    STCK.df$DirNDay[i] <- "Down"
  }
}
STCK.df$DirNDay <- as.factor(STCK.df$DirNDay) 
up <- nrow(STCK.df[STCK.df$DirNDay=="Up",])
down <- nrow(STCK.df[STCK.df$DirNDay=="Down",])
quant <- up/(up+down)
rm(list=c("up","down","quant"))

#Get Indicators
getSymbols("LSE.L", from = FromDate, to = ToDate, src="yahoo")
LSE.Close <- data.frame(date=index(LSE.L),coredata(LSE.L))
LSE.Close <- LSE.Close[,c(1,5)]
colnames(LSE.Close) <- c("Date","LSE.Close")
Merge.df <- merge(x=STCK.df, y=LSE.Close, by="Date",all.x=TRUE)
rm(LSE.L)
rm(LSE.Close)

getSymbols("UNG", from = FromDate, to = ToDate, src="yahoo")
UNG.Close <- data.frame(date=index(UNG),coredata(UNG))
UNG.Close <- UNG.Close[,c(1,5)]
colnames(UNG.Close) <- c("Date","UNG.Close")
Merge.df <- merge(x=Merge.df, y=UNG.Close, by="Date",all.x=TRUE)
rm(UNG)
rm(UNG.Close)

getSymbols("DGS10", from = FromDate, to = ToDate, src="FRED")
US10Y <- data.frame(date=index(DGS10),coredata(DGS10))
colnames(US10Y) <- c("Date","US10Y")
for (i in 2:nrow(US10Y))
{
  #imin = i - 1
  if (is.na(US10Y$US10Y[i]))
  {
    US10Y$US10Y[i] = US10Y$US10Y[i-1]
  }
}
Merge.df <- merge(x=Merge.df, y=US10Y, by="Date",all.x=TRUE)
rm(DGS10)
rm(US10Y)

getSymbols("DGS20", from = FromDate, to = ToDate, src="FRED")
US20Y <- data.frame(date=index(DGS20),coredata(DGS20))
colnames(US20Y) <- c("Date","US20Y")
for (i in 2:nrow(US20Y))
{
  #imin = i - 1
  if (is.na(US20Y$US20Y[i]))
  {
    US20Y$US20Y[i] = US20Y$US20Y[i-1]
  }
}
Merge.df <- merge(x=Merge.df, y=US20Y, by="Date",all.x=TRUE)
rm(DGS20)
rm(US20Y)

getSymbols("SPY", from = FromDate, to = ToDate, src="yahoo")
SPY.Close <- data.frame(date=index(SPY),coredata(SPY))
SPY.Close <- SPY.Close[,c(1,5)]
colnames(SPY.Close) <- c("Date","SPY.Close")
Merge.df <- merge(x=Merge.df, y=SPY.Close, by="Date",all.x=TRUE)
rm(SPY)
rm(SPY.Close)

getSymbols("UUP", from = FromDate, to = ToDate, src="yahoo")
UUP.Close <- data.frame(date=index(UUP),coredata(UUP))
UUP.Close <- UUP.Close[,c(1,5)]
colnames(UUP.Close) <- c("Date","UUP.Close")
Merge.df <- merge(x=Merge.df, y=UUP.Close, by="Date",all.x=TRUE)
rm(UUP)
rm(UUP.Close)

getSymbols("FXE", from = FromDate, to = ToDate, src="yahoo")
FXE.Close <- data.frame(date=index(FXE),coredata(FXE))
FXE.Close <- FXE.Close[,c(1,5)]
colnames(FXE.Close) <- c("Date","FXE.Close")
Merge.df <- merge(x=Merge.df, y=FXE.Close, by="Date",all.x=TRUE)
rm(FXE)
rm(FXE.Close)

getSymbols("USO", from = FromDate, to = ToDate, src="yahoo")
USO.Close <- data.frame(date=index(USO),coredata(USO))
USO.Close <- USO.Close[,c(1,5)]
colnames(USO.Close) <- c("Date","USO.Close")
Merge.df <- merge(x=Merge.df, y=USO.Close, by="Date",all.x=TRUE)
rm(USO)
rm(USO.Close)

getSymbols("GLD", from = FromDate, to = ToDate, src="yahoo")
GLD.Close <- data.frame(date=index(GLD),coredata(GLD))
GLD.Close <- GLD.Close[,c(1,5)]
colnames(GLD.Close) <- c("Date","GLD.Close")
Merge.df <- merge(x=Merge.df, y=GLD.Close, by="Date",all.x=TRUE)
rm(GLD)
rm(GLD.Close)

getSymbols("^VIX", from = FromDate, to = ToDate, src="yahoo")
VIX.Close <- data.frame(date=index(VIX),coredata(VIX))
VIX.Close <- VIX.Close[,c(1,5)]
colnames(VIX.Close) <- c("Date","VIX.Close")
Merge.df <- merge(x=Merge.df, y=VIX.Close, by="Date",all.x=TRUE)
rm(VIX)
rm(VIX.Close)

#Get Log differences of variables
mxLog <-matrix(0,numr,ncol(Merge.df) - 11 + 1)
for (i in 11:ncol(Merge.df))
{
  j = i - 10
  mxLog[2:nrow(mxLog),j] <- diff(log(Merge.df[,i])) 
}
colnames(mxLog) <- paste("l_",colnames(Merge.df[11:ncol(Merge.df)]), sep = "")
dfLog <- data.frame(mxLog)
Merge.df <- merge(Merge.df,dfLog,by=0,all=T)
rm(list=c("dfLog","mxLog"))
Merge.df <- Merge.df[,-1]

#Get Monthly Indexes
getSymbols("CPALTT01USQ661S", src="FRED")
CPI <- data.frame(date=index(CPALTT01USQ661S),coredata(CPALTT01USQ661S),NA)
colnames(CPI) <- c("Date","CPI","l_CPI")
Merge.CPI <- merge(x=CPI,y=STCK.dates, by="Date",all.x=TRUE,all.y=TRUE)
for (i in 1:nrow(Merge.CPI))
{
  if (i == 1)
  {
    cpi = Merge.CPI$CPI[i]
  }
  else if (!is.na(Merge.CPI$CPI[i]))
  {
    cpi = Merge.CPI$CPI[i]
    lcpi = log(Merge.CPI$CPI[i]) - log(Merge.CPI$CPI[i-1])
  }
  else
  {
    Merge.CPI$CPI[i] = cpi
    Merge.CPI$l_CPI[i] = lcpi
  }
}
Merge.df <- merge(x=Merge.df, y=Merge.CPI, by="Date",all.x=TRUE)
rm(CPALTT01USQ661S)
rm(CPI)
rm(cpi)
rm(Merge.CPI)

getSymbols("UNRATE", src="FRED")
UEMP <- data.frame(date=index(UNRATE),coredata(UNRATE),NA)
colnames(UEMP) <- c("Date","UEMP","l_UEMP")
Merge.UEMP <- merge(x=UEMP,y=STCK.dates, by="Date",all.x=TRUE,all.y=TRUE)
for (i in 1:nrow(Merge.UEMP))
{
  if (i==1)
  {
    uemp = Merge.UEMP$UEMP[i]
  }
  else if (!is.na(Merge.UEMP$UEMP[i]))
  {
    uemp = Merge.UEMP$UEMP[i]
    luemp = log(Merge.UEMP$UEMP[i]) - log(Merge.UEMP$UEMP[i-1])
  }
  else
  {
    Merge.UEMP$UEMP[i] = uemp
    Merge.UEMP$l_UEMP[i] =luemp
  }
}
Merge.df <- merge(x=Merge.df, y=Merge.UEMP, by="Date",all.x=TRUE)
rm(UNRATE)
rm(UEMP)
rm(Merge.UEMP)
rm(uemp)
rm(STCK.dates)

STCK.df <- Merge.df
rm(Merge.df)

#Simple Moving Average
STCK.df$MA05 <- SMA(STCK.df$Close,n=5)
STCK.df$MA10 <- SMA(STCK.df$Close,n=10)
STCK.df$MA20 <- SMA(STCK.df$Close,n=20)
STCK.df$MA50 <- SMA(STCK.df$Close,n=50)

#Exponential Moving Average
STCK.df$ExMA05 <- EMA(STCK.df$Close,n=5)
STCK.df$ExMA10 <- EMA(STCK.df$Close,n=10)
STCK.df$ExMA20 <- EMA(STCK.df$Close,n=20)
STCK.df$ExMA50 <- EMA(STCK.df$Close,n=50)

#BIAS
STCK.df$BIAS05 <- (STCK.df$Close - STCK.df$MA05) / STCK.df$MA05 * 100
STCK.df$BIAS10 <- (STCK.df$Close - STCK.df$MA10) / STCK.df$MA10 * 100

#Relative Strength Index
lsRSI <- c(6,9,12)
STCK.df$RSI06 <- NA
STCK.df$RSI09 <- NA
STCK.df$RSI12 <- NA

for (j in 1:length(lsRSI))
{
  begrow = lsRSI[j] + 2
  for (i in begrow:nrow(STCK.df))
  {
    l = i - lsRSI[j]
    u = i - 1
    sumup <- 0
    sumdown <- 0
    for (h in l:u)
    {
      if (STCK.df$Dir[h] == "Up"){
        sumup <- sumup + STCK.df$Close[h] - STCK.df$CloseYes[h] }
      else{
        sumdown <- sumdown + STCK.df$CloseYes[h] - STCK.df$Close[h] } 
    }
    
    if (sumdown == 0){
      RSI <- 100 
    }else if (sumup == 0){
      RSI <- 0 
    } else{
      RSt <- (sumup/lsRSI[j])/(sumdown/lsRSI[j])
      RSI <- (100 - (100/(1 + RSt)))}
    
    if (lsRSI[j] == 6)
    {
      STCK.df$RSI06[i] <- RSI
    }
    else if (lsRSI[j] == 9)
    {
      STCK.df$RSI09[i] <- RSI
    }
    else if (lsRSI[j] == 12)
    {
      STCK.df$RSI12[i] <- RSI
    }
  }
}

#Rate of Change (ROC)
lsROC <- c(5,10)
STCK.df$ROC05 <- NA
STCK.df$ROC10 <- NA

for (j in 1:length(lsROC))
{
  begrow = lsROC[j] + 1
  for (i in begrow:nrow(STCK.df))
  {
    l = i - lsROC[j]
    u = i
    ROCt <- (STCK.df$Close[u] - STCK.df$Close[l])/STCK.df$Close[l]
    
    if (lsROC[j] == 5)
    {
      STCK.df$ROC05[i] <- ROCt
    }
    else if (lsROC[j] == 10)
    {
      STCK.df$ROC10[i] <- ROCt
    }
    
  }
}

#9MACD
STCK.df$MACD9 <- NA
STCK.df$MACD9Sig <- NA
MACD <- MACD(x=STCK.df$Close,nfast = 9, nslow = 26, percent = F)
STCK.df$MACD9 <- MACD[,1]
STCK.df$MACD9Sig <-MACD[,2]
STCK.df$MACD9Score <- MACD[,1] - MACD[,2]

#Bollinger Bands
mxBol <- STCK.df[,c(3,4,5)]
s <- BBands(mxBol,n=5, sd=2)
STCK.df$BBands05 <- s[,4]
s <- BBands(mxBol,n=10, sd=2)
STCK.df$BBands10 <- s[,4]
s <- BBands(mxBol,n=20, sd=2)
STCK.df$BBands20 <- s[,4]

#Wiliams %R
lsWPR <- c(10,12)
STCK.df$WPR10 <- NA
STCK.df$WPR12 <- NA

for (j in 1:length(lsWPR))
{
  begrow = lsWPR[j] + 1
  for (i in begrow:nrow(STCK.df))
  {
    l = i - lsWPR[j]
    u = i
    lsMAX = c()
    for (h in l:u)
    {
      lsMAX= cbind(lsMAX, STCK.df$Close[h])
    }
    lsMAX = sort(lsMAX)
    min = lsMAX[1]
    max = lsMAX[length(lsMAX)]
    WPR = (max - STCK.df$Close[i])/(max - min)
    
    if (lsWPR[j] == 10)
    {
      STCK.df$WPR10[i] <- WPR
    }
    else if (lsWPR[j] == 12)
    {
      STCK.df$WPR12[i] <- WPR
    }
    
  } 
}

#Create training and testing data sets
STCK.df <- na.omit(STCK.df)
STCK.df <- STCK.df[-nrow(STCK.df),]
numr = nrow(STCK.df)
STCK.df <- STCK.df[,c(1,10,6,21:30,32,34:49,52:ncol(STCK.df))]
STCK.df.train <- STCK.df[1:(numr-numt),]
STCK.df.test <- STCK.df[(numr-(numt-1)):(numr),]

#Drop Excess Variables
lsRmInd <- c("lsWPR","lsMAX", "lsROC","lsRSI","ROCt","RSI","RSt","WPR", "MACD")
rm(list = lsRmInd)
rm(lsRmInd)

lsRmVar <- c("h","i","j","l","u","max","min","begrow","sumdown","sumup","numr","numt","lcpi","luemp","mxBol","s")
rm(list = lsRmVar)
rm(lsRmVar)

#Run Backward Elimination
null<-glm(DirNDay~1,data=STCK.df.train, family ="binomial")
full<-glm(DirNDay ~ .,data=STCK.df.train, family="binomial")
step(full,scope=list(lower=null, upper=full),direction="backward")
