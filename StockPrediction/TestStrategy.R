####Load kernlab####
require(kernlab)

#######StockTradingStrategy###########
######Define Parameters#####
equity <- 10000  ####Change Initial Equity Here
fee <- .000      ####Change Fee Here
strat.strat <- "Dynamic" #####Choose "Dynamic" or "Static"

######Rest of Function
UpCount <- 0
DownCount <- 0
TradeCount <- 0
act <- "Buy"
lastact <- "Buy"
length.train <- nrow(STCK.df.train)
first.open  <- STCK.df[length.train+1,2]
equity <- equity*(1-fee)
numshares <- equity/first.open
buyhold <- numshares * STCK.df[nrow(STCK.df),3] * (1-fee)


for (i in 1:(nrow(STCK.df.test)))
{
  #i = 2
  strat.df <- STCK.df[4:38]
  strat.df.train <- strat.df[i:(i+length.train-1),]
  strat.test <- STCK.df[i + length.train,]
  strat.close <- STCK.df[i + length.train,3]
  strat.open <- STCK.df[i + length.train + 1,2]
  
  ######Set Model Here##########
  if (strat.strat == "Dynamic" | i == 1)
  {
    set.seed(1)
    
    ###Target###
    #strat.fit <- ksvm(DirNDay ~ l_SPY.Close + l_UUP.Close + l_FXE.Close + 
                      #l_CPI + ExMA50,
                      #data=strat.df.train, C=1,kernel='rbfdot', prob.model=T)
    #strat.fit <- glm(DirNDay ~ l_SPY.Close + l_UUP.Close + l_FXE.Close + 
                       #l_CPI + ExMA50,
                       #data=strat.df.train, family="binomial")
    
    ###Capital One###
    #strat.fit <- ksvm(DirNDay ~l_UUP.Close + l_FXE.Close + l_CPI + MA50 + ExMA05  
                      #+ ExMA10 + BIAS10 + RSI06 + ROC10 + MACD9Score,
                      #data=strat.df.train, C=1,kernel='rbfdot', prob.model=T)
    #strat.fit <- ksvm(DirNDay ~.,
                      #data=strat.df.train, C=1,kernel='rbfdot', prob.model=T)
    strat.fit <- glm(DirNDay ~l_UUP.Close + l_FXE.Close + l_CPI + MA50 + ExMA05  
                      + ExMA10 + BIAS10 + RSI06 + ROC10 + MACD9Score,
                      data=strat.df.train,family="binomial")
    #strat.fit <- qda(DirNDay ~., data=strat.df.train, family="binomial")
  }
  
  strat.pred <- predict(strat.fit, strat.test,type="response")
  
  is.numeric(strat.pred)
  
  if (is.numeric(strat.pred) == T)
  {
    if (strat.pred < .5)
    {
      strat.pred = "Down"
    }else{
      strat.pred = "Up"
    }
  }
  
  strat.pred.len <- length(strat.pred)
  if (strat.pred.len >= 2)
  {
    strat.pred <- strat.pred$class    
  }
  
  if (i == nrow(STCK.df.test) && lastact == "Buy")
  {
    equity <- numshares*strat.close
    equity <- equity*(1-fee)
  }else if (strat.pred == "Down" && lastact != "Sell")
  {
    equity <- numshares*strat.open
    equity <- equity*(1-fee)
    act <- "Sell"
  }else if (strat.pred == "Up" && lastact != "Buy")
  {
    equity <- equity*(1-fee)
    numshares <- equity/strat.open
    act <- "Buy"
  }
  
  
  if (strat.pred == "Up")
  {
    UpCount = UpCount + 1
  }else if (strat.pred == "Down")
  {
    DownCount = DownCount + 1
  }
  
  if (act != lastact)
  {
    TradeCount = TradeCount + 1
  }
  lastact = act
  
  print(strat.pred)
  print(strat.open)
  print(strat.close)
  print(equity)
  print(TradeCount)
}
print (strat.strat)

lsRMvar <- c("i","lastact","strat.close","strat.fit","strat.pred","strat.open",
             "numshares","first.open","fee","length.train","strat.pred.len",
             "strat.strat","lsRMvar")
rm(list = lsRMvar)

###################End of Script#################
