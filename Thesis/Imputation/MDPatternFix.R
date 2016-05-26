function (x) 
{
  if (!(is.matrix(x) | is.data.frame(x))) 
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2) 
    stop("Data should have at least two columns")
  if (is.data.frame(x)) 
    x <- data.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  mode(x) <- "single"
  r <- 1 * is.na(x)
  nmis <- as.integer(apply(r, 2, sum))
  names(nmis) <- dimnames(x)[[2]]
  
  maxVars <- 50
  numDivs <- ceiling(p/maxVars)
  mdpMatrix <- matrix(0,n,1)
  decInc <- .1
  j <- 0
  inc <- 0
  
  while(j < numDivs)
  {
    #Split Data Frame
    j = j + 1
    if (maxVars + (maxVars * (j - 1)) <= p) {
      xx <- x[,((1+maxVars * (j-1)):(maxVars + maxVars * (j-1)))]
    } else {
      xx <- x[,((1+maxVars * (j-1)):((maxVars*(j-1)) + abs(p - maxVars)))]
    }
    
    #Find Unique Patterns of Missing Variables
    r <- 1 * is.na(xx)
    mdpTemp <- (r %*% (2^((1:ncol(xx)) - 1))) + 1
    mdpTempUnique <- data.frame(unique(mdpTemp))
    names(mdpTempUnique)[1] <- "Value"
    mdpTempUnique <- data.frame(mdpTempUnique[order(mdpTempUnique$Value),])
    mdpTempUnique$Pair <- 0
    
    for (i in 1:nrow(mdpTempUnique))
    {
      mdpTempUnique[i,2] =  2^(((i+inc) - 1)) + 1
    }
    inc <- inc + nrow(mdpTempUnique)
    names(mdpTempUnique)[1] <- "Value"
    
    for (i in 1:nrow(mdpMatrix))
    {
      curVal <- mdpTemp[i,1]
      curPair <- mdpTempUnique[mdpTempUnique$Value == curVal,2]
      
      mdpMatrix[i,1] <- mdpMatrix[i,1] + curPair
    }
    

  }
  
  r <- x
  mdp <- mdpMatrix
  #mdp <- (r %*% (2^((1:ncol(x)) - 1))) + 1
  
  
  
  
  ro <- order(mdp)
  x <- matrix(x[ro, ], n, p)
  mdp <- mdp[ro]
  r <- matrix(r[ro, ], n, p)
  ro <- order(ro)
  mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
  mdp <- unique(mdp)
  npatt <- length(mdpst)
  r <- 1 - r
  r <- matrix(r[mdpst, ], npatt, p)
  if (npatt == 1) 
    tmp <- format(n)
  if (npatt > 1) 
    tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
  dimnames(r) <- list(tmp, dimnames(x)[[2]])
  storage.mode(r) <- "integer"
  if (npatt > 1) 
    nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
  if (npatt == 1) 
    nmdp <- n
  co <- order(nmis)
  ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))
  r <- rbind(r[ro2, co], nmis[co])
  r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
  r
}
<environment: namespace:mice>
  
  write.csv(data.frame(mdpMatrix), file = "Data/WorkingData/mdpMatrix.csv")
