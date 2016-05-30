md.pattern2 <- function (x) 
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
  
  #Iterate through each row
  x <- data.frame(x)
  xZero <- data.frame(matrix(0,n,1))
  names(xZero)[1] <- "V1"
  curPatt <- 2
  
  while (sum(xZero==0) >= 1)
  {
    for (i in 1:nrow(xZero))
    {
      if (xZero[i,1] == 0)
      {
        curRow = i
        break
      }
    }
    
    missCols <- c(0)
    complete <- 1
    
    for (j in 1:p)
    {
      if (is.na(x[curRow,j]))
      {
        complete <- 0
        missCols <- c(missCols, j)
      }
    }
    
    if (complete == 0){
      missCols <- missCols[2:length(missCols)]
      for (i in curRow:nrow(x))
      {
        if (xZero[i,1] == 0)
        {
          numColsMiss <- 0
          isCase <- TRUE
          for (j in 1:p)
          {
            if (is.na(x[i,j]))
            {
              if (sum(missCols==j) == 1){
                numColsMiss <- numColsMiss + 1
              } else{
                isCase <- FALSE
                break
              }
            }
          }
          
          if (numColsMiss == length(missCols) && isCase)
          {
            xZero[i,1] <- curPatt
          }
        }
        
      }
      curPatt <- curPatt + 1
    } else {
      xZero[i,1] <- 1
    }  
     
  }
  
  r <- 1* is.na(x)
  mdp <- as.matrix(xZero)
  
  #Continue with function as normal
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
#<environment: namespace:mice>
  
  
