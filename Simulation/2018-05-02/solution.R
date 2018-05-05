# define function
IntervalDist <- function(obj,N = 10000){
  L <- 0; U <- 80; n <- 18
  Interval <- c()
  t <- 1
  res <- c()
  for(i in 1:N){
    x <- sort(sample(L:U, n))
    x1 <- as.character(sum(x[1:6]))
    x2 <- as.character(sum(x[7:12]))
    x3 <- as.character(sum(x[13:18]))
    res[i] <- as.numeric(substr(x1,nchar(x1),nchar(x1))) + 
      as.numeric(substr(x2,nchar(x2),nchar(x2))) + 
      as.numeric(substr(x3,nchar(x3),nchar(x3)))
  }
  Interval <- diff(which(res %in% obj))
  return(Interval)
}

x <- IntervalDist(c(13,15),N = 1000000)
max(x)
hist(x,300)

x <- IntervalDist(c(11,14,16),N = 100000)
max(x)
hist(x,300)





