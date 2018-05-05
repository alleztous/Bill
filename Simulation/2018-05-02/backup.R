# simulation
L <- 0
U <- 80
n <- 18
N <- 100000
res <- c()

for(i in 1:N){
  # take 18 numbers from 0 to 80 randomly
  x <- sort(sample(L:U, n))
  x1 <- as.character(sum(x[1:6]))
  x2 <- as.character(sum(x[7:12]))
  x3 <- as.character(sum(x[13:18]))
  res[i] <- as.numeric(substr(x1,nchar(x1),nchar(x1))) + 
    as.numeric(substr(x2,nchar(x2),nchar(x2))) + 
    as.numeric(substr(x3,nchar(x3),nchar(x3)))
}
