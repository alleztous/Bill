# load package
library(Matrix)
# set directory
setwd('/Users/lemuria/Bill/PageRank/')
# load data
web <- read.table('websites.txt')
head(web)
A <- as.matrix(read.table("adjacency.txt",sep = ','))
A[1:10,1:10]
p <- 0.9
n <- nrow(A)
# (a)
r <- rowSums(A)
# (b)
R_diag <- ifelse(r > 0, p/r, 0)
print(head(R_diag,10))
# (c)
q <- (1 - p * as.numeric(r > 0))/n
print(head(q,10))
# (d)
A_sp <- Matrix(A,sparse = TRUE)
image(A_sp)

# use power method to calculate eigenvector x
P <- diag(R_diag) %*% A_sp + q %*% t(rep(1,n))

# generated initial value randomly
x <- runif(n,0,1)
x <- x/sum(x)

y <- t(P) %*% x
thresold <- 1e-5

while (TRUE) {
  y1 <- t(P) %*% y
  e <- max(abs(y1-y)) 
  # print(e)
  if(e < thresold){
    break  
  }        # if e is smaller enough, stop the loop
  y <- y1
}
x1 <- as.vector(y1[,1])
print(head(x1,20))  # print the first 20 elements of eigenvector

# use eigen function to calculate the top eigenvector directly
x <- Re(eigen(t(P))$vectors[,1])
x <- x/sum(x)
x2 <- x
print(head(x2,20))

a <- t(A) %*% t(diag(R_diag)) - diag(1,n)
b <- -rep(1,n) %*% t(q) %*% rep(1,n)
x <- base::solve(a,b)
x <- x/sum(x)
x3 <- x[,1]
print(head(x3,20))

top25 <- web[order(x2)][1:25]
print(top25)