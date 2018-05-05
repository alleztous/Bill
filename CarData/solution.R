# remove data if already exits
rm(Highway1)
# load package
library(carData)
library(dplyr)
library(leaps)

# response
# rate
#
# variables which is interested in
#
# acpt
# slim
# sigs1
# shld 

# log transform of some variables and response of Highway1 data set 
logTrans <- c('acpt','slim','sigs1','shld','rate')
# log tran  sform
Highway1[,logTrans] <- log(Highway1[,logTrans])

# structure of Highway1 data set
str(Highway1)

# Including Plots
# the scatter plot matrix suggests some correlation between the covariates and ..., 
plot(Highway1)

# summary 
summary(Highway1)

# scatterplot of response and four interested variables
par(mfrow = c(2,2))
plot(rate ~ acpt, data = Highway1, pch = 15); grid()
plot(rate ~ slim, data = Highway1, pch = 16); grid()
plot(rate ~ sigs1, data = Highway1, pch = 17); grid()
plot(rate ~ shld, data = Highway1, pch = 18); grid()
par(mfrow = c(1,1))

# methods
# significance of single additional variable
lapply(colnames(Highway1)[-1], function(x){
  summary(lm(as.formula(paste0('rate ~ ' , x)), data = Highway1))
})

#
n <- nrow(Highway1)
train <- rep(TRUE,n)
p <- 0.3
train[sample(n,round(0.3*n))] <- FALSE

hwTrain <- Highway1[train,]
hwTest <- Highway1[-train,]

fit <- lm(rate ~ ., data = hwTrain)
coef(fit)

pred <- predict(fit, newdata = hwTest)
mse <- mean((pred - hwTest$rate)^2)

# 1.Best subsets regression
a <- regsubsets(rate ~ ., data = hwTrain)
out <- summary(a)
out$cp
plot(a, scale = 'Cp')
plot(a, scale = 'bic')

fit <- lm(rate ~ sigs1 + slim + htype, data = hwTrain)
coef(fit)

pred <- predict(fit, newdata = hwTest)
mean((pred - hwTest$rate)^2)

# len + trks + sigs1 + slim + shld + acpt + htype

# 2.Stepwise regression

# 3



# First we can do linear regression with each single variable saparately, and based on the significant results, we can pick len,trks,sigs1,slim,shld,acpt,htype seven variables, which have a p-value smaller than 0.05.
# 
# ```{r}
# # significance of single additional variable
# lapply(colnames(Highway1)[-1], function(x){
#   summary(lm(as.formula(paste0('rate ~ ' , x)), data = Highway1))
# })
# ```
# 
# For convenience, consider the factor variable htype, we think the htype MA differs from other types significantly, so we add a new column htype1 to 'Highwat1' data.
# 
# ```{r}
# plot(rate ~ htype, data = Highway1)
# Highway1$htype1 <- ifelse(Highway1$htype == 'MA','MA','other')
# ```






set.seed(1)  # for reproducible results
n <- nrow(Highway1)
p <- 0.3
train <- sample(n,round(0.3*n)) # generate index to split data

hwTrain <- Highway1[train,]  # trainning set
hwTest <- Highway1[-train,]  # testing set

fit <- lm(rate ~ len + trks + sigs1 + slim + shld + acpt + htype1, data = hwTrain)
coef(fit)

pred <- predict(fit, newdata = hwTest)
# mean square error (MSE)
mean((pred - hwTest$rate)^2)

# Best subsets regression
md <- regsubsets(rate ~ len + trks + sigs1 + slim + shld + acpt + htype1, data = hwTrain)
out <- summary(md)
out$cp
# plot(md, scale = 'Cp')
plot(md, scale = 'bic')

fit1 <- lm(rate ~ slim, data = hwTrain)
#coef(fit1)
fit2 <- lm(rate ~ trks + slim, data = hwTrain)
#coef(fit2)
fit3 <- lm(rate ~ trks + sigs1 + slim, data = hwTrain)
#coef(fit3)

pred1 <- predict(fit1, newdata = hwTest)
mean((pred1 - hwTest$rate)^2)

pred2 <- predict(fit2, newdata = hwTest)
mean((pred2 - hwTest$rate)^2)

pred3 <- predict(fit3, newdata = hwTest)
mean((pred3 - hwTest$rate)^2)








