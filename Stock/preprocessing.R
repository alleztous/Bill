## 数据预处理篇
# load package
library(xlsx)
library(xts)
library(ggplot2)

file <- '/Users/lemuria/Bill/Stock/上证A股1996-2016.xls'
result <- data.frame()

# read excel sheet
for(i in 1:21){
  t <- read.xlsx(file, sheetIndex = i,colIndex = 1:3) 
  tail(t)
  if(anyNA(t)){
    print(i)
  }
  #t <- t[complete.cases(t),]
  cat('reading the ',i,'-th excel sheet...\n',sep = '')
  result <- rbind(result,t)
}

# close price plot
result <- result[order(result$日期_Date),]

# 收盘价
plot(result$收盘价_Clpr, ylab = 'log',type = 'l', lwd = 2)
# 指数收益率
plot(diff(log(result$收盘价_Clpr)),ylab = 'log return', type = 'l', lwd = 2)

# 写入文件
write.csv(result, '/Users/lemuria/Bill/Stock/szAindex.csv',row.names = FALSE,fileEncoding = 'gbk')

# 求波峰和波谷
rownames(result) <- result$日期_Date
result <- result[3]

# 转化为xts对象可视化
szAindex <- as.xts(result)
head(szAindex)
plot.xts(szAindex,grid.ticks.lwd = 0.5,grid.ticks.on = FALSE)

# ggplot2版
gg_result <- data.frame(year = rownames(result), price = result[,1])
gg_result$time <- as.Date(gg_result$year)
ggplot(data = gg_result,mapping = aes(x = time, y = price, group = 1)) + geom_line(lwd = 1.2, col = 'black')

## 求波峰和波谷篇
time <- index(szAindex)
price <- coredata(szAindex[,1])
N <- nrow(szAindex)
year <- 365
# 默认12个月为1年，即365天

# 判断任意一个数是否为波峰
isCrest <- function(index){
  i <- 1
  current_time <- time[index]
  while(TRUE){
    if(price[index] >= price[index - i] & price[index] >= price[index + i]){
      i <- i + 1
      now_time <- time[index - i]
      if(as.numeric(current_time - now_time) > year){
        return(TRUE)
      }
    }else{
      return(FALSE)
    }
  }
}

# 判断任意一个数是否为波谷
isTrough <- function(index){
  i <- 1
  current_time <- time[index]
  while(TRUE){
    if(price[index] <= price[index - i] & price[index] <= price[index + i]){
      i <- i + 1
      now_time <- time[index - i]
      if(as.numeric(current_time - now_time) > year){
        return(TRUE)
      }
    }else{
      return(FALSE)
    }
  }
}

# indexCrest为波峰索引, Trough为波谷索引
indexCrest <- c()
indexTrough <- c()
  
# 第一步，找出所有符合的波峰和波谷
for(i in (year + 1):(N - year)){
  if(isCrest(i)){
    cat('找到波峰为:\n\t')
    indexCrest <- c(indexCrest,i)
    print(time[i])
  }
  if(isTrough(i)){
    cat('找到波谷为:\n\t')
    indexTrough <- c(indexTrough,i)
    print(time[i])
  }
}

# 时间差
diff(time[sort(c(indexCrest,indexTrough))])

# 根据图可知, 去掉第1,4个波谷,去掉第3个波峰
indexCrest <- indexCrest[-3]
indexTrough <- indexTrough[-c(1,4)]

# 波峰波谷 时间
time[indexCrest]
time[indexTrough]

# 波峰波谷 价格
price[indexCrest]
price[indexTrough]

diff(time[sort(c(indexCrest,indexTrough))])

# 定义计算涨跌幅函数
regret <- function(x) return( diff(x)/x[1:(length(x)-1)]) 

# 涨跌幅
paste0(round(regret(price[sort(c(indexCrest,indexTrough))])*100,2),'%')

plot(x = time,y = price,type = 'l',xlab = 'year', ylab = 'price',axes = TRUE)
points(time[indexCrest], price[indexCrest], col = 'blue', pch = 15, cex = 1.2)
points(time[indexTrough], price[indexTrough], col = 'red', pch = 16, cex = 1.2)
legend('topleft', col = c('blue','red'),legend = c('Crest','Trough'),pch = 15:16,cex = 1)
grid()

