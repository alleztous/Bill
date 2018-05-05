# 编写函数求平均每天盈利
avg.profit <- function(n, nreps = 10000){
  
  # n       --   进货量
  # nreps   --   卖的天数
  
  cost <- 0   # 初始化总成本
  profit <- 0   # 初始化总利润
  
  if(n <= 100){
    cost <- nreps * 2.5 * n
  }else{
    cost <- nreps * (2.5 * 100 + 4 * (n - 100))
  }
  
  for(i in 1:nreps){
    saleVol <- rgamma(1, shape = 4.9, rate = 0.05)
    
    if(saleVol >= n){
      # 销售量超过进货量，则利润封顶，达到最大值
      profit <- profit + 8 * n   
    }else{
      profit <- profit + 8 * saleVol + 1 * (100 - saleVol)
    }
  }
  # 平均天净利润等于 (总利润-总成本)/天数
  avg_profit <- (profit - cost) / nreps
 
  return(avg_profit) 
}

n <- 0:200                                 #  进货量
p <- sapply(n, avg.profit, nreps = 10000)  #  平均日利润
n[which(p == max(p))]                      #  平均日利润最高对应的日进货量

plot(n,p,type = 'l',lwd = 2,xlab = '每日进货量',ylab = '平均日利润', main = '平均日利润 vs 每日进货量',family = 'STKaiti')
grid()
# Done
