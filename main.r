# 基本参数设定 
# a b 是满意范围上下限
# c d 是可接受范围上下限
a <- 28553.5
b <- 73872.3
c <- 7334.8
d <- 210614.6

# y是最优点，一般选用y=b
# 描述侧距
ceju <- function(x, a, b){
  if(x < b) result <- a - x
  else if(x == b) result <- a - b
  else result <- x-b
  return(result)
}

# 表示x 与 满意范围之距
distance.good <- function(x, a, b){
  if(x < (a+b)/2 || x == (a+b)/2) result <- a - x
  else result <- x - b
  return(result)
}

# 表示x 与 可接受范围之距
distance.notbad <- function(x, c, d){
  if(x < (c+d)/2 || x == (c+d)/2) result <- c - x
  else result <- x - d 
  return(result)
}

# 描述x 与可接受范围，满意范围的区间套的关系

D <- function(x, a, b, c, d){
  result.good <- distance.good(x,a,b)
  result.notbad <- distance.notbad(x,c,d)
  if((x>b||x<a)&&result.good!=result.notbad){
    result <- result.notbad - result.good
  }
  
  else if((x<b && x>a)&&result.good!=result.notbad){
    result <- result.notbad - result.good + a - b
  }
  else 
    result <- a - b 
  
  return(result)
}

# 计算关联函数
k <- function(x,a=28553.5, b=73872.3,c=7334.8, d=210614.6){
  return(ceju(x,a,b)/D(x,a,b,c,d))
}