sale.a <- rnorm(100,73872,30000)
sale.b <- rnorm(100,73872,30200)

label <- {}
for(i in 1:length(sale.a)){
  
  label[i] <- predictLabel(sale.a[i],sale.b[i],a,b,c,d)
}

# 条形图
freq <- table(label)
barplot(freq, main="the result of ketuo classification", xlab="label", 
        ylab="Frequency", names.arg=c("负质变","负量变","正量变","正质变"))

# 饼图
lbls <- c("负质变","负量变","正量变","正质变")
lbls2 <- paste(lbls, " ", pct, "%", sep="")
pct <- prop.table(freq)*100
pie(freq,labels=lbls2,col=rainbow(length(lbls)))


df <- data.frame(sale.a, sale.b, label)
