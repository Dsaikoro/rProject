#参考：Rdiscriminant-判別分析-
#url(http://tmubdell.math.se.tmu.ac.jp/fukunaga/index.php?Rdiscriminant)

#現在の作業ディレクトリを調べる
getwd()
setwd("/Users/nakanodaisuke/git/rProject/searchDonguri")
#ファイル読み込み
donguri1 <- read.csv("donguri1.csv", header=TRUE)
donguri2 <- read.csv("donguri2.csv", header=TRUE)

donguri1
plot(donguri1$tate, donguri1$yoko, pch=donguri1[,3]+16, main="Figure 1")

plot(donguri2$tate, donguri2$yoko, pch=donguri2[,3]+16, main="Figure 2")

library(MASS)
c1<-lda(type~tate+yoko, donguri1)
c2 <- predict(c1)

print(c1)
print(c2)

c1$scaling

ldaline(c1)

c3<-ldaline(c1)

png("donguri1.png") #open device

plot(donguri1$tate, donguri1$yoko, pch=donguri1$type+16)
abline(c3,col=2)

dev.off() #close device
