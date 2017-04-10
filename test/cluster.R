#参考
seiseki <- matrix(c(89,90,67,46,50,57,70,80,85,90,80,90,35,40,50,40,60,50,45,55,78,85,45,55,6,55,65,80,75,85,90,85,88,92,95), 7, 5, byrow=TRUE)
colnames(seiseki) <- c("math","science","language","english","society")
rownames(seiseki) <- c("tanaka","sato","suzuki","honda","kawabata","yoshino","saito")
seiseki
#ユークリッド距離を求める
seiseki.d <- dist(seiseki)
round(seiseki.d)

sei.d <- dist(seiseki)
(sei.hc <- hclust(sei.d))

summary(sei.hc)
sei.hc$merge

sei.hc$height
sei.hc$order
par(mfrow=c(2,2))
plot(sei.hc, main="Farthest neighbor method")
plot(sei.hc, hang=-1, main="Farthest neighbor method") #hang=-1はラベルの高さを揃える

s.hc2 <- hclust(sei.d, method="average")
plot(s.hc2, hang=-1, main="Center of gravity method")

s.hc3 <- hclust(sei.d, method="ward.D2")  #ward.D2が最新版
plot(s.hc3, hang=-1, main="Ward method")

iris2 <- iris[51:150, 1:4]
iris2.hc <- hclust(dist(iris2), "ward.D2")
(iris2.cl <- cutree(iris2.hc, k=2))
iris2.lab <- c(rep(1,50), rep(2,50))
table(iris2.lab, iris2.cl)

iris2.cop <- cophenetic(iris2.hc)
cor(iris2.cop, dist(iris2))

iris2.ta <- table(iris2.lab, iris2.cl)
sum(diag(iris2.ta))/100
plot(iris2.hc, hang=-1, main="ward.D2")
