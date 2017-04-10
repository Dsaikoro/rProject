donguri2 <- read.csv("donguri3.csv")

don2 <- donguri2[,2:3]
#最近隣法
don2.hc1 <- hclust(dist(don2), "single")
(don2.cl1 <- cutree(don2.hc1, k=3))
don2.lab <- c(rep(1,43),rep(2,46),rep(3,42))
table(don2.lab, don2.cl1)
png("clusterSingle.png")
plot(don2.hc1, hang=-1, main="single")
dev.off()
don2.cop1 <- cophenetic(don2.hc1)
cor(don2.cop1, dist(don2))  #コーフェン相関係数：0.39
don2.ta1 <- table(don2.lab, don2.cl1)
sum(diag(don2.ta1))/100     #正解率42%

#最遠隣法
don2.hc2 <- hclust(dist(don2), "complete")
(don2.cl2 <- cutree(don2.hc2, k=3))
table(don2.lab, don2.cl2)
png("clusterComplete.png")
plot(don2.hc2, hang=-1, main="complete")
dev.off()
don2.cop2 <- cophenetic(don2.hc2)
cor(don2.cop2, dist(don2))   #コーフェン相関係数：0.70
don2.ta2 <- table(don2.lab, don2.cl2)
sum(diag(don2.ta2))/100     #正解率80%

#群平均法
don2.hc3 <- hclust(dist(don2), "average")
(don2.cl3 <- cutree(don2.hc3, k=3))
table(don2.lab, don2.cl3)
png("clusterAverage.png")
plot(don2.hc3, hang=-1, main="average")
dev.off()
don2.cop3 <- cophenetic(don2.hc3)
cor(don2.cop3, dist(don2))   #コーフェン相関係数：0.84
don2.ta3 <- table(don2.lab, don2.cl3)
sum(diag(don2.ta3))/100     #正解率84%

#重心法
don2.hc4 <- hclust(dist(don2), "centroid")
(don2.cl4 <- cutree(don2.hc4, k=3))
table(don2.lab, don2.cl4)
png("clusterCentroid.png")
plot(don2.hc4, hang=-1, main="centroid")
dev.off()
don2.cop4 <- cophenetic(don2.hc4)
cor(don2.cop4, dist(don2))   #コーフェン相関係数：0.82
don2.ta4 <- table(don2.lab, don2.cl4)
sum(diag(don2.ta4))/100     #正解率88%

#メディアン法
don2.hc5 <- hclust(dist(don2), "median")
(don2.cl5 <- cutree(don2.hc5, k=3))
table(don2.lab, don2.cl5)
png("clusterMedian.png")
plot(don2.hc5, hang=-1, main="median")
dev.off()
don2.cop5 <- cophenetic(don2.hc5)
cor(don2.cop5, dist(don2))   #コーフェン相関係数：0.71
don2.ta5 <- table(don2.lab, don2.cl5)
sum(diag(don2.ta5))/100     #正解率44%

#ウォード法
don2.hc6 <- hclust(dist(don2), "ward.D2")
(don2.cl6 <- cutree(don2.hc6, k=3))
table(don2.lab, don2.cl6)
png("clusterWard.png")
plot(don2.hc3, hang=-1, main="ward.D2")
dev.off()
don2.cop6 <- cophenetic(don2.hc6)
cor(don2.cop6, dist(don2))   #コーフェン相関係数：0.82
don2.ta6 <- table(don2.lab, don2.cl6)
sum(diag(don2.ta6))/100     #正解率86%

#McQuitty
don2.hc7 <- hclust(dist(don2), "mcquitty")
(don2.cl7 <- cutree(don2.hc7, k=3))
table(don2.lab, don2.cl7)
png("clusterMcQuitty.png")
plot(don2.hc7, hang=-1, main="McQuitty")
dev.off()
don2.cop7 <- cophenetic(don2.hc7)
cor(don2.cop7, dist(don2))   #コーフェン相関係数：0.81
don2.ta7 <- table(don2.lab, don2.cl7)
sum(diag(don2.ta7))/100     #正解率88%
