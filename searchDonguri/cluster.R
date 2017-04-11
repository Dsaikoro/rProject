donguri2 <- read.csv("donguri2.csv")
donguri3 <- read.csv("donguri3.csv")
don2 <- donguri2[,2:3]
don3 <- donguri3[,2,3]
#最近隣法
don2.hc1 <- hclust(dist(don2), "single")
(don2.cl1 <- cutree(don2.hc1, k=2))
don2.lab <- c(rep(1,43), rep(2,46))
table(don2.lab, don2.cl1)
png("clusterSingle2.png")
plot(don2.hc1, hang=-1, main="single")
dev.off()
don2.cop1 <- cophenetic(don2.hc1)
cor(don2.cop1, dist(don2))  #コーフェン相関係数：0.39
(don2.ta1 <- table(don2.lab, don2.cl1))
sum(diag(don2.ta1))/89     #正解率47%

don3.hc1 <- hclust(dist(don3), "single")
(don3.cl1 <- cutree(don3.hc1, k=3))
don3.lab <- c(rep(1,43),rep(2,46),rep(3,42))
table(don3.lab, don3.cl1)
png("clusterSingle3.png")
plot(don3.hc1, hang=-1, main="single")
dev.off()
don3.cop1 <- cophenetic(don3.hc1)
cor(don3.cop1, dist(don3))  #コーフェン相関係数：0.60
(don3.ta1 <- table(don3.lab, don3.cl1))
sum(diag(don3.ta1))/131     #正解率35%

 #最遠隣法
don2.hc2 <- hclust(dist(don2), "complete")
(don2.cl2 <- cutree(don2.hc2, k=2))
table(don2.lab, don2.cl2)
png("clusterComplete2.png")
plot(don2.hc2, hang=-1, main="complete")
dev.off()
don2.cop2 <- cophenetic(don2.hc2)
cor(don2.cop2, dist(don2))   #コーフェン相関係数：0.70
don2.ta2 <- table(don2.lab, don2.cl2)
sum(diag(don2.ta2))/89     #正解率90%

don3.hc2 <- hclust(dist(don3), "complete")
(don3.cl2 <- cutree(don3.hc2, k=3))
table(don3.lab, don3.cl2)
png("clusterComplete3.png")
plot(don3.hc2, hang=-1, main="complete")
dev.off()
don3.cop2 <- cophenetic(don3.hc2)
cor(don3.cop2, dist(don3))   #コーフェン相関係数：0.79
(don3.ta2 <- table(don3.lab, don3.cl2))
sum(diag(don3.ta2))/131     #正解率85%

#群平均法
don2.hc3 <- hclust(dist(don2), "average")
(don2.cl3 <- cutree(don2.hc3, k=2))
table(don2.lab, don2.cl3)
png("clusterAverage2.png")
plot(don2.hc3, hang=-1, main="average")
dev.off()
don2.cop3 <- cophenetic(don2.hc3)
cor(don2.cop3, dist(don2))   #コーフェン相関係数：0.84
(don2.ta3 <- table(don2.lab, don2.cl3))
sum(diag(don2.ta3))/89     #正解率94%

don3.hc3 <- hclust(dist(don3), "average")
(don3.cl3 <- cutree(don3.hc3, k=3))
table(don3.lab, don3.cl3)
png("clusterAverage3.png")
plot(don3.hc3, hang=-1, main="average")
dev.off()
don3.cop3 <- cophenetic(don3.hc3)
cor(don3.cop3, dist(don3))   #コーフェン相関係数：0.80
(don3.ta3 <- table(don3.lab, don3.cl3))
sum(diag(don3.ta3))/131     #正解率89%

#重心法
don2.hc4 <- hclust(dist(don2), "centroid")
(don2.cl4 <- cutree(don2.hc4, k=2))
table(don2.lab, don2.cl4)
png("clusterCentroid2.png")
plot(don2.hc4, hang=-1, main="centroid")
dev.off()
don2.cop4 <- cophenetic(don2.hc4)
cor(don2.cop4, dist(don2))   #コーフェン相関係数：0.82
(don2.ta4 <- table(don2.lab, don2.cl4))
sum(diag(don2.ta4))/89     #正解率99%

don3.hc4 <- hclust(dist(don3), "centroid")
(don3.cl4 <- cutree(don3.hc4, k=3))
table(don3.lab, don3.cl4)
png("clusterCentroid3.png")
plot(don3.hc4, hang=-1, main="centroid")
dev.off()
don3.cop4 <- cophenetic(don3.hc4)
cor(don3.cop4, dist(don3))   #コーフェン相関係数：0.80
(don3.ta4 <- table(don3.lab, don3.cl4))
sum(diag(don3.ta4))/131     #正解率99%

#メディアン法
don2.hc5 <- hclust(dist(don2), "median")
(don2.cl5 <- cutree(don2.hc5, k=2))
table(don2.lab, don2.cl5)
png("clusterMedian2.png")
plot(don2.hc5, hang=-1, main="median")
dev.off()
don2.cop5 <- cophenetic(don2.hc5)
cor(don2.cop5, dist(don2))   #コーフェン相関係数：0.72
don2.ta5 <- table(don2.lab, don2.cl5)
sum(diag(don2.ta5))/89     #正解率49%

don3.hc5 <- hclust(dist(don3), "median")
(don3.cl5 <- cutree(don3.hc5, k=3))
table(don3.lab, don3.cl5)
png("clusterMedian3.png")
plot(don3.hc5, hang=-1, main="median")
dev.off()
don3.cop5 <- cophenetic(don3.hc5)
cor(don3.cop5, dist(don3))   #コーフェン相関係数：0.79
(don3.ta5 <- table(don3.lab, don3.cl5))
sum(diag(don3.ta5))/131     #正解率85%

#ウォード法
don2.hc6 <- hclust(dist(don2), "ward.D2")
(don2.cl6 <- cutree(don2.hc6, k=2))
table(don2.lab, don2.cl6)
png("clusterWard2.png")
plot(don2.hc3, hang=-1, main="ward.D2")
dev.off()
don2.cop6 <- cophenetic(don2.hc6)
cor(don2.cop6, dist(don2))   #コーフェン相関係数：0.82
(don2.ta6 <- table(don2.lab, don2.cl6))
sum(diag(don2.ta6))/89     #正解率97%

don3.hc6 <- hclust(dist(don3), "ward.D2")
(don3.cl6 <- cutree(don3.hc6, k=3))
table(don3.lab, don3.cl6)
png("clusterWard3.png")
plot(don3.hc6, hang=-1, main="ward.D2")
dev.off()
don3.cop6 <- cophenetic(don3.hc6)
cor(don3.cop6, dist(don3))   #コーフェン相関係数：0.71
(don3.ta6 <- table(don3.lab, don3.cl6))
sum(diag(don3.ta6))/131     #正解率77%

#McQuitty
don2.hc7 <- hclust(dist(don2), "mcquitty")
(don2.cl7 <- cutree(don2.hc7, k=2))
table(don2.lab, don2.cl7)
png("clusterMcQuitty2.png")
plot(don2.hc7, hang=-1, main="McQuitty")
dev.off()
don2.cop7 <- cophenetic(don2.hc7)
cor(don2.cop7, dist(don2))   #コーフェン相関係数：0.81
(don2.ta7 <- table(don2.lab, don2.cl7))
sum(diag(don2.ta7))/89     #正解率99%

don3.hc7 <- hclust(dist(don3), "mcquitty")
(don3.cl7 <- cutree(don3.hc7, k=3))
table(don3.lab, don3.cl7)
png("clusterMcQuitty3.png")
plot(don3.hc7, hang=-1, main="McQuitty")
dev.off()
don3.cop7 <- cophenetic(don3.hc7)
cor(don3.cop7, dist(don3))   #コーフェン相関係数：0.70
(don3.ta7 <- table(don3.lab, don3.cl7))
sum(diag(don3.ta7))/131     #正解率61%
