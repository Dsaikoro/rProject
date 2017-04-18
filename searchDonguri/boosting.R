#参考にしてた奴にはboost,adaパッケージを使うって描いてたけど...消されとるやんけぇ！
#他サイトにXgboost使ってる奴があったのでこちらを参考にする
#参考：パッケージユーザのための機械学習(12) -Xgboost-

library(ada)

don2.ada <- ada(type~., data=don2.test, iter=20)
summary(don2.ada)
don2.adap <- predict(don2.ada, don2.test[,-3], type="vector")
(don2.adat <- table(don2.test[,3], don2.adap))
sum(diag(don2.adat))/sum(don2.adat)

px <- seq(5,25, 0.1)
py <- seq(5, 25, 0.1)
pgrid <- expand.grid(px,py)
names(pgrid) <- c("tate","yoko")

don2.ada <- ada(type~., donguri2_v12, iter=20)
xors.adap <- predict(don2.ada, newdata=pgrid, type='vector')
plot(donguri2_v12[,-3],col=c(rep('red',43),rep('blue',46)),pch=19,cex=0.5,xlim=c(5,25),ylim=c(5,25))
par(new=T)
contour(px,py,array(don2.adap,dim=c(length(px),length(py))),xlim=c(5,25),ylim=c(5,25))

