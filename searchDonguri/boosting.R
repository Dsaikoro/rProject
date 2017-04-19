#参考にしてた奴にはboost,adaパッケージを使うって描いてたけど...消されとるやんけぇ！
#他サイトにXgboost使ってる奴があったのでこちらを参考にする
#参考：パッケージユーザのための機械学習(12) -Xgboost-

library(ada)

don2.ada <- ada(type~., data=don2.test, iter=20)  #予測・判別
summary(don2.ada)
don2.adap <- predict(don2.ada, don2.test[,-3], type="vector")
(don2.adat <- table(don2.test[,3], don2.adap))  #判別・誤判別のクロス表を作成
sum(diag(don2.adat))/sum(don2.adat) #正解率

px <- seq(10,25, 0.1)
py <- seq(5, 15, 0.1)
pgrid <- expand.grid(px,py)
names(pgrid) <- c("tate","yoko")

don2.ada <- ada(type~., donguri2_v12, iter=20)
don2.adap <- predict(don2.ada, newdata=pgrid, type='vector')
plot(donguri2_v12[,-3],col=c(rep('red',43),rep('blue',46)),pch=19,cex=0.5,xlim=c(10,25),ylim=c(5,15))
par(new=T)
contour(px,py,array(don2.adap,dim=c(length(px),length(py))),xlim=c(10,25),ylim=c(5,15))

don3.ada <- ada(type~., data=don3.test, iter=30)
