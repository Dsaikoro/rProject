library(xgboost)
library(Matrix)

train <- read.csv("short_prac_train.csv")
test <- read.csv("short_prac_test.csv")

train.mx <- sparse.model.matrix(label~., train)
test.mx <- sparse.model.matrix(label~., test)

dtrain <- xgb.DMatrix(train.mx, label=train$label)
dtest <- xgb.DMatrix(test.mx, label=test$label)

train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.2, max_depth=5, shbsample=1, colsample_bytree=0.5), data=dtrain, nrounds=150, watchlist=list(eval=dtest, train=dtrain))

pred <- predict(train.gdbt, newdata=dtest)
sum(diag(table(test$label,pred)))/nrow(test)

#XORデータ読み込み
xors <- read.table("xor_simple.txt",header=T)
xorc <- read.table("xor_complex.txt", header=T)

#グリッドを作る
px <- seq(-4,4,0.03)
py <- seq(-4,4,0.03)
pgrid1 <- expand.grid(px,py)
names(pgrid1) <- names(xors)[-3]

#パッケージをロード
library(xgboost)
library(Matrix)

#元データのラベルが良くないので直す
xors$label <- xors$label-1
xorc$label <- xorc$label-1

#sparse.model.matrix形式に直す
xors.mx <- sparse.model.matrix(label~.,xors)
xorc.mx <- sparse.model.matrix(label~.,xorc)
pgrid1.mx <- sparse.model.matrix(~.,pgrid1)

#xgb.DMatrix形式に直す
dxors <- xgb.DMatrix(xors.mx, label=xors$label)
dxorc <- xgb.DMatrix(xorc.mx, label=xorc$label)
dpgrid1 <- xgb.DMatrix(pgrid1.mx)

#xgboostモデルを学習させる。ここではxgb.train関数を使う記法で
xors.gdbt <- xgb.train(params=list(objective="binary:logistic",eval_metric="logloss"),data=dxors,nrounds=100)
xorc.gdbt <- xgb.train(params=list(objective="binary:logistic",eval_metric="logloss"),data=dxorc,nrounds=100)

#シンプルパターン
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
rect(-4,0,0,4,col='#ffaaaa')
rect(-4,-4,0,0,col='#aaaaff')
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xors[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=1)
par(new=T)
contour(px,py,array(predict(xors.gdbt,newdata=dpgrid1),dim=c(length(px),length(py))),levels=0.5,drawlabels=T,col='purple',lwd=2,xlim=c(-4,4),ylim=c(-4,4))

#複雑パターン
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
rect(-4,0,0,4,col='#ffaaaa')
rect(-4,-4,0,0,col='#aaaaff')
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xorc[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=1)
par(new=T)
contour(px,py,array(predict(xorc.gdbt,newdata=dpgrid1),dim=c(length(px),length(py))),levels=0.5,drawlabels=T,col='purple',lwd=2,xlim=c(-4,4),ylim=c(-4,4))

#ややオーバーフィッティング強め
xorc.gdbt <- xgb.train(params=list(objective="binary:logistic",eval_metric="logloss",eta=1,max_depth=8),data=dxorc,nrounds=100)
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
rect(-4,0,0,4,col='#ffaaaa')
rect(-4,-4,0,0,col='#aaaaff')
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xorc[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=1)
par(new=T)
contour(px,py,array(predict(xorc.gdbt,newdata=dpgrid1),dim=c(length(px),length(py))),levels=0.5,drawlabels=T,col='purple',lwd=2,xlim=c(-4,4),ylim=c(-4,4))

#やや汎化強め
xorc.gdbt <- xgb.train(params=list(objective="binary:logistic",eval_metric="logloss",eta=0.1,max_depth=4),data=dxorc,nrounds=100)
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
rect(-4,0,0,4,col='#ffaaaa')
rect(-4,-4,0,0,col='#aaaaff')
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xorc[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=1)
par(new=T)
contour(px,py,array(predict(xorc.gdbt,newdata=dpgrid1),dim=c(length(px),length(py))),levels=0.5,drawlabels=T,col='purple',lwd=2,xlim=c(-4,4),ylim=c(-4,4))

