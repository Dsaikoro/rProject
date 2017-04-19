install.packages("~/Downloads/mvpart_1.6-2.tar",repos=NULL,type="source")
require("mvpart")
#シンプルなケース
d1 <- read.table("xor_simple.txt", header=T)

d1.rp <- rpart(label~., d1)
#rpart()で決定木。目的変数とせつめいへんすうのくみあわs目的変数と説明変数の組み合わせはformula式で与えられる
plot(d1.rp,uniform=T,margin=0.2)
text(d1.rp, uniform=T, use.n=T,all=F)
#最新バージョンではtext()にもuniform引数が必要なので注意

plot(d1[1:50,-3],col="blue",pch=19,cex=1,xlim=c(-3,3),ylim=c(-3,3))
points(d1[51:100,-3],col="red",pch=19,cex=1)
#一旦プロットしてみた
px<-seq(-3,3,0.03)
py<-seq(-3,3,0.03)
pgrid<-expand.grid(px,py)
names(pgrid)<-c("x","y")
#コンタープロットを使うので、メッシュグリッドを切る
out1<-predict(d1.rp,pgrid,type="vector")
#予測する＝コンターのデータを作る
par(new=T)
contour(px,py,array(out1,dim=c(length(px),length(py))),xlim=c(-3,3),ylim=c(-3,3),col="purple",lwd=3,drawlabels=F)
#コンタープロットで決定境界線を書く

#複雑なケース
d2 <- read.table("xor_complex.txt", header=T)
d2.rp<-rpart(label~.,d2)
#まず決定技モデルを算出
plot(d2[1:50,-3],col="blue",pch=19,cex=1,xlim=c(-3,3),ylim=c(-3,3))
points(d2[51:100,-3],col="red",pch=19,cex=1)
#一旦プロットしておく
out2<-predict(d2.rp,pgrid,type="vector")

par(new=T)
contour(px,py,array(out2,dim=c(length(px),length(py))),xlim=c(-3,3),ylim=c(-3,3),col="purple",lwd=3,drawlabels=F)


data(iris)
iris.rp<-rpart(Species~., iris)
plot(iris.rp, uniform=T, margin=0.2)
text(iris.rp, uniform=T, use.n=T, all=F)

plotcp(iris.rp)

iris.rp2 <- rpart(Species~., iris, cp=0.094)
plot(iris.rp2, uniform=T, margin=0.2)
text(iris.rp2, uniform=T, use.n=T, all=F)
