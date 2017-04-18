library(ada)
spam.ada <- ada(type~.,data=spam.train, iter=20)
spam.adap <- predict(spam.ada,spam.test[,-58],type="vector")
(spam.adat<-table(spam.test[,58],spam.adap))
sum(diag(spam.adat))/sum(spam.adat)
plot(spam.ada, kappa=TRUE, spam.test[,-58], spam.test[,58])

xors <- read.table("xor_simple.txt", header=TRUE, quote="\"")
xorc <- read.table("xor_complex.txt", header=TRUE, quote="\"")

xors$label <- as.factor(xors$label-1)
xorc$label <- as.factor(xorc$label-1)

px <- seq(-4, 4, 0.03)
py <- seq(-2, 4, 0.03)
pgrid <- expand.grid(px,py)

names(pgrid) <- c("x", "y.1")

xors.ada <- ada(label~., xors, iter=20)
xors.adap <- predict(xors.ada, newdata=pgrid, type='vector')
plot(xors[,-3], col=c(rep('red',50),rep('blue',50)),pch=19,cex=1,xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
contour(px, py, array(xors.adap, dim=c(length(px),length(py))),xlim=c(-4,4),ylim=c(-4,4),col="purple",lwd=3,drawlabels=F)
