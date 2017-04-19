library(randomForest)
donguri2 <- read.csv("./donguri2.csv")
don.rf <- randomForest(type~., donguri2, mtry=2)
print(don.rf)

importance(don.rf)

table(donguri2$type, predict(don.rf,donguri2[-3]))

require("randomForest")

tuneRF(donguri2[,-3],donguri2[,3],doBest=T)
don.rf <- randomForest(type~., donguri2)

px <- seq(5,25,0.1)
py <- seq(5,25,0.1)
pgrid <- expand.grid(px,py)
names(pgrid) <- c("tate","yoko")

plot(donguri2[1:43,-3], col="blue", pch=19,cex=0.5,xlim=c(5,25),ylim=c(5,25))
points(donguri2[44:89,-3], col="red",pch=19,cex=0.5)
par(new=T)
contour(px,py,array(don.rf, dim=c(length(px),length(py))),xlim=c(5,25),ylim=c(5,25),col="purple",lwd=1,drawlabels=F,levels=0.5)
