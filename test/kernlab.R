#参考：R言語でSVMによる分類学習
#url(http://yut.hatenablog.com/entry/20120827/1346024147)
library(kernlab)

rowdata <- nrow(iris)

random_ids <- sample(rowdata, rowdata*0.5)
random_ids

iris_train <- iris[random_ids,]
iris_train

iris_pred <- iris[-random_ids,]

iris_svm <- ksvm(Species~., data=iris_train)
iris_svm

result_pred <- predict(iris_svm, iris_pred)
result_pred

plot(iris_svm, data=iris_train)
table(result_pred, iris_pred$Species)


#参考：Rとカーネル方・サポートベクターマシン
#url(https://www1.doshisha.ac.jp/~mjin/R/31/31.html)
library(kernlab)
x <- as.matrix(iris[,1:4])
iris.kpc1 <- kpca(x,kernel="rbfdot",features=2,kpar=list(sigma=0.1))
plot(pcv(iris.kpc1), col=as.integer(iris[,5]))

iris.kpc2 <- kpca(x, kernel="polydot", features=2, kpar=list(degree=1))
plot(pcv(iris.kpc2), col=as.integer(iris[,5]))

predict(kpc(iris.kpc1),new.data)

set.seed(10)
y <- as.matrix(iris[51:150,5])
iris1 <- data.frame(iris[51:150,3:4], y)
ir.ksvm <- ksvm(y~., data=iris1)
plot(ir.ksvm, data=iris1[,1:2])
table(iris1$y, predict(ir.ksvm,iris1[,1:2]))
