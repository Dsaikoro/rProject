install.packages('e1071')
iris[c(1,100,150),]

library(e1071)

train <- seq.int(1, 150, by=2)
test <- setdiff(1:150, train)

iris.svm <- svm(Species~., data=iris[train,])
iris.pred <- predict(iris.svm, iris[test,])
table(iris[test,5], iris.pred)
