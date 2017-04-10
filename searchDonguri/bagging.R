library(ipred)
library(kernlab)

#ランダムに学習用データとテスト用データを作成
set.seed(50)
(rowdata <- nrow(donguri1))
random_ids <- sample(rowdata, rowdata/2)
don1.train <- donguri1[random_ids,]
don1.test <- donguri1[-random_ids,]

don1.bag <- bagging(type~., data=don1.train, nbagg=40)
don1.bagprd <- predict(don1.bag, don1.test[,-3], type="class") 

(don1.bagt <- table(don1.test[,3], don1.bagprd))

sum(diag(don1.bagt))/sum(don1.bagt)

don2.train <-donguri2[random_ids,]
don2.test <- donguri2[-random_ids,]

don2.bag <- bagging(type~., data=don2.train, nbagg=40)
don2.bagprd <- predict(don2.bag, don2.test[,-3], type="class")
(don2.bagt <- table(don2.test[,3], don2.bagprd))
sum(diag(don2.bagt))/sum(don2.bagt)
