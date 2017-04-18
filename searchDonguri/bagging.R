library(ipred)
library(kernlab)

#ランダムに学習用データとテスト用データを作成
set.seed(50)
(rowdata <- nrow(donguri2))
random_ids <- sample(rowdata, rowdata/2)

don2.train <-donguri2[random_ids,2:4]
don2.test <- donguri2[-random_ids,2:4]

don2.bag <- bagging(type~., data=don2.train, nbagg=40)
don2.bagprd <- predict(don2.bag, don2.test[,-3], type="class")
(don2.bagt <- table(don2.test[,3], don2.bagprd))
sum(diag(don2.bagt))/sum(don2.bagt)

set.seed(80)
(rowdata3 <- nrow(donguri3))
random_ids3 <- sample(rowdata3, rowdata3/2)

don3.train <- donguri3[random_ids3, 2:4]
don3.test <- donguri3[-random_ids3, 2:4]

don3.bag <- bagging(type~., data=don3.train, nbagg=40)
don3.bagprd <- predict(don3.bag, don3.test[,-3], type="class")
(don3.bagt <- table(don3.test[,3], don3.bagprd))
sum(diag(don3.bagt))/sum(don3.bagt)

