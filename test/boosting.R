library(kernlab)
library(ada)

data(spam)
set.seed(50)
tr.num <- sample(4601,2500)
spam.train <- spam[tr.num,]
spam.test <- spam[-tr.num,]

spam.ada <- ada(type~., data=spam.train, iter=20)

spam.adap <- predict(spam.ada, spam.test[,-58], type="vector")
(spam.adat <- table(spam.test[,58], spam.adap))
sum(diag(spam.adat))/sum(spam.adat)

plot(spam.ada, kappa=TRUE, spam.test[,-58],spam.test[,58])
spam.ada
