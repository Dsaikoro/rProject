install.packages("randomForest")
library(randomForest)
spam.rf <- randomForest(type~., data=spam.train, na.action="na.omit")
print(spam.rf)
summary(spam.rf)
spam.rf$type
plot(spam.rf)

spam.rf$err.rate

spam.rf$importance
varImpPlot(spam.rf)

spam.rfp <- predict(spam.rf,spam.test[,-58])
(spam.rft<-table(spam.test[,58],spam.rfp))
sum(diag(spam.rft))/sum(spam.rft)
