#参考：R言語でSVMによる分類学習
library(nnet)

#nnet関数でNeuralNetworkに学習させる
don1_nnet <- nnet(type~., data=don1.train, size=2, rang=.1,decay=5e-4,maxit=200)

#未分類のデータを予測する
result_prd_nnet <- predict(don1_nnet,don1.test,type="class")

#正解と比較
table(result_prd_nnet, don1.test$type)
