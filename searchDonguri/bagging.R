library(ipred)

donguri2 <- read.csv("./donguri2.csv")
#ランダムに学習用データとテスト用データを作成
set.seed(50)  #乱数のシード値設定
(rowdata <- nrow(donguri2)) #データの行数を取得
random_ids <- sample(rowdata, rowdata/2) #idの半分をランダムに抽出

don2.train <-donguri2[random_ids,]  #学習データ
don2.test <- donguri2[-random_ids,] #テストデータ

don2.bag <- bagging(type~., data=don2.train, nbagg=40)  #予測・判別
don2.bagprd <- predict(don2.bag, don2.test[,-3], type="class")
(don2.bagt <- table(don2.test[,3], don2.bagprd))  #判別・誤判別のクロス表を作成
sum(diag(don2.bagt))/sum(don2.bagt) #正解率


set.seed(50)
(rowdata3 <- nrow(donguri3))
random_ids3 <- sample(rowdata3, rowdata3/2)

don3.train <- donguri3[random_ids3, 2:4]
don3.test <- donguri3[-random_ids3, 2:4]

don3.bag <- bagging(type~., data=don3.train, nbagg=40)
don3.bagprd <- predict(don3.bag, don3.test[,-3], type="class")
(don3.bagt <- table(don3.test[,3], don3.bagprd))
sum(diag(don3.bagt))/sum(don3.bagt)

