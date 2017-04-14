#参考：フリーソフトによるデータ解析・マイニング　ーRと集合ー
#url(ftp://158.208.129.61/suzuki/mjin_R.pdf)
#参考：R言語でSVMによる分類学習

library(kernlab)
donguri2_v2 <- read.csv("./donguri2.csv")
#donguri2のデータの行数を取得
rowdata <- nrow(donguri2_v2)

#行数からランダムんに行番号をちゅ行数からランダムに行番号を抽出
random_ids <- sample(rowdata, rowdata/2)
random_ids

#学習データを作成
donguri2_training <- donguri2_v2[random_ids,]
donguri2_training

#予測データを作成
donguri2_predictiong <- donguri2_v2[-random_ids,]
donguri2_predictiong

#ksvm関数でトレーニングデータを学習
donguri_svm <- ksvm(type~tate+yoko, data=donguri2_training)
donguri_svm

#predict関数で予測データを評価
result_predict <- predict(donguri_svm, donguri2_predictiong)
result_predict

#予測結果と正解との比較
plot(donguri_svm, data=donguri2_v2)
table(result_predict, donguri2_predictiong$type)

donguri1 <- read.csv("donguri1_v2.csv", header = TRUE)
donguri2 <- read.csv("donguri2_v2.csv", header = TRUE)

set.seed(10)
don.ksvm <- ksvm(type~., data=donguri1_v2)
plot(don.ksvm, data=donguri1_v2)
table(donguri1_v2$type, predict(don.ksvm, donguri1_v2))
png("donguri1_svm.png")
dev.off()
