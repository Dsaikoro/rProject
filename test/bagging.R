install.packages("ipred") #bagging関数が入ったパッケージ
library(ipred)
library(kernlab) #spamデータを使用する為
data(spam )

#学習データ及びテストデータ作成
set.seed(50)
tr.num <- sample(4601, 2500)
spam.train <- spam[tr.num,]
spam.test <- spam[-tr.num,]


#bagging(formula, data, nbagg=25…)
#formula:目的変数と説明変数
#data:用いるデータ
#nbagg:繰り返しの回数を指定(デフォ値は25)
#predict(object, ...):予測関数
spam.bag <- bagging(type~., data=spam.train, nbagg=40)
spam.bagp <- predict(spam.bag, spam.test[,-58], type="class")

#判別・誤判別のクロス表の作成
(spam.bagt <- table(spam.test[,58],spam.bagp))

#正解率を求める
#diag()は対角成分を取り出す関数
sum(diag(spam.bagt))/sum(spam.bagt)
