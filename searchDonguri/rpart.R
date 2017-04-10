#参考：決定木ー分類木
#url(http://www.trifields.jp/decision-tree-classification-tree-1012)
library(rpart)      #決定木用パッケージ
library(rpart.plot) #結果を視覚的に表示するためのパッケージ
library(partykit)   #結果を視覚的に表示するためのパッケージ


ct <- rpart(type~., data = donguri2, method = "class")
print(ct)

#標準のplot関数を使用
par(xpd = NA)
plot(ct, branch = 0.8, margin = 0.05)
text(ct, use.n = TRUE, all = TRUE)

#rpart.plot関数を使用
rpart.plot(ct, type=1, uniform = TRUE, extra = 1, under=1, faclen=0)
#partykitパッケージのas.party関数を用いてデータを変換したもの
plot(as.party(ct))

printcp(ct)
plotcp(ct)
