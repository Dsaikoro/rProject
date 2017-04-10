iris2.km <- kmeans(iris2,2)
#クラスターの精度確認
table(iris2.lab,iris2.km$cluster)/50
