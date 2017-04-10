#睡眠データ
x = with(sleep, extra[group == 1])  
y = with(sleep, extra[group == 2])
boxplot(x, y)

summary(x)  #最小値, 25%, 中央値, 平均値, 75%, 最大値
sd(x)
