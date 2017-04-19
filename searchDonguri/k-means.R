don2 <- read.csv("./donguri2.csv")
don2.km <- kmeans(don2, 2)
table(don2.lab, don2.km$cluster)/50 
