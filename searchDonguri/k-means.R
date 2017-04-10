don2.km <- kmeans(don2, 3)
table(don2.lab, don2.km$cluster)/50 
