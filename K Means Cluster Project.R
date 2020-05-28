df1 <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/winequality-red.csv", sep = ";")
df2 <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/winequality-white.csv", sep = ";")

df1$label <- sapply(df1$pH, function(x){"red"})
df2$label <- sapply(df2$pH, function(x){"white"})

wine <- rbind(df1,df2)

library(ggplot2)


ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill = label), colour = "black", bins = 50) + scale_fill_manual(values = c('#ae4554','#faf7ea'))

ggplot(wine, aes(citric.acid)) + geom_histogram(aes(fill = label), bins = 50, colour = "black") + scale_fill_manual(values = c('#ae4554','#faf7ea'))

ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill = label), colour = "black", bins = 50) + scale_fill_manual(values = c('#ae4554','#faf7ea'))

ggplot(wine, aes(citric.acid, residual.sugar)) + geom_point(aes(colour = label), alpha = 0.4) + scale_colour_manual(values = c('#ae4554','#faf7ea'))

ggplot(wine, aes(residual.sugar, volatile.acidity)) + geom_point(aes(colour = label), alpha = 0.4) + scale_colour_manual(values = c('#ae4554','#faf7ea'))

clus.data <- wine[1:12]

wine.cluster <- kmeans(clus.data,2)

wine.cluster$centers

table(wine$label, wine.cluster$cluster)

