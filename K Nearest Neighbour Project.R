library(ISLR)

standardised.Iris <- scale(iris[,1:4])

standardised.Iris <- cbind(standardised.Iris, iris[5])

library(caTools)

split <- sample.split(standardised.Iris$Species,SplitRatio = 0.7)

train.data <- subset(standardised.Iris, split == T)
test.data <- subset(standardised.Iris, split == F)

library(class)

predicted.species <- knn(train.data[-5],test.data[-5],train.data$Species, k=1)

misclassError <- mean(test.data$Species != predicted.species)
predicted.species <- NULL
error.rate <- NULL
for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train.data[-5], test.data[-5],train.data$Species, k=i)
  error.rate[i] <- mean(test.data$Species != predicted.species)
}
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

error.plot <- ggplot(error.df, aes(x=k.values,y=error.rate)) + geom_point() + geom_line(lty = "dotted", colour = "red")



