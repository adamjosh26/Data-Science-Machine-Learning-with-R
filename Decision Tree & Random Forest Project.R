library(ISLR)
df <- College

library(ggplot2)

ggplot(df, aes(x=Room.Board,y=Grad.Rate)) + geom_point(aes(colour = Private))

ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill = Private),colour = "black", bins = 50)

ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill = Private),colour = "black", bins = 50)

subset(df, Grad.Rate >100)

df["Cazenovia College", "Grad.Rate"] <- 100

library(caTools)
set.seed(101)
split <- sample.split(df$Grad.Rate, SplitRatio = 0.7)

train <- subset(df, split == T)
test <- subset(df, split == F)

library(rpart)

tree <- rpart(Private~., method = "class", data = train)

predicted.tree <- predict(tree, test)

predicted.tree <- as.data.frame(predicted.tree)

Label <- function(p){
    if(p >=0.5){
      return("Yes")
    }else{
      return("No")
    }
}

predicted.tree$Private <- sapply(predicted.tree$Yes, Label)

table(predicted.tree$Private, test$Private)

library(rpart.plot)

prp(tree)

library(randomForest)

model <- randomForest(Private~., importance = T, data = train)

p <- predict(model, test)

table(p,test$Private)

