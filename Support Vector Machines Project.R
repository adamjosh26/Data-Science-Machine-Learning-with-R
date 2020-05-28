df <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/loan_data.csv")

df$inq.last.6mths <- factor(df$inq.last.6mths)
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec)
df$not.fully.paid <- factor(df$not.fully.paid)
df$credit.policy <- factor(df$credit.policy)

library(ggplot2)
library(ggthemes)

ggplot(df, aes(fico)) + geom_histogram(aes(fill = not.fully.paid), colour = "black")

ggplot(df, aes(purpose)) + geom_bar(aes(fill = not.fully.paid),position = "dodge", colour = "black")

ggplot(df,aes(int.rate,fico)) + geom_point(aes(colour = not.fully.paid),alpha = 0.4)

library(caTools)
library(e1071)

split <- sample.split(df$not.fully.paid, SplitRatio = 0.7)
train <- subset(df, split ==T)
test <- subset(df, split == F)

model <- svm(not.fully.paid~., train)
summary(model)

predicted.model <- predict(model, test[1:13])

table(predicted.model,test$not.fully.paid)

tuned.results <- tune(svm, train.x = not.fully.paid~., data = train, kernel = "radial", ranges = list(cost=c(1,10),gamma=c(0.1,1)))

tuned.svm <- svm(not.fully.paid~., train, kernel = "radial", gamma = 0.5, cost = 0.1)

predicted.values <- predict(tuned.svm, test[1:13])

table(predicted.values, test$not.fully.paid)

