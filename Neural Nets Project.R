bank <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/bank_note_data.csv")

library(caTools)

split <- sample.split(bank$Class, SplitRatio = 0.7)
train <- subset(bank, split == T)
test <- subset(bank, split == F)

library(neuralnet)

nn.model <- neuralnet(Class~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train, linear.output = F, hidden = 10)

predicted.nn.values <- compute(nn.model, test[-5])

predicted.nn.values <- sapply(predicted.nn.values$net.result,round)

table(test$Class, predicted.nn.values)

library(randomForest)

bank$Class <- factor(bank$Class)

split <- sample.split(bank$Class, SplitRatio = 0.7)
train <- subset(bank, split == T)
test <- subset(bank, split == F)

rf.model <- randomForest(Class~.,train)

predicted.rf <- predict(rf.model, test[-5])

table(test$Class, predicted.rf)

