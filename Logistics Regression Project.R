adult <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")
library(dplyr)
adult <- select(adult,-X)

group_job <- function(job){
  job <- as.character(job)
  if(job == "Never-worked" | job == "Without-pay"){
    return("Unemployed")
  }else if(job == "Local-gov" | job == "State-gov"){
    return("SL-gov")
  }else if(job == "Self-emp-inc" | job == "Self-emp-not-inc"){
    return("Self-emp")
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, group_job)

group_marital <- function(status){
  status <- as.character(status)
  if(status == "Married-AF-spouse" | status == "Married-civ-spouse" | status == "Married-spouse-absent"){
    return("Married")
  }else if(status == "Divorced" | status == "Separated" | status == "Widowed"){
    return("Not-Married")
  }else{
    return(status)
  }
}

adult$marital <- sapply(adult$marital, group_marital)

Asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos", "Philippines", "Taiwan", "Thailand", "Vietnam")
North.America <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "United-States")
South.America <- c("Columbia", "Ecuador", "Peru", "Trinadad&Tobago")
Europe <- c("England", "France","Germany", "Greece", "Holand-Netherlands", "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland", "Yugoslavia")
Others <- c("South")

group_country <- function(country){
  if(country %in% Asia){
    return("Asia")
  }else if(country %in% North.America){
    return("North America")
  }else if(country %in% South.America){
    return("South America")
  }else if(country %in% Europe){
    return("Europe")
  }else{return("Other")}
}


adult$country <- sapply(adult$country, group_country)

adult$type_employer <- factor(adult$type_employer)
adult$marital <- factor(adult$marital)
adult$country <- factor(adult$country)


library(Amelia)

adult[adult == "?"] <- NA

#missmap(adult,main = "Missing Map", y.at=c(1), y.labels = c(" "), col=c("yellow", "black"))

adult <- na.omit(adult)

adult$income <- factor(adult$income)

#missmap(adult,main = "Missing Map", y.at=c(1), y.labels = c(" "), col=c("yellow", "black"))

library(ggplot2)
library(ggthemes)

#ggplot(adult,aes(age)) + geom_histogram(aes(fill = income), colour = "black",binwidth = 1)

#ggplot(adult, aes(hr_per_week)) + geom_histogram()

adult <- adult %>% rename(region = country)
#adult <- rename(adult, region = country)

#ggplot(adult, aes(region)) + geom_bar(aes(fill = income), colour = "black")

library(caTools)

set.seed(101)
split <- sample.split(adult$income, SplitRatio = 0.7)

train <- subset(adult, split == T)
test <- subset(adult,split == F)

model <- glm(income~., family = binomial(link = "logit"),data = train)

summary(model)
#new.model <- step(model)
#summary(new.model)


test$predicted.income <- predict(model, test, type = "response")

table(test$income,test$predicted.income>0.5)

results <- ifelse(test$predicted.income>0.5,1,0)

error <- mean(results != test$income)

print("Accuracy = ", 1-error)

#calculate recall & precision







