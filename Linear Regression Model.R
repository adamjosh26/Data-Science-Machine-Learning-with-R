library()
library(ggplot2)
library(caTools)
library(dplyr)


bike <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/Machine Learning Projects/CSV files for ML Projects/bikeshare.csv")

count_temp <- ggplot(bike, aes(x=temp, y=count)) + geom_point(aes(colour = temp),alpha = 0.2)

bike$datetime <- as.POSIXct(bike$datetime)
count_datetime <- ggplot(bike, aes(x=datetime, y=count)) + geom_point(aes(colour = temp), alpha = 0.2) + scale_colour_continuous(low = "green", high = "red")

cor(bike[,c("temp","count")])

boxpl <- ggplot(bike, aes(x=factor(season),y=count)) + geom_boxplot(aes(colour = factor(season)))

to.hour <- function(x){
  format(x, "%H")
}
bike$hour <- to.hour(bike$datetime)

count_hour_workingday <- ggplot(filter(bike, workingday==1), aes(x=hour,y=count)) + geom_point(aes(colour = temp),position = position_jitter(w=1,h=0)) + scale_colour_gradientn(colours = c("dark blue","blue","light green","yellow","orange","red"))

count_hour_nonworkingday <- ggplot(filter(bike, workingday==0), aes(x=hour,y=count)) + geom_point(aes(colour = temp),position = position_jitter(w=1,h=0)) + scale_colour_gradientn(colours = c("dark blue","blue","light green","yellow","orange","red"))

temp.model <- lm(count~temp,bike)

summary(temp.model)
 # y = 9.1705x + 6.0462 --> count against temp

predict(temp.model, data.frame(temp = c(25)))

bike$hour <- sapply(bike$hour, as.numeric)

model <- lm(count~. -datetime-atemp-casual-registered,bike)

summary(model)



