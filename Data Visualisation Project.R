library(ggplot2)
library(ggthemes)
library(data.table)
df <- fread("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/R Course Notes/R for Data-Science and-Machine Learning/Projects/Data Visualization Project/Economist_Assignment_Data.csv",drop = 1)


pl <- ggplot(df, aes(x=CPI, y=HDI)) 
pl2<- pl + geom_point(aes(colour = Region),size = 5, pch = 1) 
pl3 <- pl2 + geom_smooth(aes(group = 1), method = "lm", formula = y~log(x), se = F, colour = "red") 

points.to.label <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                     "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                     "India", "Italy", "China", "South Africa", "Spane",
                     "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                     "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                     "New Zealand", "Singapore")

pl4 <- pl3 + geom_text(aes(label = Country), data = subset(df,Country %in% points.to.label), check_overlap = T) 
pl5 <- pl4 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10 = least corrupt)", limits = c(0.9,10.2), breaks = 1:10) 
pl6 <- pl5 + scale_y_continuous(name = "Human Development Index, 2011 (1 = Best)", limits = c(0.2,1))
pl7 <- pl6 + ggtitle("Corruption and Human Development") + theme_economist_white()

print(pl7)