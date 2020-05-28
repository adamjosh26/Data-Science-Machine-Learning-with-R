batting <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/R Course Notes/R for Data-Science and-Machine Learning/Projects/Capstone Project/Batting.csv")
batting$BA <- batting$H/batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- (batting$X1B + (2*batting$X2B) + (3*batting$X3B) + (4*batting$HR))/batting$AB
batting <- subset(batting, yearID >=1985)

sal <- read.csv("/Users/adayummmm/Desktop/Certifications/Data Science & Machine Learning with R/R Course Notes/R for Data-Science and-Machine Learning/Projects/Capstone Project/Salaries.csv")

combo <- merge(batting,sal, c("playerID","yearID"))

star_players <- c("giambja01", "damonjo01", "saenzol01")

lost_players <- subset(combo, playerID %in% star_players)
lost_players <- subset(lost_players, yearID == 2001)

lost_players <- lost_players[c("playerID", "H", "X2B", "X3B", "HR", "OBP", "SLG", "BA","AB")]

# Sum of AB 1469
# AVG OBP 0.364
# Total Salary 15 million

combo <- subset(combo, yearID == 2001)
combo <- subset(combo, salary < 8000000 & OBP >0 & AB >= 450)

pl <- ggplot(combo, aes(x=OBP, y=salary)) + geom_point(size = 2)

print(pl)

library(dplyr)

options <- head(arrange(combo, desc(OBP)))
                
options[,c('playerID','AB','salary','OBP')]
          