library("rcompanion")
library("car")
library("fastR")
library(dbplyr)
library(ggplot2)

#Repeated Measures ANOVA - How has player salary changed over time?
#independent variable is yearID

#Creating a Dataframe of necessary columns
keeps <- c("yearID", "salary", "playerID", "teamID")
baseballSalariesM <- baseballSalaries2[keeps]
View(baseballSalariesM)

#Remove NA from df
baseballSalaries0NA <- na.omit(baseballSalariesM)
View(baseballSalaries0NA)

# Recode "teamID" so singular franchises are consistent
baseballSalaries0NA$teamID[baseballSalaries0NA$teamID == 'ANA'] <- 'LAA'
baseballSalaries0NA$teamID[baseballSalaries0NA$teamID == 'CAL'] <- 'LAA'
baseballSalaries0NA$teamID[baseballSalaries0NA$teamID == 'FLO'] <- 'MIA'
baseballSalaries0NA$teamID[baseballSalaries0NA$teamID == 'ML4'] <- 'MIL'
baseballSalaries0NA$teamID[baseballSalaries0NA$teamID == 'MON'] <- 'WAS'

View(baseballSalaries0NA)

#Re-code teamID as numeric
table(baseballSalaries0NA$teamID)
length(table(baseballSalaries0NA$teamID))
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='ANA'] <- 0
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='ARI'] <- 1
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='ATL'] <- 2
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='BAL'] <- 3
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='BOS'] <- 4
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='CAL'] <- 5
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='CHA'] <- 6
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='CHN'] <- 7
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='CIN'] <- 8
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='CLE'] <- 9
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='COL'] <- 10
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='DET'] <- 11
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='FLO'] <- 12
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='HOU'] <- 13
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='KCA'] <- 14
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='LAA'] <- 15
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='LAN'] <- 16
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='MIA'] <- 17
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='MIN'] <- 18
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='ML4'] <- 19
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='MON'] <- 20
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='NYA'] <- 21
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='NYN'] <- 22
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='OAK'] <- 23
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='PHI'] <- 24
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='PIT'] <- 25
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='SDN'] <- 26
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='SEA'] <- 27
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='SFN'] <- 28
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='SLN'] <- 29
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='TBA'] <- 30
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='TEX'] <- 31
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='TOR'] <- 32
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='WAS'] <- 33
baseballSalaries0NA$teamIDR[baseballSalaries0NA$teamID=='MIL'] <- 34

keepit <- c("yearID", "teamIDR", "salary")
teamSalaries <- baseballSalaries0NA[keepit]
View(teamSalaries)

#break down years to every 5 years or so
# Subset to only include data from 2000-2016 (the years for which we have salary information)
teamSalaries2000 <- teamSalaries[which(teamSalaries$yearID > 1999 & teamSalaries$yearID < 2017), ]

#convert yearID to factor
teamSalaries2000$yearID <- as.factor(as.numeric(teamSalaries2000$yearID))

View(teamSalaries2000)
#cant do corr now that its a factor

#Other Data Explorations
ggplot(teamSalaries2000, aes(x = factor(yearID), y = salary)) + geom_boxplot()
ggplot(baseballSalaries0NA) + geom_line(aes(x = yearID, y = salary, color = teamID)) +
ylab("Salaries Per Team") + ggtitle("MLB Salaries Per Year")
baseballSalariesAgg <- aggregate(salary~teamID, baseballSalaries0NA, mean)
baseballSalariesAgg
ggplot(baseballSalariesAgg, aes(sample = salary)) + geom_qq()
baseballSalariesAgg %>% group_by(salary) %>% summarize(count = n())

#is there a linear relationship between the increase of salary over time? salary is y
d <- ggplot(teamSalaries, aes(x = yearID, y = salary))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
#linear model, yearID as factor
lin_reg <- lm(salary ~ yearID, teamSalaries2000)
summary(lin_reg)
#linear model, yearID as numeric
lin_reg <- lm(salary ~ yearID, teamSalaries)
summary(lin_reg)

cor.test(teamSalaries$teamIDR, teamSalaries$salary, method="pearson", use = "complete.obs")
# p value = 1.604e-08, significant correlation of team and salary, 
#does this signal a relationship between the variance in spending?
#does this signal that teams are planning to spend more year to year strategically? 
#or are these correlated because there are just simply related? teams spend more or less and its different every year?

#For exporting
#library(openxlsx)
#write.xlsx(teamSalaries, "/users/anthonyzippay/documents/teamSalaries.xlsx")


#Normality
plotNormalHistogram(baseballSalaries4$salary)
baseballSalaries4$salarySQRT <- sqrt(baseballSalaries4$salary)
plotNormalHistogram(baseballSalaries4$salarySQRT)
baseballSalaries4$salaryLOG <- log(baseballSalaries4$salary)
plotNormalHistogram(baseballSalaries4$salaryLOG)

#Assumptions for ANOVA
leveneTest(salary ~ yearID, data=teamSalaries2000)
#significant, assumption not met
#DS0106, meredith said this assumption doesnt matter because the dataset is so large


#group by teamID and avg salaries and run a repeated measures ANOVA or continue with linear regression model, (avg player salaries by team)



