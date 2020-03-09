library("rcompanion")
library("car")
library("fastR")
library(dbplyr)


#Repeated Measures ANOVA - How has player salary changed over time?
#indepeendent variable is yearID

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

#Re-code teamID
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

#Correlation matrix to determine related variables
res <- cor(teamSalaries)
round(res, 2)
#Actual reshaping of data
teamSalaries1 <- make.rm(constant=c("yearID", "teamIDR"), 
                      repeated=c("salary"), 
                      data=teamSalaries)
View(teamSalaries1)

#Normality
plotNormalHistogram(baseballSalaries4$salary)
baseballSalaries4$salarySQRT <- sqrt(baseballSalaries4$salary)
plotNormalHistogram(baseballSalaries4$salarySQRT)
baseballSalaries4$salaryLOG <- log(baseballSalaries4$salary)
plotNormalHistogram(baseballSalaries4$salaryLOG)

leveneTest(repdat ~ yearID*contrasts, data=teamSalaries1)
