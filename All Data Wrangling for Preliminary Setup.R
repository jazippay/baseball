# Final Project: Baseball Data Analysis
library(dplyr)
libary(Lahman)

# Eliminate unneeded columns from Batting table, namely "stint"
colnames(Batting)
keeps1 <- c("playerID", "yearID", "G", "AB", "R", "H", "X2B", "X3B", "RBI", "SB", "CS", "SO", "IBB", "SH", "SF", "GIDP")
Batting1 <- Batting[keeps1]

# Eliminate unneeded columns from "Pitching" table, namely "stint"
colnames(Pitching)
keeps2 <- c("playerID", "yearID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA")
Pitching1 <- Pitching[keeps2]

# Eliminate unneeded columns from "Fielding" table, namely "stint"
colnames(Fielding)
keeps3 <- c("playerID", "yearID", "POS", "G", "GS", "InnOuts", "PO", "A", "E", "DP", "PB", "WP", "SB", "CS", "ZR")
Fielding1 <- Fielding[keeps3]

# Eliminate unneeded columns from "People" table
colnames(People)
keeps4 <- c("playerID", "birthYear", "birthMonth", "birthDay", "birthCountry", "birthState", "birthCity", "nameFirst", "nameLast")
Players1 <- People[keeps4]
View(Players1)
View(People)
# Eliminate unneeded columns from "Salaries" table
colnames(Salaries)
keeps5 <- c("playerID", "yearID", "teamID", "salary")
Salaries1 <- Salaries[keeps5]

# Eliminate unneeded columns from "Teams" table
colnames(Teams)
keeps6 <- c("yearID", "teamID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "name", "attendance")
Teams0 <- Teams[keeps6]



Salaries1$teamID[Salaries1$teamID == 'ANA'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'CAL'] <- 'LAA'
Salaries1$teamid[Salaries1$teamID == 'FLO'] <- 'MIA'
Salaries1$teamID[Salaries1$teamID == 'ML4'] <- 'MIL'
Salaries1$teamID[Salaries1$teamID == 'MON'] <- 'WAS'

#Subsetting Columns from Batting, Pitching and Fielding to only include YearID between 1985-2016
View(Batting1)
View(Pitching1)
View(Fielding1)
View(Salaries1)
View(Teams0)

#Batting 1985-2016
batting8516 <- Batting1[ which(Batting1$yearID > 1984 & Batting1$yearID < 2017), ]
View(batting8516)
#Pitching 1985-2016
pitching8516 <- Pitching1[ which(Pitching1$yearID > 1984 & Pitching1$yearID < 2017), ]
View(pitching8516)
#Fielding 1985-2016
fielding8516 <- Fielding1[ which(Fielding1$yearID > 1984 & Fielding1$yearID < 2017), ]
View(fielding8516)
#Players 1985-2016, doesnt have yearID maybe filter by debut?
Players8516 <- Players1
# Subset "Teams" to only include teams from 1985-2016 (the years for which we have salary information)
Teams1 <- Teams0[ which(Teams0$yearID > 1984 & Teams0$yearID < 2017), ]

# Recode "teamID" so singular franchises are consistent
Teams1$teamID[Teams1$teamID == 'ANA'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'CAL'] <- 'LAA'
Teams1$teamid[Teams1$teamID == 'FLO'] <- 'MIA'
Teams1$teamID[Teams1$teamID == 'ML4'] <- 'MIL'
Teams1$teamID[Teams1$teamID == 'MON'] <- 'WAS'
View(Teams1)

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(Players1, Salaries1, by=c("playerID"), all=TRUE)

# Merge "Fielding" data frame and "baseballSalaries" data frame by "playerID"
baseball0 <- merge(fielding8516, baseballSalaries, by=c("playerID"), all=TRUE)

# Merge "Batting" data frame and "baseball0" data frame by "playerID"
baseball1 <- merge(batting8516, baseball0, by=c("playerID"), all=TRUE)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(pitching8516, baseball0, by=c("playerID"), all=TRUE)

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams1, Salaries1, by=c("teamID"), all=TRUE)
View(teamSalaries)

#Remove NA from any dataset, based on final datasets that have been merged together baseball0, baseball1, baseball2 and teamSalaries
#baseball00 <- na.omit(baseball0)
#baseball111 <- na.omit(baseball1)
#baseball22 <- na.omit(basbeall2)
#this was not working for me, something might not be right...

#Aggregate/Sum Salaries by Team
teamSalaries2 <- aggregate(teamSalaries$salary, by=list(Category=teamSalaries$teamID), FUN=sum)
View(teamSalaries2)
#Aggregate/Sum Salaries by Team and Year (SumTeamYear)
teamSalariesSTY <- aggregate(teamSalaries$salary, by=list(Category=teamSalaries$teamID, teamSalaries$yearID.x), FUN=sum)
View(teamSalariesSTY)


