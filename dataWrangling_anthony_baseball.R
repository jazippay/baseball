#Data Wrangling - Removing unwanted columns from source data tables

#View Source Data Tables
View(Salaries)
View(People)
View(Teams)

#Subsetting each source data tables to remove unwanted columns

#Sub Player Columns from People Dataset
keeps1 <- c("playerID", "birthYear", "birthMonth", "birthDay", "birthCountry", "birthState", "birthCity", "nameFirst", "nameLast")
players1 <- People[keeps1]

#Sub Teams
colnames(Teams)
keeps2 <- c("yearID", "teamID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "name", "park", "attendance")
teams1 <- Teams[keeps2]

#Sub Salaries Columns from Salaries Dataset
colnames(Salaries)
keeps3 <- c("yearID", "teamID", "playerID", "salary")
salaries1 <- Salaries[keeps3]

#Subsetting Columns from Batting, Pitching and Fielding to only include YearID between 1985-2016
View(Batting)
View(Pitching)
View(Fielding)

library(dplyr)
#Batting 1985-2016
batting8516 <- Batting[ which(Batting$yearID > 1984 & Batting$yearID < 2017), ]
View(batting8516)
#Pitching 1985-2016
pitching8516 <- Pitching[ which(Pitching$yearID > 1984 & Pitching$yearID < 2017), ]
View(pitching8516)
#Fielding 1985-2016
fielding8516 <- Fielding[ which(Fielding$yearID > 1984 & Fielding$yearID < 2017), ]
View(fielding8516)

#Remove NA from any dataset, based on final datasets that have been merged together baseball0, baseball1, baseball2 and teamSalaries
baseball001 <- na.omit(baseball0)
baseball100 <- na.omit(baseball1)
baseball200 <- na.omit(basbeall2)
teamSalaries300 <- na.omit(teamSalaries)
