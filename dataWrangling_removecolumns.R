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