# Final Project: Baseball Data Analysis

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

# Eliminate unneeded columns from "Salaries" table
colnames(Salaries)
keeps5 <- c("playerID", "yearID", "teamID", "salary")
Salaries1 <- Salaries[keeps5]

# Eliminate unneeded columns from "Teams" table
colnames(Teams)
keeps6 <- c("yearID", "teamID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "name", "attendance")
Teams0 <- Teams[keeps6]

# Subset "Teams" to only include teams from 1985-2016 (the years for which we have salary information)
Teams1 <- Teams0[1918:2835, ]

# Recode "teamID" so singular franchises are consistent
Teams1$teamID[Teams1$teamID == 'ANA'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'CAL'] <- 'LAA'
Teams1$teamid[Teams1$teamID == 'FLO'] <- 'MIA'
Teams1$teamID[Teams1$teamID == 'ML4'] <- 'MIL'
Teams1$teamID[Teams1$teamID == 'MON'] <- 'WAS'

Salaries1$teamID[Salaries1$teamID == 'ANA'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'CAL'] <- 'LAA'
Salaries1$teamid[Salaries1$teamID == 'FLO'] <- 'MIA'
Salaries1$teamID[Salaries1$teamID == 'ML4'] <- 'MIL'
Salaries1$teamID[Salaries1$teamID == 'MON'] <- 'WAS'

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(Players1, Salaries1, by=c("playerID"), all=TRUE)

# Merge "Fielding" data frame and "baseballSalaries" data frame by "playerID"
baseball0 <- merge(Fielding1, baseballSalaries, by=c("playerID"), all=TRUE)

# Merge "Batting" data frame and "baseball0" data frame by "playerID"
baseball1 <- merge(Batting1, baseball0, by=c("playerID"), all=TRUE)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(Pitching1, baseball0, by=c("playerID"), all=TRUE)

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams1, Salaries1, by=c("teamID"), all=TRUE)