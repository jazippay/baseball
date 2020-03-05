# Final Project: Baseball Data Analysis

# Eliminate unneeded columns from Batting table, namely "stint"
keeps1 <- c("playerID", "yearID", "teamID", "lgID", "G", "AB", "R", "H", "X2B", "X3B", "RBI", "SB", "CS", "SO", "IBB", "SH", "SF", "GIDP")
Batting1 <- Batting[keeps1]

# Eliminate unneeded columns from Pitching table, namely "stint"
keeps2 <- c("playerID", "yearID", "teamID", "lgID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA")
Pitching1 <- Pitching[keeps2]

# Eliminate unneeded columns from Fielding table, namely "stint"
keeps3 <- c("playerID", "yearID", "teamID", "lgID", "POS", "G", "GS", "InnOuts", "PO", "A", "E", "DP", "PB", "WP", "SB", "CS", "ZR")
Fielding1 <- Fielding[keeps3]

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(People, Salaries, by=c("playerID"), all=TRUE)

# Merge "Fielding" data frame and "baseballSalaries" data frame by "playerID"
baseball0 <- merge(Fielding1, baseballSalaries, by=c("playerID"), all=TRUE)

# Merge "Batting" data frame and "baseball0" data frame by "playerID"
baseball1 <- merge(Batting1, baseball0, by=c("playerID"), all=TRUE)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(Pitching1, baseball0, by=c("playerID"), all=TRUE)

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams, Salaries, by=c("teamID"), all=TRUE)