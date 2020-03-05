# Final Project: Baseball Data Analysis

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(People, Salaries, by=c("playerID"), all=TRUE)

# Merge "Fielding" data frame and "baseballSalaries" data frame by "playerID"
baseball0 <- merge(Fielding, baseballSalaries, by=c("playerID"), all=TRUE)

# Merge "Batting" data frame and "baseball0" data frame by "playerID"
baseball1 <- merge(Batting, baseball0, by=c("playerID"), all=TRUE)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(Pitching, baseball0, by=c("playerID"), all=TRUE)

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams, Salaries, by=c("teamID"), all=TRUE)
