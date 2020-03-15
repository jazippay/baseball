# Final Project: Baseball Data Analysis

library(Lahman)
library(dplyr)

# Subset "Teams" to only include teams from 1985-2016 (the years for which we have salary information)
Teams0 <- Teams[1918:2835, ]

# Subset "Batting" to only include data from 1985-2016 (the years for which we have salary information)
Batting0 <- Batting[which(Batting$yearID > 1984 & Batting$yearID < 2017), ]

# Subset "Pitching" to only include data from 1985-2016 (the years for which we have salary information)
Pitching0 <- Pitching[which(Pitching$yearID > 1984 & Pitching$yearID < 2017), ]

# Subset "Fielding" to only include data from 1985-2016 (the years for which we have salary information)
Fielding0 <- Fielding[which(Fielding$yearID > 1984 & Fielding$yearID < 2017), ]

# Rename player ID column in "war_daily_bat" and "war_daily_pitch" to be consistent

names(war_daily_pitch)[names(war_daily_pitch) == "player_ID"] <- "playerID"

# Subset "war_daily_bat" to only include data from after 1985
warBat <- war_daily_bat[which(war_daily_bat$year_ID >= "1980"), ]

# Subset "war_daily_bat" to only include data from after 1985
warPitch <- war_daily_pitch[which(war_daily_pitch$year_ID >= "1980"), ]

warBatNA <- na.omit(warBat)
warPitchNA <- na.omit(warPitch)

warBatNA$age <- as.numeric(warBatNA$age)
warBatNA$year_ID <- as.numeric(warBatNA$year_ID)
warBatNA$PA <- as.numeric(warBatNA$PA)
warBatNA$G <- as.numeric(warBatNA$G)
warBatNA$Inn <- as.numeric(warBatNA$Inn)
warBatNA$runs_bat <- as.numeric(warBatNA$runs_bat)
warBatNA$runs_br <- as.numeric(warBatNA$runs_br)
warBatNA$runs_dp <- as.numeric(warBatNA$runs_dp)
warBatNA$runs_field <- as.numeric(warBatNA$runs_field)
warBatNA$runs_infield <- as.numeric(warBatNA$runs_infield)
warBatNA$runs_outfield <- as.numeric(warBatNA$runs_outfield)
warBatNA$runs_catcher <- as.numeric(warBatNA$runs_catcher)
warBatNA$runs_good_plays <- as.numeric(warBatNA$runs_good_plays)
warBatNA$runs_defense <- as.numeric(warBatNA$runs_defense)
warBatNA$runs_position <- as.numeric(warBatNA$runs_position)
warBatNA$runs_position_p <- as.numeric(warBatNA$runs_position_p)
warBatNA$runs_replacement <- as.numeric(warBatNA$runs_replacement)
warBatNA$runs_above_rep <- as.numeric(warBatNA$runs_above_rep)
warBatNA$runs_above_avg <- as.numeric(warBatNA$runs_above_avg)
warBatNA$runs_above_avg_off <- as.numeric(warBatNA$runs_above_avg_off)
warBatNA$runs_above_avg_def <- as.numeric(warBatNA$runs_above_avg_def)
warBatNA$WAA <- as.numeric(warBatNA$WAA)
warBatNA$WAA_off <- as.numeric(warBatNA$WAA_off)
warBatNA$WAA_def <- as.numeric(warBatNA$WAA_def)
warBatNA$WAR <- as.numeric(warBatNA$WAR)
warBatNA$WAR_def <- as.numeric(warBatNA$WAR_def)
warBatNA$WAR_off <- as.numeric(warBatNA$WAR_off)
warBatNA$WAR_rep <- as.numeric(warBatNA$WAR_rep)
warBatNA$salary <- as.numeric(warBatNA$salary)
warBatNA$teamRpG <- as.numeric(warBatNA$teamRpG)
warBatNA$oppRpG <- as.numeric(warBatNA$oppRpG)
warBatNA$oppRpPA_rep <- as.numeric(warBatNA$oppRpPA_rep)
warBatNA$oppRpG_rep <- as.numeric(warBatNA$oppRpG_rep)
warBatNA$pyth_exponent <- as.numeric(warBatNA$pyth_exponent)
warBatNA$pyth_exponent_rep <- as.numeric(warBatNA$pyth_exponent_rep)
warBatNA$waa_win_perc <- as.numeric(warBatNA$waa_win_perc)
warBatNA$waa_win_perc_off <- as.numeric(warBatNA$waa_win_perc_off)
warBatNA$waa_win_perc_def <- as.numeric(warBatNA$waa_win_perc_def)
warBatNA$waa_win_perc_rep <- as.numeric(warBatNA$waa_win_perc_rep)
warBatNA$OPS_plus <- as.numeric(warBatNA$OPS_plus)
warBatNA$TOB_lg <- as.numeric(warBatNA$TOB_lg)
warBatNA$TB_lg <- as.numeric(warBatNA$TB_lg)
warBatNA$player_ID <- as.factor(warBatNA$player_ID)

warPitchNA$age <- as.numeric(warPitchNA$age)
warPitchNA$year_ID <- as.numeric(warPitchNA$year_ID)
warPitchNA$G <- as.numeric(warPitchNA$G)
warPitchNA$GS <- as.numeric(warPitchNA$GS)
warPitchNA$IPouts <- as.numeric(warPitchNA$IPouts)
warPitchNA$IPouts_start <- as.numeric(warPitchNA$IPouts_start)
warPitchNA$IPouts_relief <- as.numeric(warPitchNA$IPouts_relief)
warPitchNA$RA <- as.numeric(warPitchNA$RA)
warPitchNA$xRA <- as.numeric(warPitchNA$xRA)
warPitchNA$xRA_sprp_adj <- as.numeric(warPitchNA$xRA_sprp_adj)
warPitchNA$xRA_def_pitcher <- as.numeric(warPitchNA$xRA_def_pitcher)
warPitchNA$PPF <- as.numeric(warPitchNA$PPF)
warPitchNA$PPF_custom <- as.numeric(warPitchNA$PPF_custom)
warPitchNA$xRA_final <- as.numeric(warPitchNA$xRA_final)
warPitchNA$BIP <- as.numeric(warPitchNA$BIP)
warPitchNA$BIP_perc <- as.numeric(warPitchNA$BIP_perc)
warPitchNA$RS_def_total <- as.numeric(warPitchNA$RS_def_total)
warPitchNA$runs_above_avg <- as.numeric(warPitchNA$runs_above_avg)
warPitchNA$runs_above_avg_adj <- as.numeric(warPitchNA$runs_above_avg_adj)
warPitchNA$runs_above_rep <- as.numeric(warPitchNA$runs_above_rep)
warPitchNA$RpO_replacement <- as.numeric(warPitchNA$RpO_replacement)
warPitchNA$GR_leverage_index_avg <- as.numeric(warPitchNA$GR_leverage_index_avg)
warPitchNA$WAR <- as.numeric(warPitchNA$WAR)
warPitchNA$Salary <- as.numeric(warPitchNA$Salary)
warPitchNA$teamRpG <- as.numeric(warPitchNA$teamRpG)
warPitchNA$oppRpG <- as.numeric(warPitchNA$oppRpG)
warPitchNA$pyth_exponent <- as.numeric(warPitchNA$pyth_exponent)
warPitchNA$waa_win_perc <- as.numeric(warPitchNA$waa_win_perc)
warPitchNA$WAA <- as.numeric(warPitchNA$WAA)
warPitchNA$WAA_adj <- as.numeric(warPitchNA$WAA_adj)
warPitchNA$oppRpG_rep <- as.numeric(warPitchNA$oppRpG_rep)
warPitchNA$pyth_exponent_rep <- as.numeric(warPitchNA$pyth_exponent_rep)
warPitchNA$waa_win_perc_rep <- as.numeric(warPitchNA$waa_win_perc_rep)
warPitchNA$WAR_rep <- as.numeric(warPitchNA$WAR_rep)
warPitchNA$ERA_plus <- as.numeric(warPitchNA$ERA_plus)
warPitchNA$ER_lg <- as.numeric(warPitchNA$ER_lg)

# Eliminate unneeded columns from Batting table, namely "stint"
colnames(Batting0)
keeps1 <- c("playerID", "yearID", "G", "AB", "R", "H", "X2B", "X3B", "RBI", "SB", "CS", "SO", "IBB", "SH", "SF", "GIDP")
Batting1 <- Batting0[keeps1]

# Eliminate unneeded columns from "Pitching" table, namely "stint"
colnames(Pitching0)
keeps2 <- c("playerID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA")
Pitching1 <- Pitching[keeps2]

# Eliminate unneeded columns from "People" table
colnames(People)
keeps3 <- c("playerID", "nameFirst", "nameLast")
People1 <- People[keeps3]

# Eliminate unneeded columns from "Salaries" table
colnames(Salaries)
keeps4 <- c("playerID", "yearID", "teamID", "salary")
Salaries1 <- Salaries[keeps4]

# Eliminate unneeded columns from "Teams" table
colnames(Teams)
keeps5 <- c("teamID", "yearID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "name", "attendance")
Teams1 <- Teams0[keeps5]

# Eliminate unneeded columns from "war_daily_bat" table
colnames(war_daily_bat)
keeps6 <- c("playerID", "Salary", "WAR", "WAA", "OPS_plus")
warB <- war_daily_bat[keeps6]

# Eliminate unneeded columns from "war_daily_pitch" table
colnames(war_daily_pitch)
keeps8 <- c("playerID", "Salary", "WAR", "WAA", "ERA_plus")
warP <- war_daily_pitch[keeps8]

# Recode "teamID" so singular franchises are consistent
Teams1$teamID[Teams1$teamID == 'ANA'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'CAL'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'FLO'] <- 'MIA'
Teams1$teamID[Teams1$teamID == 'ML4'] <- 'MIL'
Teams1$teamID[Teams1$teamID == 'MON'] <- 'WAS'

Salaries1$teamID[Salaries1$teamID == 'ANA'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'CAL'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'FLO'] <- 'MIA'
Salaries1$teamID[Salaries1$teamID == 'ML4'] <- 'MIL'
Salaries1$teamID[Salaries1$teamID == 'MON'] <- 'WAS'

levels(factor(warBat$team_ID))
levels(factor(warPitch$team_ID))

warBat$team_ID[warBat$team_ID == 'ANA'] <- 'LAA'
warBat$team_ID[warBat$team_ID == 'CAL'] <- 'LAA'
warBat$team_ID[warBat$team_ID == 'FLA'] <- 'MIA'
warBat$team_ID[warBat$team_ID == 'MON'] <- 'WSN'
warBat$team_ID[warBat$team_ID == 'TBD'] <- 'TBR'

warPitch$team_ID[warPitch$team_ID == 'ANA'] <- 'LAA'
warPitch$team_ID[warPitch$team_ID == 'CAL'] <- 'LAA'
warPitch$team_ID[warPitch$team_ID == 'FLA'] <- 'MIA'
warPitch$team_ID[warPitch$team_ID == 'MON'] <- 'WSN'
warPitch$team_ID[warPitch$team_ID == 'TBD'] <- 'TBR'

# Subset "Batting" to only include data from after 1995-1999
batA <- Batting[which(Batting$yearID >= 1995 & Batting$yearID <= 1999), ]

# Subset "Batting" to only include data from 2012-2016
batB <- Batting[which(Batting$yearID >= 2012 & Batting$yearID <= 2016), ]

# Subset "Pitching" to only include data from after 1995-1999
pitchA <- Pitching[which(Pitching$yearID >= 1995 & Pitching$yearID <= 1999), ]

# Subset "Pitching" to only include data from 2012-2016
pitchB <- Pitching[which(Pitching$yearID >= 2012 & Pitching$yearID <= 2016), ]

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(People1, Salaries1, by=c("playerID"), all=TRUE)

# Merge "Batting" data frame and "baseballSalaries" data frame by "playerID"
baseball1 <- merge(Batting1, baseballSalaries, by=c("playerID"), all=TRUE)

# Merge in "warB", which contains advanced batting metrics
batAdv <- merge(baseball1, warB, by=c("playerID"), all=TRUE)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(baseballSalaries, Pitching1, by=c("playerID"), all=TRUE)

# Merge in "warP", which contains advanced pitching metrics
pitchAdv <- merge(baseball2, warP, by=c("playerID"), all=TRUE)

# Simple Player Salaries dataset (only "playerID", "yearID", "salary")
keepsSmall <- c("playerID", "yearID", "salary")
PlayersSimple <- baseballSalaries[keepsSmall]

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams1, Salaries1, by=c("teamID"), all=TRUE)

# Aggregate "teamSalaries" by both team and season
teamSalariesSTY <- aggregate(teamSalaries$salary, by=list(Category=teamSalaries$teamID, teamSalaries$yearID.x), FUN=sum)

## For future use in Python
# Remove NA from any dataset, based on final datasets that have been merged together baseball0, baseball1, baseball2 and teamSalaries
baseball0NA <- na.omit(baseball0)
baseball1NA <- na.omit(baseball1)
baseball2NA <- na.omit(basbeall2)
teamSalariesNA <- na.omit(teamSalaries)