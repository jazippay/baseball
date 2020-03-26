#Linear Regression Analysis WAR and AB, WAR and IPouts
#Create a Dataframe of WAR and AB by combining warBatNA and batting
#Create a Dataframe of WAR and IPouts from warPitchNA

#Dataframe merging WAR and AB variables      
keepA <- c('yearID', 'AB', 'playerID')
keepB <- c('WAR', 'team_ID', 'playerID')
BattingA <- Batting[keepA]
warB <- warBatNA[keepB]

warBatting <- merge(BattingA,warB,by=c("playerID"))
View(warBatting)

#Create 3 Different dataframes based on range of years; 95-2019,95-99 and 2015-2019
warBattingAll <- warBatting[which(warBatting$yearID > 1994 & warBatting$yearID < 2020), ]
warBatting90 <- warBatting[which(warBatting$yearID > 1994 & warBatting$yearID < 2000), ]
warBatting2000 <- warBatting[which(warBatting$yearID > 2014 & warBatting$yearID < 2020), ]

#Remove NA from all datasets
warBattingAll <- na.omit(warBattingAll)
#Conver WAR to numeric
warBatingAll$WAR <- as.numeric(warBatingAll$WAR)

#linear model formula
lin_reg <- lm(AB ~ WAR, warBattingAll)
summary(lin_reg)
#Adjusted R-sqaured .23
#significant p value
lin_reg <- lm(AB ~ WAR, warBatting90)
summary(lin_reg)
#Adjusted R-sqaured .21
#significant p value
lin_reg <- lm(AB ~ WAR, warBatting2000)
summary(lin_reg)
#Adjusted R-sqaured .30
#significant p value

#Create a Dataframe of WAR and IPouts from warPitchNA
#Create 3 Different dataframes based on range of years; 95-2019,95-99 and 2015-2019
warPitchAll <- warPitchNA[which(warPitchNA$year_ID > 1994 & warPitchNA$year_ID < 2020), ]
warPitch90 <- warPitchNA[which(warPitchNA$year_ID > 1994 & warPitchNA$year_ID < 2000), ]
warPitch2000 <- warPitchNA[which(warPitchNA$year_ID > 2014 & warPitchNA$year_ID < 2020), ]

#Linear Regression on WAR IPouts
lin_reg <- lm(IPouts ~ WAR, warPitchAll)
summary(lin_reg)
#Adjusted R-sqaured .42
#significant p value
lin_reg <- lm(IPouts ~ WAR, warPitch90)
summary(lin_reg)
#Adjusted R-sqaured .46
#significant p value
lin_reg <- lm(IPouts ~ WAR, warPitch2000)
summary(lin_reg)
#Adjusted R-sqaured .39
#significant p value
