# Data Science Final Project
# Week 4: Analysis

# Linear regression between salary and WAR
## Aggregate 'salary' and 'WAR' columns in warBatA
warBatASalary <- aggregate(salary ~ team_ID, warBatA, mean)
warBatAWAR <- aggregate(WAR ~ team_ID, warBatA, mean)
## Merge 'warBatASalary' & 'warBatAWAR'
warBatATeams <- warBatASalary %>% right_join(warBatAWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warBatA
cor.test(warBatATeams$salary, warBatATeams$WAR, method="pearson", use = "complete.obs")
### p-value: 0.0008613
### cor: 0.5678903
## Linear regression model
lin_regBatA <- lm(salary ~ WAR, warBatATeams)
print(lin_regBatA)
### y = 1029189x + 659218
summary(lin_regBatA)
### Adj. R-squared: 0.2991
### p-value: 0.0008613

## Aggregate 'salary' and 'WAR' columns in warBatB
warBatBSalary <- aggregate(salary ~ team_ID, warBatB, mean)
warBatBWAR <- aggregate(WAR ~ team_ID, warBatB, mean)
## Merge 'warBatBSalary' & 'warBatBWAR'
warBatBTeams <- warBatBSalary %>% right_join(warBatBWAR, by=c("team_ID"))
## Cor test of 'warBatBSalary' & 'warBatBWAR'
cor.test(warBatBTeams$salary, warBatBTeams$WAR, method="pearson", use = "complete.obs")
### p-value: 0.1493
### cor: 0.2698578
### Results insignificant
## Linear regression model
lin_regBatB <- lm(salary ~ WAR, warBatBTeams)
print(lin_regBatB)
### y = 2447283x + 3366237
summary(lin_regBatB)
### Adj. R-squared: 0.03971
### p-value: 0.1493
### Results insignificant

## Aggregate 'salary' and 'WAR' columns in warPitchA
warPitchASalary <- aggregate(salary ~ team_ID, warPitchA, mean)
warPitchAWAR <- aggregate(WAR ~ team_ID, warPitchA, mean)
## Merge 'warPitchASalary' & 'warPitchAWAR'
warPitchATeams <- warPitchASalary %>% right_join(warPitchAWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warPitchA
cor.test(warPitchATeams$salary, warPitchATeams$WAR, method="pearson", use = "complete.obs")
### p-value: 8.022e-05
### cor: 0.6569902
## Linear regression model
lin_regPitchA <- lm(salary ~ WAR, warPitchATeams)
print(lin_regPitchA)
### y = 1129046x + 337518
summary(lin_regPitchA)
### Adj. R-squared: 0.4113
### p-value: 8.022e-05

## Aggregate 'salary' and 'WAR' columns in warPitchB
warPitchBSalary <- aggregate(salary ~ team_ID, warPitchB, mean)
warPitchBWAR <- aggregate(WAR ~ team_ID, warPitchB, mean)
## Merge 'warPitchBSalary' & 'warPitchBWAR'
warPitchBTeams <- warPitchBSalary %>% right_join(warPitchBWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warPitchB
cor.test(warPitchBTeams$salary, warPitchBTeams$WAR, method="pearson", use = "complete.obs")
### p-value: 0.007931
### cor: 0.4753988
## Linear regression model
lin_regPitchB <- lm(salary ~ WAR, warPitchBTeams)
print(lin_regPitchB)
### y = 3506832x = 2186677
summary(lin_regPitchB)
### Adj. R-squared: 0.1984
### p-value: 0.007931

# Linear regression between salary and age
## Aggregate 'age' columns in warBatA & warBatB
warBatAAge<- aggregate(age ~ team_ID, warBatA, mean)
warBatBAge<- aggregate(age ~ team_ID, warBatB, mean)
## Merge 'warBatASalary' & 'warBatAAge'
warBatASalAge <- warBatASalary %>% right_join(warBatAAge, by=c("team_ID"))
## Cor test of 'salary' & 'age' for warBatA
cor.test(warBatASalAge$salary, warBatASalAge$age, method="pearson", use = "complete.obs")
### p-value: 1.8e-05
### cor: 0.69802
## Linear regression model
lin_regBatASalAge <- lm(salary ~ age, warBatASalAge)
print(lin_regBatASalAge)
### y = 354693x - 8794716
summary(lin_regBatASalAge)
### Adj. R-squared: 0.4689
### p-value: 1.8e-05

## Merge 'warBatBSalary' & 'warBatBAge'
warBatBSalAge <- warBatBSalary %>% right_join(warBatBAge, by=c("team_ID"))
## Cor test of 'salary' & 'age' for warBatA
cor.test(warBatBSalAge$salary, warBatBSalAge$age, method="pearson", use = "complete.obs")
### p-value: 0.0004111
### cor: 0.6038025
## Linear regression model
lin_regBatBSalAge <- lm(salary ~ age, warBatBSalAge)
print(lin_regBatBSalAge)
### y = 1833777x - 46892185
summary(lin_regBatBSalAge)
### Adj. R-squared: 0.3419
### p-value: 0.0004111

## Aggregate 'age' columns in warPitchA & warPitchB
warPitchAAge<- aggregate(age ~ team_ID, warPitchA, mean)
warPitchBAge<- aggregate(age ~ team_ID, warPitchB, mean)
## Merge 'warPitchASalary' & 'warPitchAAge'
warPitchASalAge <- warPitchASalary %>% right_join(warPitchAAge, by=c("team_ID"))
## Cor test of 'salary' & 'age' for warPitchA
cor.test(warPitchASalAge$salary, warPitchASalAge$age, method="pearson", use = "complete.obs")
### p-value: 0.01736
### cor: 0.4311888
## Linear regression model
lin_regPitchASalAge <- lm(salary ~ age, warPitchASalAge)
print(lin_regPitchASalAge)
### y = 221479x - 5156764
summary(lin_regPitchASalAge)
### Adj. R-squared: 0.1568
### p-value: 0.01736

## Merge 'warPitchBSalary' & 'warPitchBAge'
warPitchBSalAge <- warPitchBSalary %>% right_join(warPitchBAge, by=c("team_ID"))
## Cor test of 'salary' & 'age' for warPitchB
cor.test(warPitchBSalAge$salary, warPitchBSalAge$age, method="pearson", use = "complete.obs")
### p-value: 0.0002192
### cor: 0.6254396
## Linear regression model
lin_regPitchBSalAge <- lm(salary ~ age, warPitchBSalAge)
print(lin_regPitchBSalAge)
### y = 1495010x - 38021459
summary(lin_regPitchBSalAge)
### Adj. R-squared: 0.3694
### p-value: 0.0002192

# Linear regression between salary & years
## Cor test of 'salary' & 'year_ID' for warBatA
cor.test(warBatA$salary, warBatA$year_ID, method="pearson", use = "complete.obs")
### p-value: 1.066e-12
### cor: 0.1031825
## Linear regression model
lin_regBatASalYear <- lm(salary ~ year_ID, warBatA)
print(lin_regBatASalYear)
### y = 126265x - 250958516
summary(lin_regBatASalYear)
### Adj. R-squared: 0.01044
### p-value: 1.066e-12

## Cor test of 'salary' & 'year_ID' for warBatB
cor.test(warBatB$salary, warBatB$year_ID, method="pearson", use = "complete.obs")
### p-value: 0.5194
### cor: 0.01009375
#### Result insignificant
## Linear regression model
lin_regBatBSalYear <- lm(salary ~ year_ID, warBatB)
print(lin_regBatBSalYear)
### y = 42880x - 82020029
summary(lin_regBatBSalYear)
### Adj. R-squared: -0.0001436
### p-value: 0.5194
#### Result insignificant

## Cor test of 'salary' & 'year_ID' for warPitchA
cor.test(warPitchA$salary, warPitchA$year_ID, method="pearson", use = "complete.obs")
### p-value: 1.6e-09
### cor: 0.1276105
## Linear regression model
lin_regPitchASalYear <- lm(salary ~ year_ID, warPitchA)
print(lin_regPitchASalYear)
### y = 138687x - 275926911
summary(lin_regPitchASalYear)
### Adj. R-squared: 0.01584
### p-value: 1.6e-09

## Cor test of 'salary' & 'year_ID' for warPitchB
cor.test(warPitchB$salary, warPitchB$year_ID, method="pearson", use = "complete.obs")
### p-value: 0.8147
### cor: 0.004955968
#### Result insignificant
## Linear regression model
lin_regPitchBSalYear <- lm(salary ~ year_ID, warPitchB)
print(lin_regPitchBSalYear)
### y = 19626x - 35653995
summary(lin_regPitchBSalYear)
### Adj. R-squared: -0.0004223
### p-value: 0.8147
#### Result insignificant

## Cor test of 'salary' & 'year_ID' for warBatNA
cor.test(warBatNA$salary, warBatNA$year_ID, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.3578622
## Linear regression model
lin_regBatNASalYear <- lm(salary ~ year_ID, warBatNA)
print(lin_regBatNASalYear)
### y = 131437x - 260887743
summary(lin_regBatNASalYear)
### Adj. R-squared: 0.128
### p-value: 2.2e-16

## Cor test of 'salary' & 'year_ID' for warPitchNA
cor.test(warPitchNA$salary, warPitchNA$year_ID, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.3389813
## Linear regression model
lin_regPitchNASalYear <- lm(salary ~ year_ID, warPitchNA)
print(lin_regPitchNASalYear)
### y = 116415x - 231063311
summary(lin_regPitchNASalYear)
### Adj. R-squared: 0.1148
### p-value: 2.2e-16

# Linear regression between WAR & plate appearances
## Cor test of 'WAR' & 'PA' for warBatA
cor.test(warBatA$WAR, warBatA$PA, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.7170853
## Linear regression model
lin_regBatAWARPA <- lm(WAR ~ PA, warBatA)
print(lin_regBatAWARPA)
### y = 0.004769x - 0.268813
summary(lin_regBatAWARPA)
### Adj. R-squared: 0.5141
### p-value: 2.2e-16

## Cor test of 'WAR' & 'PA' for warBatB
cor.test(warBatB$WAR, warBatB$PA, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.7261039
## Linear regression model
lin_regBatBWARPA <- lm(WAR ~ PA, warBatB)
print(lin_regBatBWARPA)
### y = 0.004619x - 0.195328
summary(lin_regBatBWARPA)
### Adj. R-squared: 0.5272
### p-value: 2.2e-16

# Linear regression between WAR & innings pitched
## Cor test of 'WAR' & 'IPouts' for warPitchA
cor.test(warPitchA$WAR, warPitchA$IPouts, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.684004
## Linear regression model
lin_regPitchAWARIP <- lm(WAR ~ IPouts, warPitchA)
print(lin_regPitchAWARIP)
### y = 0.005357x - 0.439640
summary(lin_regPitchAWARIP)
### Adj. R-squared: 0.4677
### p-value: 2.2e-16

## Cor test of 'WAR' & 'IPouts' for warPitchB
cor.test(warPitchB$WAR, warPitchB$IPouts, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.6777066
## Linear regression model
lin_regPitchBWARIP <- lm(WAR ~ IPouts, warPitchB)
print(lin_regPitchBWARIP)
### y = 0.005311x - 0.327967
summary(lin_regPitchBWARIP)
### Adj. R-squared: 0.4592
### p-value: 2.2e-16

# Linear regression between OPS+ & plate appearances
## Cor test of 'OPS_plus' & 'PA' for warBatA
cor.test(warBatA$OPS_plus, warBatA$PA, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4275362
## Linear regression model
lin_regBatAOPSPA <- lm(OPS_plus ~ PA, warBatA)
print(lin_regBatAOPSPA)
### y = 0.181x + 17.476
summary(lin_regBatAOPSPA)
### Adj. R-squared: 0.1826
### p-value: 2.2e-16

## Cor test of 'OPS_plus' & 'PA' for warBatB
cor.test(warBatB$OPS_plus, warBatB$PA, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4871326
## Linear regression model
lin_regBatBOPSPA <- lm(OPS_plus ~ PA, warBatB)
print(lin_regBatBOPSPA)
### y = 0.2149x + 4.0279
summary(lin_regBatBOPSPA)
### Adj. R-squared: 0.2372
### p-value: 2.2e-16

# Linear regression between ERA+ & innings pitched
## Cor test of 'ERA_plus' & 'IPouts' for warPitchA
cor.test(warPitchA$ERA_plus, warPitchA$IPouts, method="pearson", use = "complete.obs")
### p-value: 8.943e-14
### cor: 0.1352583
## Linear regression model
lin_regPitchAERAIP <- lm(ERA_plus ~ IPouts, warPitchA)
print(lin_regPitchAERAIP)
### y = 0.03837x + 95.54401
summary(lin_regPitchAERAIP)
### Adj. R-squared: 0.01797
### p-value: 8.943e-14

## Cor test of 'ERA_plus' & 'IPouts' for warPitchB
cor.test(warPitchB$ERA_plus, warPitchB$IPouts, method="pearson", use = "complete.obs")
### p-value: 4.357e-09
### cor: 0.09141024
## Linear regression model
lin_regPitchBERAIP <- lm(ERA_plus ~ IPouts, warPitchB)
print(lin_regPitchBERAIP)
### y = 0.0409x + 100.9065
summary(lin_regPitchBERAIP)
### Adj. R-squared: 0.008114
### p-value: 4.357e-09

# Linear regression between 'salary' and 'WAR' containing only players with 4+ WAR
## Subset 'warBatA' to only include players with 4+ WAR
warBatA4 <- warBatA[which(warBatA$WAR >= 4), ]
## Aggregate 'salary' and 'WAR' columns in warBatA4
warBatA4Salary <- aggregate(salary ~ team_ID, warBatA4, mean)
warBatA4WAR <- aggregate(WAR ~ team_ID, warBatA4, mean)
## Merge 'warBatASalary' & 'warBatAWAR'
warBatA4.0 <- warBatA4Salary %>% right_join(warBatA4WAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warBatA
cor.test(warBatA4.0$salary, warBatA4.0$WAR, method="pearson", use = "complete.obs")
### p-value: 0.2034
### cor: 0.2389928
#### Result insignificant

## Subset 'warBatB' to only include players with 4+ WAR
warBatB4 <- warBatB[which(warBatB$WAR >= 4), ]
## Aggregate 'salary' and 'WAR' columns in 'warBatB4'
warBatB4Salary <- aggregate(salary ~ team_ID, warBatB4, mean)
warBatB4WAR <- aggregate(WAR ~ team_ID, warBatB4, mean)
## Merge 'warBatBSalary' & 'warBatBWAR'
warBatB4.0 <- warBatB4Salary %>% right_join(warBatB4WAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for 'warBatB'
cor.test(warBatB4.0$salary, warBatB4.0$WAR, method="pearson", use = "complete.obs")
### p-value: 0.03837
### cor: 0.3799233
## Linear regression model
lin_regBatB4.0 <- lm(salary ~ WAR, warBatB4.0)
print(lin_regBatB4.0)
### y = 2309008x - 5018732
summary(lin_regBatB4.0)
### Adj. R-squared: 0.1138
### p-value: 0.03837

## Subset 'warPitchA' to only include players with 4+ WAR
warPitchA4 <- warPitchA[which(warPitchA$WAR >= 4), ]
## Aggregate 'salary' and 'WAR' columns in warPitchA4
warPitchA4Salary <- aggregate(salary ~ team_ID, warPitchA4, mean)
warPitchA4WAR <- aggregate(WAR ~ team_ID, warPitchA4, mean)
## Merge 'warPitchASalary' & 'warPitchAWAR'
warPitchA4.0 <- warPitchA4Salary %>% right_join(warPitchA4WAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for 'warPitchA'
cor.test(warPitchA4.0$salary, warPitchA4.0$WAR, method="pearson", use = "complete.obs")
### p-value: 0.0007366
### cor: 0.5909647
## Linear regression model
lin_regPitchA4.0 <- lm(salary ~ WAR, warPitchA4.0)
print(lin_regPitchA4.0)
### y = 985644x - 2437831
summary(lin_regPitchA4.0)
### Adj. R-squared: 0.3251
### p-value: 0.0007366

## Subset 'warPitchB' to only include players with 4+ WAR
warPitchB4 <- warPitchB[which(warPitchB$WAR >= 4), ]
## Aggregate 'salary' and 'WAR' columns in warPitchB4
warPitchB4Salary <- aggregate(salary ~ team_ID, warPitchB4, mean)
warPitchB4WAR <- aggregate(WAR ~ team_ID, warPitchB4, mean)
## Merge 'warPitchBSalary' & 'warPitchBWAR'
warPitchB4.0 <- warPitchB4Salary %>% right_join(warPitchB4WAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for 'warPitchB'
cor.test(warPitchB4.0$salary, warPitchB4.0$WAR, method="pearson", use = "complete.obs")
### p-value: 0.3005
### cor: 0.2068641
#### Result insignificant
## Linear regression model
lin_regPitchB4.0 <- lm(salary ~ WAR, warBatB4.0)
print(lin_regPitchB4.0)
### y = 2309008x - 5018732
summary(lin_regPitchB4.0)
### Adj. R-squared: 0.1138
### p-value: 0.03837

# Linear regression between salary & OPS+
## Cor test of 'salary' & 'OPS_plus' for warBatA
cor.test(warBatA$salary, warBatA$OPS_plus, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.1604671
## Linear regression model
lin_regBatASalOPS <- lm(salary ~ OPS_plus, warBatA)
print(lin_regBatASalOPS)
### y = 3424x + 1106971
summary(lin_regBatASalOPS)
### Adj. R-squared: 0.02549
### p-value: 2.2e-16

## Cor test of 'salary' & 'OPS_plus' for warBatB
cor.test(warBatB$salary, warBatB$OPS_plus, method="pearson", use = "complete.obs")
### p-value: 4.131e-06
### cor: 0.08266121
## Linear regression model
lin_regBatBSalOPS <- lm(salary ~ OPS_plus, warBatB)
print(lin_regBatBSalOPS)
### y = 6286x + 4720946
summary(lin_regBatBSalOPS)
### Adj. R-squared: 0.006512
### p-value: 4.131e-06

# Linear regression between salary & ERA+
## Cor test of 'salary' & 'ERA_plus' for warPitchA
cor.test(warPitchA$salary, warPitchA$ERA_plus, method="pearson", use = "complete.obs")
### p-value: 6.642e-08
### cor: 0.1147473
## Linear regression model
lin_regPitchASalERA <- lm(salary ~ ERA_plus, warPitchA)
print(lin_regPitchASalERA)
### y = 3644x + 647099
summary(lin_regPitchASalERA)
### Adj. R-squared: 0.01272
### p-value: 0.01272

## Cor test of 'salary' & 'ERA_plus' for warPitchB
cor.test(warPitchB$salary, warPitchB$ERA_plus, method="pearson", use = "complete.obs")
### p-value: 0.0002052
### cor: 0.07992986
## Linear regression model
lin_regPitchBSalERA <- lm(salary ~ ERA_plus, warPitchB)
print(lin_regPitchBSalERA)
### y = 7430x + 3156378
summary(lin_regPitchBSalERA)
### Adj. R-squared: 0.005927
### p-value: 0.0002052

# Linear regression between age, plate appearances, & salary
## Cor test of 'age' & 'salary' for warBatA
cor.test(warBatA$age, warBatA$salary, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.364693
## Cor test of 'PA' & 'salary' for warBatA
cor.test(warBatA$age, warBatA$PA, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.1104071
## Linear regression model
lin_regBatASalAgePA <- lm(salary ~ age + PA, warBatA)
print(lin_regBatASalAgePA)
summary(lin_regBatASalAgePA)
### Adj. R-squared: 0.2559
### p-value: 2.2e-16

## Cor test of 'age' & 'salary' for warBatB
cor.test(warBatB$age, warBatB$salary, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4370244
## Cor test of 'PA' & 'salary' for warBatB
cor.test(warBatB$age, warBatB$PA, method="pearson", use = "complete.obs")
### p-value: 0.003453
### cor: 0.03473897
## Linear regression model
lin_regBatBSalAgePA <- lm(salary ~ age + PA, warBatB)
print(lin_regBatBSalAgePA)
summary(lin_regBatBSalAgePA)
### Adj. R-squared: 0.2547
### p-value: 2.2e-16

# Linear regression between age, innings pitched, & salary
## Cor test of 'age' & 'salary' for warPitchA
cor.test(warPitchA$age, warPitchA$salary, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.3891816
## Cor test of 'IPouts' & 'salary' for warPitchA
cor.test(warPitchA$age, warPitchA$IPouts, method="pearson", use = "complete.obs")
### p-value: 1.462e-07
### cor: 0.09475569
## Linear regression model
lin_regPitchASalAgeIP <- lm(salary ~ age + IPouts, warPitchA)
print(lin_regPitchASalAgeIP)
summary(lin_regPitchASalAgeIP)
### Adj. R-squared: 0.3421
### p-value: 2.2e-16

## Cor test of 'age' & 'salary' for warPitchB
cor.test(warPitchB$age, warPitchB$salary, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.3881008
## Cor test of 'IPouts' & 'salary' for warPitchB
cor.test(warPitchB$age, warPitchB$IPouts, method="pearson", use = "complete.obs")
### p-value: 5.646e-06
### cor: 0.06921631
## Linear regression model
lin_regPitchBSalAgeIP <- lm(salary ~ age + IPouts, warPitchB)
print(lin_regPitchBSalAgeIP)
summary(lin_regPitchBSalAgeIP)
### Adj. R-squared: 0.2905
### p-value: 2.2e-16

# Multiple linear regression between salary, HR, & RBI
## Subset 'Batting1' to match the years of 'warBatA'
BattingA <- Batting1[which(Batting1$yearID > 1994 & Batting1$yearID < 2000), ]
## Rename player ID and year ID columns in 'BattingA' to be consistent
names(BattingA)[names(BattingA) == "playerID"] <- "player_ID"
names(BattingA)[names(BattingA) == "yearID"] <- "year_ID"
## Merge 'warBatA' & 'BattingA'
primeBatA <- merge(BattingA, warBatA, by=c("player_ID", "year_ID"))
## Cor test of 'salary' & 'HR' for primeBatA
cor.test(primeBatA$salary, primeBatA$HR, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4377785
## Cor test of 'salary' & 'RBI' for primeBatA
cor.test(primeBatA$salary, primeBatA$RBI, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4170428
## Linear regression model
lin_regBatASalHRRBI <- lm(salary ~ HR + RBI, primeBatA)
print(lin_regBatASalHRRBI)
summary(lin_regBatASalHRRBI)
### Adj. R-squared: 0.1918
### p-value: 2.2e-16

## Subset 'Batting1' to match the years of 'warBatB'
BattingB <- Batting1[which(Batting1$yearID > 2014), ]
## Rename player ID and year ID columns in 'BattingA' to be consistent
names(BattingB)[names(BattingB) == "playerID"] <- "player_ID"
names(BattingB)[names(BattingB) == "yearID"] <- "year_ID"
## Merge 'warBatB' & 'BattingB'
primeBatB <- merge(BattingB, warBatB, by=c("player_ID", "year_ID"))
## Cor test of 'salary' & 'HR' for primeBatB
cor.test(primeBatB$salary, primeBatB$HR, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.2243393
## Cor test of 'salary' & 'RBI' for primeBatB
cor.test(primeBatB$salary, primeBatB$RBI, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.2400759
## Linear regression model
lin_regBatBSalHRRBI <- lm(salary ~ HR + RBI, primeBatB)
print(lin_regBatBSalHRRBI)
summary(lin_regBatBSalHRRBI)
### Adj. R-squared: 0.05729
### p-value: 2.2e-16

# Multiple linear regression between salary, SO, ERA, & W
## Subset 'Pitching1' to match the years of 'warPitchA'
PitchingA <- Pitching1[which(Pitching1$yearID > 1994 & Pitching1$yearID < 2000), ]
## Rename player ID and year ID columns in 'PitchingA' to be consistent
names(PitchingA)[names(PitchingA) == "playerID"] <- "player_ID"
names(PitchingA)[names(PitchingA) == "yearID"] <- "year_ID"
## Merge 'warPitchA' & 'PitchingA'
primePitchA <- merge(PitchingA, warPitchA, by=c("player_ID", "year_ID"))
## Cor test of 'salary' & 'SO' for primePitchA
cor.test(primePitchA$salary, primePitchA$SO, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.464393
## Cor test of 'salary' & 'ERA' for primePitchA
cor.test(primePitchA$salary, primePitchA$ERA, method="pearson", use = "complete.obs")
### p-value: 2.323e-06
### cor: -0.09431749
## Cor test of 'salary' & 'W' for primePitchA
cor.test(primePitchA$salary, primePitchA$W, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.4360883
## Linear regression model
lin_regPitchASalSOERAW <- lm(salary ~ SO + ERA + W, primePitchA)
print(lin_regPitchASalSOERAW)
summary(lin_regPitchASalSOERAW)
### Adj. R-squared: 0.221
### p-value: 2.2e-16

## Subset 'Pitching1' to match the years of 'warPitchB'
PitchingB <- Pitching1[which(Pitching1$yearID > 2014), ]
## Rename player ID and year ID columns in 'PitchingB' to be consistent
names(PitchingB)[names(PitchingB) == "playerID"] <- "player_ID"
names(PitchingB)[names(PitchingB) == "yearID"] <- "year_ID"
## Merge 'warPitchB' & 'PitchingB'
primePitchB <- merge(PitchingB, warPitchB, by=c("player_ID", "year_ID"))
## Cor test of 'salary' & 'SO' for primePitchB
cor.test(primePitchB$salary, primePitchB$SO, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.3381697
## Cor test of 'salary' & 'ERA' for primePitchB
cor.test(primePitchB$salary, primePitchB$ERA, method="pearson", use = "complete.obs")
### p-value: 6.656e-05
### cor: -0.0793777
## Cor test of 'salary' & 'W' for primePitchB
cor.test(primePitchB$salary, primePitchB$W, method="pearson", use = "complete.obs")
### p-value: 2.2e-16
### cor: 0.340884
## Linear regression model
lin_regPitchBSalSOERAW <- lm(salary ~ SO + ERA + W, primePitchB)
print(lin_regPitchBSalSOERAW)
summary(lin_regPitchBSalSOERAW)
### Adj. R-squared: 0.1211
### p-value: 2.2e-16