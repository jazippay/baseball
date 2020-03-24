# Data Science Final Project
# Week 4: Analysis

# Linear regression between salary and WAR
## Aggregate 'salary' and 'WAR' columns in warBatA & warBatB
warBatASalary <- aggregate(salary ~ team_ID, warBatA, mean)
warBatAWAR <- aggregate(WAR ~ team_ID, warBatA, mean)
warBatBSalary <- aggregate(salary ~ team_ID, warBatB, mean)
warBatBWAR <- aggregate(WAR ~ team_ID, warBatB, mean)
## Merge 'warBatASalary' & 'warBatAWAR'
warBatATeams <- warBatASalary %>% right_join(warBatAWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warBatA
cor.test(warBatATeams$salary, warBatATeams$WAR, method="pearson", use = "complete.obs")
## Linear regression model
lin_regBatA <- lm(salary ~ WAR, warBatATeams)
print(lin_regBatA)
### y = 1029189x + 659218
summary(lin_regBatA)
### Adj. R-squared: 
### p-value: 

## Merge 'warBatBSalary' & 'warBatBWAR'
warBatBTeams <- warBatBSalary %>% right_join(warBatBWAR, by=c("team_ID"))
## Cor test of 'warBatBSalary' & 'warBatBWAR'
cor.test(warBatBTeams$salary, warBatBTeams$WAR, method="pearson", use = "complete.obs")
## Linear regression model
lin_regBatB <- lm(salary ~ WAR, warBatBTeams)
print(lin_regBatB)
### y = 2447283x + 3366237
summary(lin_regBatB)
### Adj. R-squared: 
### p-value: 

## Aggregate 'salary' and 'WAR' columns in warPitchA & warPitchB
warPitchASalary <- aggregate(salary ~ team_ID, warPitchA, mean)
warPitchAWAR <- aggregate(WAR ~ team_ID, warPitchA, mean)
warPitchBSalary <- aggregate(salary ~ team_ID, warPitchB, mean)
warPitchBWAR <- aggregate(WAR ~ team_ID, warPitchB, mean)
## Merge 'warPitchASalary' & 'warPitchAWAR'
warPitchATeams <- warPitchASalary %>% right_join(warPitchAWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warPitchA
cor.test(warPitchATeams$salary, warPitchATeams$WAR, method="pearson", use = "complete.obs")
## Linear regression model
lin_regPitchA <- lm(salary ~ WAR, warPitchATeams)
print(lin_regPitchA)
### y = 1129046x + 337518
summary(lin_regPitchA)
### Adj. R-squared: 
### p-value: 

## Merge 'warPitchBSalary' & 'warPitchBWAR'
warPitchBTeams <- warPitchBSalary %>% right_join(warPitchBWAR, by=c("team_ID"))
## Cor test of 'salary' & 'WAR' for warPitchB
cor.test(warPitchBTeams$salary, warPitchBTeams$WAR, method="pearson", use = "complete.obs")
## Linear regression model
lin_regPitchB <- lm(salary ~ WAR, warPitchBTeams)
print(lin_regPitchB)
### y = 3506832x = 2186677
summary(lin_regPitchB)
### Adj. R-squared: 
### p-value: 

# Linear regression between salary and age
## Aggregate 'age' columns in warBatA & warBatB
warBatAAge<- aggregate(age ~ team_ID, warBatA, mean)
warBatBAge<- aggregate(age ~ team_ID, warBatB, mean)
## Merge 'warBatASalary' & 'warBatAAge'
warBatASalAge <- warBatASalary %>% right_join(warBatAAge, by=c("team_ID"))
## Cor test of 'salary' & 'age' for warBatA
cor.test(warBatASalAge$salary, warBatASalAge$age, method="pearson", use = "complete.obs")
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
## Linear regression model
lin_regPitchBSalAge <- lm(salary ~ age, warPitchBSalAge)
print(lin_regPitchBSalAge)
### y = 1495010x - 38021459
summary(lin_regPitchBSalAge)
### Adj. R-squared: 0.3694
### p-value: 0.0002192




