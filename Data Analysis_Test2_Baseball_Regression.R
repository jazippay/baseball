#Regression Test, Do Runs Influence Wins?

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library(gvlma)

teams8516 <- Teams[ which(Teams$yearID > 1984 & Teams$yearID < 2017), ]
View(teams8516)
teams8516 <- NaRV.omit(teams8516)
scatter.smooth(x=teams8516$R, y=teams8516$W, main="Total Wins by Runs Scored")

#Creating a linear Model and testing for assumptions
lmMod <- lm(W~R, data=teams8516)
par(mfrow=c(2,2))
plot(lmMod)
lmtest::bptest(lmMod)
#not significant therefore we have met the assumption
car::ncvTest(lmMod)
#not significant therefore we have met the assumption
gvlma(lmMod)

summary(lmMod)
#OK so we confirmed what everybody knows, runs significantly influence wins

#Do Wins influence attendance?
scatter.smooth(x=teams8516$W, y=teams8516$attendance, main="Wins by Attendance")
lmMod_W <- lm(attendance~W, data=teams8516)
summary(lmMod_W)
#Wins and attendance have a linear relationship. But what does that mean?

#Wrangling Subsetting teams8516 columns
colnames(teams8516)
keepsT <- c("yearID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance")
teams_numeric <- teams8516[keepsT]

#correlation matrix
res <- cor(teams_numeric)
round(res, 2)
