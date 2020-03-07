#Setting Up for a ANOVA
#How has average salary changed over time?

library(rcompanion)
library(dbplyr)
library(IDPmisc)
library(car)

#Testing for Normality
plotNormalHistogram(salaries1$salary)

salaries1$salarySQRT <- sqrt(salaries1$salary)
plotNormalHistogram(salaries1$salarySQRT)
salaries1$salaryLOG <- log(salaries1$salary)
plotNormalHistogram(salaries1$salaryLOG)
salaries2 <- NaRV.omit(salaries1)
plotNormalHistogram(salaries2$salaryLOG)
#thats a nice bell curve!




