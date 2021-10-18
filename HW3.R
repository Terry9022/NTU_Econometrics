# Exercise 3
# install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("dLagM")
install.packages("vars")

# import 2 datasets (divide the primary dataset into 2 sub-datasets)
library(readxl)
data_g <- read_xls("germany.xls")
names(data_g)[1] <- "Year"
names(data_g)[2] <- "Month"
names(data_g)[3] <- "Circulation"
names(data_g)[4] <- "Deposits"
names(data_g)[5] <- "CPI"
data_g <- data_g[-c(1, 2), ]  # drop useless rows

data_h <- read_xls("Hungary.xls")
names(data_h)[1] <- "Year"
names(data_h)[2] <- "Month"
names(data_h)[3] <- "Circulation"
names(data_h)[4] <- "Deposits"
names(data_h)[5] <- "CPI"
data_h <- data_h[-c(1, 2), ]  # drop useless rows

# (1)
library(dplyr)
library(dLagM)
data_g$moneySupply <- as.numeric(data_g$Circulation) + as.numeric(data_g$Deposits)
data_h$moneySupply <- as.numeric(data_h$Circulation) + as.numeric(data_h$Deposits)
growth_rate <- function(x)(x/lag(x)-1) * 100
data_g$inflationRate <- growth_rate(as.numeric(data_g$CPI))
data_g$MSGrowthRate <- growth_rate(as.numeric(data_g$moneySupply))
data_h$inflationRate <- growth_rate(as.numeric(data_h$CPI))
data_h$MSGrowthRate <- growth_rate(as.numeric(data_h$moneySupply))

data_g_noNA <- data_g[complete.cases(data_g[7:8]), ]  # drop NAs in column inflationRate and MSGrowthRate
data_h_noNA <- data_h[complete.cases(data_h[7:8]), ]  # drop NAs in column inflationRate and MSGrowthRate

data_g_hyper <- data_g_noNA[data_g_noNA$inflationRate >= 50, ]  # only preserve the data in hyperinflation
data_h_hyper <- data_h_noNA[data_h_noNA$inflationRate >= 50, ]  # only preserve the data in hyperinflation

koyck_g <- koyckDlm(y = data_g_hyper$CPI, x= data_g_hyper$moneySupply)
print(koyck_g)

koyck_h <- koyckDlm(y = data_h_hyper$CPI, x = data_h_hyper$moneySupply)
print(koyck_h)

# (2)
library(vars)
library(lmtest)

VARselect(data_g_noNA[, 7:8], lag.max = 20,type = "const")  # choosing the optimal order, which is 13

grangertest(inflationRate ~ MSGrowthRate, order = 13, data = data_g_noNA)
grangertest(MSGrowthRate ~ inflationRate, order = 13, data = data_g_noNA)
# interpretation
"optimal lags: 13
 p-values in two-way Granger test are approximately 0
 We can reject the null hypothesis that there is no Granger causality between inflation rate and money supply
 growth rate in Germany."


VARselect(data_h_noNA[, 7:8], lag.max = 20,type = "const")  # choosing the optimal order, which is 12

grangertest(inflationRate ~ MSGrowthRate, order = 12, data = data_h_noNA)
grangertest(MSGrowthRate ~ inflationRate, order = 12, data = data_h_noNA)
# interpretation
"optimal lags: 12
 p-values in two-way Granger test are 0.846 and 0.1946 respectively
 We cannot reject the null hypothesis that there is no Granger causality between inflation rate and money supply
 growth rate in Hungary."
