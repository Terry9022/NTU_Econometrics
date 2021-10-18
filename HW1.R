# 8.14
# install packages
install.packages("rmarkdown", dependencies=TRUE)
install.packages("foreign")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("nlme")
install.packages("dynlm")

# (a)
library(foreign)
data1 <- read.dta("cloth.dta")
data1$q1_2 <- (data1$q1) ^ 2
data1$q1_3 <- (data1$q1) ^ 3
data1$q2_2 <- (data1$q2) ^ 2
data1$q2_3 <- (data1$q2) ^ 3
regression1 <- lm(c1 ~ q1 + q1_2 + q1_3, data = data1)
print(regression1)
regression2 <- lm(c2 ~ q2 + q2_2 + q2_3, data = data1)
print(regression2)
# interpretation 
"β1 = 72.774 ; β2 = 83.659 ; β3 = -13.796 ; β4 = 1.191
δ1 = 51.185 ; δ2 = 108.286 ; δ3 = -20.015 ; δ4 = 1.613
The estimated coefficients have the expected signs. β1 and δ1 are TFC(Total Fixed Cost), 
which is always positive. The remaining parts of the cost function are TVC(Total Variable Cost).
After using calculus, we know the curve of TVC is reasonable 
(according to the concept of TVC we learn at microeconomics), 
indicating the signs on all coefficients estimated are correct."

# (b)
data1$e1 = data1$c1 - (72.774 + 83.659*data1$q1 - 13.796*data1$q1_2 + 1.191*data1$q1_3)
data1$e2 = data1$c2 - (51.185 + 108.286*data1$q2 - 20.015*data1$q2_2 + 1.613*data1$q2_3)

var.test(data1$e1, data1$e2)
# check heteroskedasticity
# p-value = 0.01534 (<0.1)?Areject Ho which claims residuals to be homoskedastic

# (c)
data1$c_total <- data1$c1 + data1$c2  # calculate total cost
data1$q_total <- data1$q1 + data1$q2  # calculate q1+q2
data1$q_2_total <- data1$q1_2 + data1$q2_2  # calculate q1_2+q2_2 (the sum of quadratic term)
data1$q_3_total <- data1$q1_3 + data1$q2_3  # calculate q1_3+q2_3 (the sum of cubic term)
regression3 <- lm(c_total ~ q_total + q_2_total + q_3_total, data = data1)
print(regression3)
# interpretation
"β1 = δ1 = 158.647 ; β2 = δ2 = 86.075 ; β3 = δ3 = -15.240 ; β4 = δ4 = 1.312
The signs on each coefficient are positive, positive, negative and positive,
which is reasonable for the same reason as (a)."

# (d)
library(car)
regression4 <- lm(c_total ~ q1 + q2 + q1_2 + q2_2 + q1_3 + q2_3, data = data1) 
linearHypothesis(regression4, c("q1 = q2", "q1_2 = q2_2", "q1_3 = q2_3"))
# p-value is >0.1
# cannot reject Ho which claims the coefficients in 2 models are the same


# 8.12
# (a)
data2 <- read.dta("pubexp.dta")
# potential heteroskedasticity problem
"The regression provided omit variables that have an impact on Yi(expenditure on education).
For example, the intelligence everyone possesses(omitted variable).
A smarter person is likely to invest in himself more, that is, he is willing to
spend more money on the education. But the regression provided doesn't include
this variable, making the variance of error terms may differ from one another,
resulting in heteroskedasticity."

# (b)
data2$ee_p <- data2$ee / data2$p  # Yi
data2$gdp_p <- data2$gdp / data2$p  # Xi
regression5 <- lm(ee_p ~ gdp_p, data = data2)
print(regression5)
#print(summary(regression5))
plot(ee_p ~ gdp_p, data = data2)  # plot the regression function
abline(lm(ee_p ~ gdp_p, data = data2))  # add the regression line
par(mfrow=c(2,2))  # present 4 plots in 1 panel
plot(regression5)  # plot the residuals and others
# interpretation
"What we need is the top-left plot.
If there is no heteroskedastity, there should be a random distribution of points 
throughout the range of X axis and a flat red line.
But we can see that the red line is slightly curved and the residuals seem to decrease
as the fitted Y values increase. So heteroskedasticity exists."

# (c)
library(lmtest)
bptest(regression5, ~ gdp_p + I((gdp_p) ^ 2), data = data2)
# Use Breusch-Pagan Test to test heteroskedasticity
# p-value = 0.006869 (<0.05). Reject Ho that claims homoskedasticity

# (d)
library(sandwich)
sqrt(vcovHC(regression5, type = "HC0")[2, 2])  # type HC0 is suggested by White
coeftest(regression5, vcov = vcovHC(regression5, type = "HC0"))
# Both above codes compute the standard error using White's formula is 0.0060262
# Standard error in (b): 0.005179 (using summary command)

confint(regression5,'gdp_p' ,level=0.95)
regression55 <- lm(ee_p  ~ gdp_p, data = data2, weights = 1/gdp_p)
confint(regression55,'gdp_p' ,level=0.95)

b2_mean <- mean(regression5$coefficients[2])  # mean(b2)
n <- nrow(data2)  # n = 34
b2_sd_b <- 0.005179  # sd(b2) in (b)
b2_sd_d <- 0.0060262  # sd(b2) in (d)

b_CI <- cbind(CI_lower = b2_mean - 1.96 * b2_sd_b / n, CI_upper = b2_mean + 1.96 * b2_sd_b / n)
d_CI <- cbind(CI_lower = b2_mean - 1.96 * b2_sd_d / n, CI_upper = b2_mean + 1.96 * b2_sd_d / n)
print(b_CI)
print(d_CI)
# The confidence interval that ignores the heteroskedasticity(case in (b)) is smaller.

# (e)
library(nlme)
regression6 <- gls(ee_p ~ gdp_p, data = data2, weights = varPower())
print(summary(regression6))
b2_sd_e <- 0.004235666  # sd(b2) in (e)
e_CI <- cbind(CI_lower = b2_mean - 1.96 * b2_sd_e / n, CI_upper = b2_mean + 1.96 * b2_sd_e / n)
print(e_CI)
# interpretation
"β1 = -0.06566439 ; β2 = 0.06488641
Regression: yi = -0.06566439 + 0.06488641*xi + ei
Per capita GDP increases $1 results in the increase of the expenditure
on education per person by $(0.065)
GLS allows the errors to have unequal variances.
The confidence interval using GLS (case in (e)) is smaller than that in (d) and (b)."


# 9.15
# (a)
library(foreign)
library(lmtest)
data3 <- read.dta("mining.dta")
data3$pro_t<- ts(data3$pro, start = 1972, freq = 4)
data3$pow_t<- ts(data3$pow, start = 1972, freq = 4)
data3$t <- seq_along(data3$year)  # add time trend
data3$t_2 <- (data3$t) ^ 2
data3$t_t<- ts(data3$t, start = 1972, freq = 4)
data3$t_2_t<- ts(data3$t_2, start = 1972, freq = 4)

regression7 <- lm(log(pow) ~ t_t + t_2_t + log(pro), data = data3)
print(regression7)
dwtest(log(data3$pow) ~ data3$t + data3$t_2 + log(data3$pro))  # Durbin-Watson Test
# interpretation
"The signs on coefficients are negative, positive, negative and positive.
The mining production and time are positively correlated with electric power use
for mining, which is reasonable, for the reason that the bigger the production, 
the more the power used in mining. The power consumed is higher as time goes by, 
but the difference is not so significant, thus the quadratic term is negative,
meaning the amount of increase is becoming smaller throughout the period.
The p-value is near 0(<0.05). We can reject H0 which claims there is no autocorrelation"

# (b)
library(dynlm)
regression8 <- dynlm(formula = log(pow_t) ~ t_t + t_2_t + log(L(pow_t, 1))+ log(L(pro_t, 0:1)), data = data3)
print(summary(regression8))
# I include 1 lag of POW and 1 lag of PRO (t=0 & t=1) because in this case R^2 is the highest.

# (c)
linearHypothesis(regression7, c("log(pro) = 1"))
# p-value is 0.4817(>0.1). Cannot reject the null that claims the coefficient of log(PRO) is 1.
linearHypothesis(regression8, "log(L(pro_t, 0:1))0 = 1")
# p-value is 0.8704(>0.1). Cannot reject the null that claims the coefficient of log(L(pro_t)) is 1.

#(d)
regression8_lag <- coef(regression8)[4:6]
total_multiplier <- sum(regression8_lag)
regression8_lag 
print(total_multiplier)
# The multiplier(B) is the sum of all lag term coefficients.
# B = 0.9025823

# 9.17
# (a)
data4 <- read.dta("robbery.dta")
data4$tt <- seq_along(data4$rob)  # add time trend, t = 1~118
data4$rob_t<- ts(data4$rob, start = 1966)
regression9 <- dynlm(rob_t ~ tt + L(rob_t, 1), data = data4)
print(summary(regression9))
acf(regression9$residuals, main = "Autocorrelation Of The Residuals")
# As we can see from the correlogram, there's evident sign of errors being correlated.
# Note: ACF at lag 0 equals 1 by default(i,e., the correlation of a time series with itself)

# (b)
November_1975 <- predict(regression9 , newdata = data.frame("tt"=119, 'rob_t' = 431), interval='predict', level=0.95)
print(November_1975)
# The 95% confidence interval for the number of armed robberies in November: 408.9313~437.5228
tmp<-ts(data.frame('rob'= 423.2271,"tt"=119, 'rob_t'=423.2271))
data5<-rbind(tmp,data4)
ordered_rows <- order(data5$tt)
data5 <- data5[ordered_rows, ]
rownames(data5 ) <- NULL
data5$rob<- ts(data5$rob)
data5$rob_t<- ts(data5$rob_t)
data5$tt<- ts(data5$tt)
regression10 <- dynlm(rob_t ~ tt + L(rob_t, 1), data = data5)
print(summary(regression10))


December_1975 <- predict(regression10 , newdata = data.frame("tt"=120, 'rob_t' = 423.2271), interval='predict', level=0.95)
print(December_1975)
# The 95% confidence interval for the number of armed robberies in December: 410.4896~439.0874

