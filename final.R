#################################
##      Econometrics II        ##
##    Spring 2020 Finals       ##
##                             ##
##        Name: 呂紀廷         ##
##   Student ID: B06109022     ##
#################################


library(foreign)
library(stats)
library(nlme)
library(lmtest)
library(car)
library(sandwich)
library(dynlm)
library(plm)
library(dummies)
library(dplyr)
library(sjPlot)
library(readxl)
library(dLagM)
library(vars)
library(jtools)
library(gmm)
library(AER)
library(mfx)
library(nnet)
library(knitr)
library(stargazer)
library(permutations)
library(broom)
library(MASS)
library(ggplot2)
library(moments)
library(sampleSelection)
library(truncreg)
library(rquery)

#Q1
library(tseries)
#i
data1 <- read.dta("HSEINV.DTA")
data1$t_t<- ts(data1$t)  # transform the variable ROB into time-series form
data1$linvpc_t<- ts(data1$linvpc)
data1$lprice_t<- ts(data1$lprice)
df <- diff(data1$linvpc_t) 
adf.test(df, k=2)
#ii
df2 <- diff(data1$lprice_t) 
adf.test(df2, k=2)
#iii
bfx <- as.matrix(cbind(data1$linvpc_t,data1$lprice_t), demean=FALSE) 
po.test(bfx)

#Q2
#i
data2 <- read.dta("cps91.dta")
fraction=sum(data2$inlf)/length(data2$inlf)
fraction
#ii
data2_onlywork=data2[data2$inlf==1, ]
regression1 <- lm(lwage ~ educ + exper + expersq+black+hispanic, data = data2_onlywork)
print(summary(regression1))
#iii
regression2  <- glm(inlf ~ educ + exper + expersq+black+hispanic+nwifeinc+kidlt6, data = data2, family = binomial(link = 'probit'))
print(summary(regression2))
#iv
inverse_mill=dnorm(regression2$fitted.values)/ pnorm(regression2$fitted.values)
data2$inverse_mill=inverse_mill
data2_onlywork=data2[data2$inlf==1, ]
regression3 <- lm(lwage ~ educ + exper + expersq+black+hispanic+inverse_mill, data = data2_onlywork)
print(summary(regression3))


#Q3
#i
data3 <- read.dta("charity.dta")
fraction2=sum(data3$respond)/length(data3$respond)
fraction2
#ii
regression4  <- glm(respond ~ resplast + weekslast + propresp+mailsyear+avggift, data = data3, family = binomial(link = 'probit'))
print(summary(regression4))
#iii
marg_eff = probitmfx(respond ~ resplast + weekslast + propresp+mailsyear+avggift, data = data3)
print(marg_eff)
regression5  <- lm(respond ~ resplast + weekslast + propresp+mailsyear+avggift, data = data3)
print(summary(regression5))
#iv
regression6 <-tobit(gift ~ resplast + weekslast + propresp+mailsyear+avggift, data = data3)
summary(regression6)
#v
x1 <- 0.3348172
x2 <- 59.0481988  
x3 <-0.4843592 
xmailsyear<- 2.0495548
x5=18.2428372 

b1 <- coef(regression6)[[1]]
b2 <- coef(regression6)[[2]]
b3 <- coef(regression6)[[3]]
b4 <- coef(regression6)[[4]]
b5 <- coef(regression6)[[5]]
b6 <- coef(regression6)[[6]]
bSigma <- regression6$scale
Phactor <- pnorm((b1+b2*x1+b3*x2+b4*x3+b5*xmailsyear+b6*x5)/bSigma) 
mar_mailsyear <- xmailsyear*Phactor
print(mar_mailsyear)

regression7 <-lm(gift ~ resplast + weekslast + propresp+mailsyear+avggift, data = data3)
summary(regression7)
#Q4
#i
data4 <- read.dta("HTV.DTA")
regression8 <- lm(lwage ~ educ + abil + exper+nc+west+south+urban, data = data4)
print(summary(regression8))
#ii
data4_smallerthan20=data4[data4$wage<20,]
regression9 <- lm(lwage ~ educ + abil + exper+nc+west+south+urban, data =data4_smallerthan20)
print(summary(regression9))
#iii
regression10<- truncreg(lwage ~ educ + abil + exper+nc+west+south+urban, data =data4_smallerthan20,direction='right')
print(summary(regression10))








