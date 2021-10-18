#################################
##      Econometrics II        ##
##    Spring 2020 Midterms     ##
##                             ##
##        Name:呂紀廷          ##
##   Student ID: B061009022    ##
#################################

library(estimatr)
library(foreign)
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

#2
#(i)
data1 <- read.dta("428131_phillips.dta")
data1_until2002<-data1[0:55,]
data1_until2002$unem_t<-ts(data1_until2002$unem,start=1948)
data1_until2002$inf_t<-ts(data1_until2002$inf,start=1948)
regression1<-dynlm(unem_t ~ inf_t + L(unem_t,1), data = data1_until2002)
print(summary(regression1))
predict2003_1<- predict(regression1, data.frame("unem_t"=5.8, 'inf_t' = 1.6))
print(predict2003_1)
#(ii)
regression2<-dynlm(unem_t ~ L(inf_t,0:1) + L(unem_t,1), data = data1_until2002)
print(summary(regression2))
#(iii)
predict2003_2<- 1.362212+-0.09597*1.6+0.25014 *2.8+0.65390*5.8
print(predict2003_2)

#3
#(i)
data2 <- read.dta("428131_airfare.dta")
data2$logdist_2<-(log(data2$dist))^2
regression3<-plm(log(fare) ~ concen + log(dist) + logdist_2 + y98+y99+y00, data = data2,model='pooling') 
print(summary(regression3))
#(ii)
confint(regression3, 'concen')
regression3_2 <- coeftest(regression3, vcov = vcovHC(regression3, type = "HC1"))  # gls regression,
print(regression3_2)
confint(regression3_2, 'concen')
#(iii)
plot(lfare ~ ldist, data = data2) 
abline(lm(lfare ~ ldist, data = data2))
plot(lfare ~ dist, data = data2) 
plot(fare ~ dist, data = data2) 
#(iv)
regression4<-plm(log(fare) ~ concen + log(dist) + logdist_2 + y98+y99+y00, data = data2,index=c('id'),model='random') 
print(summary(regression4))
#(v)
regression5<-plm(log(fare) ~ concen + log(dist) + logdist_2 + y98+y99+y00, data = data2,index=c('id'),model='within') 
print(summary(regression5))

#4
#(i)
data3 <- read.dta("428131_MURDER.dta",convert.factors = FALSE)
atleast_one_exec<-data3[data3$exec>=1,]
nrow(atleast_one_exec)
max(atleast_one_exec$exec)
#(ii)
data3_only90and93<-data3[data3$year>=90,]
data3_only90and93_2<-data3_only90and93[,c('mrdrte','d93','exec','unem')]
data3_only90and93_2$mrdrte<-as.numeric(data3_only90and93_2$mrdrte)
data3_only90and93_2$d93<-as.numeric(data3_only90and93_2$d93)
data3_only90and93_2$exec<-as.numeric(data3_only90and93_2$exec)
data3_only90and93_2$unem<-as.numeric(data3_only90and93_2$unem)

regression6<-lm(mrdrte ~ d93 + exec+unem, data =data3_only90and93_2) 
print(summary(regression6))
#(iii)
data3_only90<-data3_only90and93_2[data3_only90and93_2$d93==0,]
data3_only93<-data3_only90and93_2[data3_only90and93_2$d93==1,]
data3_differ<-data3_only93-data3_only90
regression7<-lm(mrdrte ~exec+unem, data = data3_differ) 
print(summary(regression7))
#(iv)
regression8<-dynlm(L(exec,0) ~ L(exec,1), data = data3_differ) 
print(summary(regression8))


#5
#(i)
data4 <- read.dta("428131_SMOKE.dta",convert.factors = FALSE)

#(iv)
data4$age_2<-data4$age^2
regression9<-lm(log(income) ~ cigs + educ+age+age_2, data =data4) 
print(summary(regression9))

#(v)
regression10<-lm(cigs ~  + educ+age+age_2+log(cigpric)+restaurn, data =data4) 
print(summary(regression10))

#(iv)
iv1 <- ivreg(log(income) ~ cigs + educ+age+age_2 | educ+age+age_2+log(cigpric)+restaurn
             ,data = data4)
print(summary(iv1, vcov = sandwich, diagnostics = TRUE))





















