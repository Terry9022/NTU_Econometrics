#install.packages("MASS")
#install.packages("knitr")

# 16.2
library(foreign)
library(stats)

# (a)
data1 <- read.dta("transport.dta")
logit <- glm(auto ~ dtime, data = data1, family = "binomial")  # family = "binomial": logit regression
print(summary(logit))
# interpretation
"logit(p) = -0.23758 + 0.05311 * DTIME
 β1 + β2 * DTIMEi = -0.23758 + 0.05311 * DTIMEi
        (se)        (0.75048)  (0.02064)
 probit: β1 = -0.0644, se(β1) = 0.3992, β2 = 0.0299, se(β2) = 0.0103
 logit: β1 = -0.23758, se(β1) = 0.75048, β2 = 0.05311, se(β2) = 0.02064
 Estimate β1 and its standard error in logit model are smaller than those in probit.
 Estimate β2 and its standard error in logit model are bigger than those in probit."

# (b)
dlogis(0.82462)  # 0.2118923
# interpretation
"DTIME = 20, using logistic density function
 marginal effect = λ(-0.23758 + 0.05311 * 20) * 0.05311 = λ(0.82462) * 0.05311 = 0.2118923 * 0.05311 = 0.0112536
 (λ is the pdf for a logistic variable)
 Marginal effect using probit model is 0.0104: a 10-minute increase in DTIME increases the probability of driving by 10.4%.
 Marginal effect using logit model is 0.0112536: a 10-minute increase in DTIME increases the probability of driving by 11.25%."

# (c)
plogis(1.3557)  # 0.7950599
# interpretation
"DTIME = 30, using logistic distribution function
 probability = Λ(-0.23758 * 0.05311 * 30) = Λ(1.3557) = 0.7950599 (Λ is the CDF for a logistic variable)
 probability effect in logit model is 0.795, and that in probit model is 0.798.
 There is little differnce between 2 predicted probabilities."

# (d)
auto_predict <- predict(logit, newdata = data1, type = "response")
# type = "response" gives predicted probability for logit model
for(i in c(1:21)){ data1$auto_predict[i] <- auto_predict[i] }
for(i in c(1:21)){
  if((data1$auto[i] == 1 && data1$auto_predict[i] >= 0.5) | (data1$auto[i] == 0 && data1$auto_predict[i] < 0.5)){
    data1$correct_pred[i] <- 1 }else{ data1$correct_pred[i] <- 0 }}
correct_count <- 0
for(i in c(1:21)){ if(data1$correct_pred[i] == 1){ correct_count = correct_count + 1 }}
print(correct_count / 21)
# 90.476% of the predictions are correct when using logit model.


# 16.6
library(nnet)
library(knitr)

# (a)
data2 <- read.dta('nels_small.dta')
data2$factor_pse <- factor(data2$psechoice)  # turn Y value into category (data type)
data2$factor_pse <- relevel(data2$factor_pse, ref = 1)  # choose the group not going to college (psechoice = 1) to be base group
regression1 <- multinom(factor_pse ~ grades + faminc + female + black, data = data2)
reg1 <- summary(regression1)

z <- reg1$coefficients / reg1$standard.errors
p_value <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed Z test

reg1_sum <- rbind(reg1$coefficients[1,], reg1$standard.errors[1,], p_value[1,],
                  reg1$coefficients[2,], reg1$standard.errors[2,], p_value[2,])
rownames(reg1_sum) <- c("Coef_2", "SE_2", "p-value_2", "Coef_3", "SE_3", "p-value_3")
# _2 for group2 (psechoice = 2), _3 for group3 (psechoice = 3)
kable(reg1_sum)
# interpretation
"For PSECHOICE = 2, the estimated coefficients on GRADES and FAMINC are significant at 5% (base on p_values)
 For PSECHOICE = 3, the estimated coefficients on GRADES, FAMINC and BLACK are significant at 5% (base on p_values)"

# (b)
g_med <- median(data2$grades)  # 6.64
f_med <- median(data2$faminc)  # 42.5
result1 <- predict(regression1, newdata = data.frame(grades = g_med, faminc = f_med, female = 0, black = 0), type = "prob")
# type = "prob" predicts probability of class
result1[3]  # 4-year college: PSECHOICE = 3 (the 3rd class)
# The probability of a white man with median GRADES and FAMINC attending 4-year college is 0.5239

# (c)
result1[3] / result1[1] 
# interpretation
"Probability ratio of a white man with median GRADES and FAMINC attending 4-year college rather than not
 attending any is 2.7278"

# (d)
result2 <- predict(regression1, newdata = data.frame(grades= 4.905, faminc = f_med, female=0, black=0), type = "prob")
result2[3] - result1[3] 
# interpretation
"The increase in the probability of attending 4-year college of a white man with median GRADES and FAMINC changing
 from 6.64 to 4.905 is 0.2081"

# (e)
data2_2 <- data2[data2$psechoice!=2, ]
data2_2$factor_pse <- relevel(data2_2$factor_pse, ref = 1)
regression2 <- multinom(factor_pse ~ grades + faminc + female + black, data = data2_2)
summary(regression2)
result3 <- predict(regression2, newdata = data.frame(grades= g_med, faminc = f_med, female=0, black=0), type = "prob")

result3 / (1-result3) 
# interpretation
"Probability ratio of a white man with median GRADES and FAMINC attending 4-year college rather than not
 attending any is 2.7592"


# 16.8
library(MASS)

# (a)
pnorm(-0.909776)  # 0.1814703
pnorm(-0.054176) - pnorm(-0.909776)  # 0.2969271
1 - pnorm(-0.054176)  # 0.5216025
# interpretation (Φ is the CDF for normal distribution)
"No college: P(y=1) = Φ(-2.9456-(-0.3066*6.64)) = Φ(-0.909776) = 0.1815
 2-year college: P(y=2) = Φ(-2.0900-(-0.3066*6.64)) - Φ(-2.9456-(-0.3066*6.64)) = Φ(-0.054176)-Φ(-0.909776) = 0.2970
 4-year college: P(y-3) = 1 - Φ(-2.0900-(-0.3066*6.64)) = 0.5216"
pnorm(-1.441727)  # 0.0746897
pnorm(-0.586127) - pnorm(-1.441727)  # 0.2042054
1 - pnorm(-0.586127)  # 0.7211049
"No college: P(y=1) = Φ(-2.9456-(-0.3066*4.905)) = Φ(-1.441727) = 0.07469
 2-year college: P(y=2) = Φ(-2.0900-(-0.3066*4.905)) - Φ(-2.9456-(-0.3066*4.905)) = Φ(-0.586127)-Φ(-1.441727) = 0.2042
 4-year college: P(y-3) = 1 - Φ(-2.0900-(-0.3066*4.905)) = 0.7211

 The results are anticipated since we expect the probability of attending 4-year college to be higher, and
 that of not going to college to be lower, for students with better grades."

# (b)
regression3 <- polr(factor_pse ~ grades + faminc + famsiz + black + parcoll, data = data2, method = "probit") 
# polr: for ordinal probit model
print(summary(regression3))
# interpretation
"The estimates suggest that as student's grades increase, family income increases, a black student, or a student with
 the parents graduated from college, has a higher probability of choosing 4-year college.
 Based on t-values, all variables except FAMSIZ are significant at 5%."

#(c)
regression4 <- polr(factor_pse  ~ grades, data = data2, method = 'probit')
print(summary(regression4))

teststat <- 2*(as.numeric(logLik(regression3)) - as.numeric(logLik(regression4)))
teststat

df <- length(coef(regression3)) - length(coef(regression4)) 
df  # degree of freedom: 4

pchisq(teststat, df = 4, lower.tail = FALSE)  # p-value
# interpretation
# https://rpubs.com/roes7096/LTR
# Based on p-values, we reject the null and conclude that FAMINC, FAMSIZ, BLACK and PARCOLL are jointly significant.

# (d)
# (i) GRADES = 6.64
result4 <- predict(regression3, newdata = data.frame(grades=6.64, faminc=52, famsiz=4, black=1, parcoll=1), type = "prob")
result4[3]  # the probability: 0.8525

# (ii) GRADES = 4.905
result5 <- predict(regression3, newdata = data.frame(grades=4.905, faminc=52, famsiz=4, black=1, parcoll=1), type = "prob")
result5[3]  # the probability: 0.9406

# (e)

result6 <- predict(regression3, newdata = data.frame(grades=6.64, faminc=52,famsiz=4, black=0, parcoll=1), type = "prob")
result6[3]  # the probability (GRADES = 6.64): 0.6309

result7 <- predict(regression3, newdata = data.frame(grades=4.905, faminc=52, famsiz=4, black=0, parcoll=1), type = "prob")
result7[3]  # the probability (GRADES = 4.905): 0.8013
# Given other conditions, the probability of a black student going 4-year college decreases as their grades increase.








