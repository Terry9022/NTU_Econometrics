# 15.12
# install packages
install.packages("plm")
install.packages("dummies")
install.packages("dplyr")

# (a)
library(foreign)
data1 <- read.dta("crime.dta")
# interpretation
"(i)  If the deterrence effect of legal system increases, the crime rate should decrease, thus having the negative sign.
(ii)  If wages increase, the crime rate should decrease, thus having the negative sign.
(iii) If the population density increases, the crime rate should increase, thus having the positive sign."

# (b)
regression1 <- lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg, data = data1)
print(summary(regression1))
# interpretation
"(i)  The signs are negative, negative, positive, positive and positive.
      The signs on the last 3 terms(PRBPRIS, AVGSEN, WMFG) are not the same as I expected in (a).
      Based on p-values, PRBARR, PRBCONV, PRBPRIS, WMFG are significant at significance level 5%, but AVGSEN isn't.
 (ii) Coefficient on PRBARR: -0.0348715. 
      Meaning that a 1% increase in the probability of arrest decreases crime rate by 0.03487%."

# (c)
library(plm)
library(lmtest)
#regression2 <- lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg + factor(county) - 1, data = data1)
#print(regression2)
reg.fe <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg, data = data1, 
                   index = c("county", "year"), model = "within")  # panel data
coeftest(reg.fe, vcov = vcovHC, type = "HC1")
# interpretation
"(i)  The signs are negative, negative, negative, positive and negative.
      The signs are much more similar to what I expected in (a) than those in (b).
      Based on p-values, PRBARR, PRBCONV, PRBPRIS, WMFG are significant at significance level 5%, but AVGSEN isn't.
(ii)  Coefficient on PRBARR: -0.0022717. 
      Meaning that a 1% increase in the probability of arrest decreases crime rate by 0.0022%.
      After controlling for fixed effect, the deterrent effect of the probability of arrest is smaller.
(iii) Coefficient on AVGSEN: 0.000068241. 
      Meaning that a 1-year increase in the average prison sentence makes almost no difference in crime rate.
      After controlling for fixed effect, the deterrent effect of the average prison sentence is smaller."

# (d)
reg.pooling <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg, data = data1,
                   index = c("county", "year"), model = "pooling")
pFtest(reg.fe, reg.pooling)  # jointly test the county level to be 0
# p-value is approximately 0. We can reject H0 that claims county level effects are jointly zero.

# (e)
library(dummies)
library(dplyr)
createDummyFeatures(data1, cols = "year")  # create dummies
data1 <- cbind(data1, dummy(data1$year, sep = "_"))  # add to the original dataframe
data1$data1_81 <- NULL  # drop one of the dummies to avoid multicollinearity
regression2 <- lm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg + density + pctymle
                  + data1_82 + data1_83 + data1_84 + data1_85 + data1_86 + data1_87, data = data1)  # without fixed effect
print(regression2)
regression3 <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg + density + pctymle
                   + data1_82 + data1_83 + data1_84 + data1_85 + data1_86 + data1_87, data = data1, 
                   index = c("county", "year"), model = "within")  # with fixed effect
print(regression3)
regression4 <- plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + wmfg + density + pctymle
                   + data1_82 + data1_83 + data1_84 + data1_85 + data1_86 + data1_87, data = data1,
                   index = c("county", "year"), model = "pooling")
pFtest(regression3, regression4)
# interpretation
"(i)  The results with fixed effect estimator are smaller for the variables originally in the regression
      obtained in (b) but are bigger for the other newly added variables.
(ii)  p-value is approximately 0. We can reject H0 that claims county level effects are jointly zero.
(iii) Coefficient on WMFG in the regression without fixed effect: (9.436e-06)
      Wage increases $1 increases cirme rate by (9.436e-06).
      Coefficient on WMFG in the regression with fixed effect: (-4.0794e-05)
      Wage increases $1 decreases crime rate by (4.0794e-05)"

# (f)
# interpretation
"To reduce the crime rate, it's the best to advocate prison sentence or male-child subsidy policy.
 The implementations of these 2 variables are likely to have a bigger effect."



