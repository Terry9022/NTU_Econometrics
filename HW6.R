install.packages('moments')
# 16.10
library(foreign)
library(stats)
library(ggplot2)
library(moments)

# (a)
data1 <- read.dta("olympics.dta")
data1$share=0
data88=data1[data1$year==88,]
data92=data1[data1$year==92,]
data96=data1[data1$year==96,]
data88$share=data88$medaltot/738
data92$share=data92$medaltot/815
data96$share=data96$medaltot/842
data2=rbind(data88,data92,data96) 
data3=data2[complete.cases(data2), ]

#Plot histogram
qplot(x=data3$share, data=data3,  geom="histogram",  # 圖形=histogram
      main = "Histogram of Share",  xlab="Share",binwidth=0.01)

# (b)
#(i)
regression1 <- lm(share ~ log(pop)+log(gdp)+host+soviet, data = data3)
print(summary(regression1))
#(ii)
plot(log(data3$gdp),regression1$residuals) 
#(iii)
skewness(regression1$residuals)
kurtosis(regression1$residuals)

# (c)
Canada=predict(regression1, data.frame("pop"=30689000, 'gdp' = 6.41256e+11,"host"=0,"soviet"=0))
print(Canada)
Austraila=predict(regression1, data.frame("pop"=19071000, 'gdp' = 3.22224e+11,"host"=1,"soviet"=0))
print(Austraila)

# (d)
library(AER)
regression2 <-tobit(share ~ log(pop)+log(gdp) + host+soviet,  data = data3)
summary(regression2)

# (e)
sigma <- regression2$scale
Canada_2=predict(regression2, data.frame("pop"=30689000, 'gdp' = 6.41256e+11,"host"=0,"soviet"=0))
Canada_3=Canada_2+ sigma*(dnorm(Canada_2/sigma )/pnorm(Canada_2/sigma ))
print(Canada_3)

sigma <- regression2$scale
Austraila_2=predict(regression2, data.frame("pop"=19071000, 'gdp' = 3.22224e+11,"host"=1,"soviet"=0))
Austraila_3=Austraila_2+ sigma*(dnorm(Austraila_2/sigma )/pnorm(Austraila_2/sigma ))
print(Austraila_3)


# 16.11
install.packages("mfx")
install.packages("sampleSelection")
install.packages("rquery")

# (a)
library(foreign)
library(stats)
data2 <- read.dta("oscar.dta")
probit <- glm(winner ~ nominations + gglobes, data = data2, family = binomial(link = 'probit'))
print(summary(probit))
# interpretation
"(1) The coefficients are significant on the significance level 1%.
 (2) The signs are positive, as anticipated. 
     The more the nominations and the Golden Globes won, the greater the probability of winning the Oscar."

# (b)
library(mfx)
marg_eff = probitmfx(winner ~ nominations + gglobes, data = data2)
print(marg_eff)
# The marginal effect of an additional NOMINATION on the probability of winning the Oscar is 0.043

# (c)
# The code is as that in (b)
# The marginal effect of an additional GGLOBE on the probability of winning the Oscar is 0.07

# (d)
library(sampleSelection)
library(dplyr)
win_predict = predict(probit, type = "response")
# type = "response" returns the predicted probability

  
for(i in c(1:100)){ data2$win_predict[i] <- win_predict[i] }

new <- data2 %>% 
  group_by(year) %>% 
  summarize(highest_predict = max(win_predict))

data2 <- merge(data2, new, by = "year")

for(i in c(1:100)){
  if((data2$win_predict[i] == data2$highest_predict[i]) && (data2$winner[i] == 1)){
    data2$correct_pred[i] <- 1 } else { data2$correct_pred[i] <- 0 }}

correct_count <- 0
for(i in c(1:100)){ if(data2$correct_pred[i] == 1){ correct_count = correct_count + 1 }}
print(correct_count / 20)  # 1984~2003: 10 years
# percentage of correct predictions: 85%

# (e)
library(rquery)
newdata = data2[data2$year == 2004, ]
pred_2004 <- predict(probit, newdata, type = "response")
pred_2004 <- data.frame("year" = 2004, "title" = newdata$title, "win_predict" = c(pred_2004))
new_2004 <- pred_2004 %>%
  group_by(year) %>%
  summarize(highest_predict = max(win_predict))
pred_2004 <- merge(pred_2004, new_2004, by = "year")

for(i in c(1:5)){
  if((pred_2004$win_predict[i] == pred_2004$highest_predict[i])){ 
    winner = pred_2004$title[i]} }
print(as.character(winner))
# The predicted winner: The Aviator
# The actual winner: Million Dollar Baby
# The prediction is not valid during 2004.


