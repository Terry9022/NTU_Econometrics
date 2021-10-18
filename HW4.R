# Exercise 4
# Q11.8

#(a)
library(foreign)
data1 <- read.dta("mroz.dta")
data1_only_womenwork<-data1[data1$lfp==1,]
data1_no_womenwork<-data1[data1$lfp==0,]
target_columns_work <- data1_only_womenwork[,c('age',"kidsl6","faminc",'lfp')]
target_columns_no_work <- data1_no_womenwork[,c('age',"kidsl6","faminc",'lfp')]
print(str(target_columns_work))
print(str(target_columns_no_work))
print(summary(target_columns_work))
print(summary(target_columns_no_work))
#We can that the family whose women work will earn more incomes than those women don't work. 
#Besides,the families of women not having work have more children whose ages is under six.

#(b)
data1$nwifeinc<-data1$faminc-data1$wage*data1$hours
one<-function(x){if(x<1){x=1}else{x}}
data1$wage<-mapply(one,data1$wage)
data1$wage_log<-log(data1$wage)
regression1 <- lm(hours ~ log(wage) + educ + age + kidsl6 + kids618 + nwifeinc, data = data1)
print(summary(regression1))
#I expect log(wage)'s sign is positive because more wages means the working hours are more.
#I expect education's sign is positive because more education means the woman is more likely to work.
#I expect age's sign is negative because more older means the woman is more unlikely to work.
#I expect kidsl6's and kids618's sign are negative because more children means the woman is more unlikely to work.
#nwifeinc measures the husband's incomes.
#I expect nwifeinc's sign is negative because more husband earns the woman is more unlikely to work.

#(c)
data1_only_womenwork<-data1[data1$lfp==1,]
regression2 <- lm(hours ~ wage_log + educ + age + kidsl6 + kids618 + nwifeinc, data = data1_only_womenwork)
print(summary(regression2))
#The result is different than expectation for log(wage) and education.
#Prehaps the reason is for these women who need to work to survive, they are even poorer. 
#Thus their edcation is ever shorter and their wage for job is even fewer even though their working is longer than average women.

#(d)
data1$exper_2 <- (data1$exper) ^ 2
data1_only_womenwork<-data1[data1$lfp==1,]
regression3 <- lm(wage_log ~ educ + age + kidsl6 + kids618 + nwifeinc + exper + exper_2, data = data1_only_womenwork)
print(summary(regression3))
#The effect upon wage of an additional year of education is 9.988e-02, which means that the more education is the more wage is .

#(e)
#In my opinion, the identification is better when the extra instruments EXPER and its square is added.
#That is because when we construct supply equation we must consider the employee's working experience which will influence wage a lot.

#(f)
library(AER)
iv1 = ivreg(hours ~ wage_log + educ + age + kidsl6 + kids618 + nwifeinc | educ + age + kidsl6 + kids618 + nwifeinc + exper + exper_2 
             ,data = data1_only_womenwork)
print(summary(iv1, vcov = sandwich, diagnostics = TRUE))
#the sign of wage is positive and it is significant.
#the sign of education is negative and it is significant.
#the sign of age, kidsl6, kids618, nwifeinc is negative and it is not significant.
