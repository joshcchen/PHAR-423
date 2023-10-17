library(tidyverse)
df <- read.csv("workshops/data/gusto.csv")

df %>% summarise(mean(age), sd(age), n())

table(df$tx)

# Mortality Rate of GUSTO Trial (2 Methods)
x <- table(df$day30)
x[2]/nrow(df)
sum(df$day30)/nrow(df)

# Filtering and Mutating df: Filter removes double therapy patients and mutate 
# creates a new column tx2 assigning treatment as numerical values
df2 <- df %>% filter(tx!="SK+tPA") %>% mutate(tx2=ifelse(tx=="tPA", 1, 0))
table(df2$tx2)

# 2x2 Table: Top = Day 30 Side = Treatment
x <- table(df2$tx2, df2$day30)
a <- x[1,1]
b <- x[1,2]
c <- x[2,1]
d <- x[2,2]

RR <- (d/(c+d)) / (b/(a+b))
OR <- ((a*d)/(b*c))

# 95% CI for OR
SE_log_OR <- sqrt((1/a) + (1/b) + (1/c) + (1/d))

log_OR <- log(OR)

lb <- log_OR - 1.96*SE_log_OR
ub <- log_OR + 1.96*SE_log_OR

lb2 <- exp(lb)
ub2 <- exp(ub)

# 95% CI for RR
SE_log_RR <- sqrt((1/a) - (1/(a+b)) + (1/c) - (1/(c+d)))

log_RR <- log(RR)

lbRR <- log_RR - 1.96*SE_log_RR
ubRR <- log_RR + 1.96*SE_log_RR

exp(lbRR)
exp(ubRR)

# Oct 17
# Making sex binary Female = 0 Male = 1
df2 <- df2 %>% mutate(sex2=ifelse(sex=="male",1,0))

#Linear regression model: dependent variable = pulse, independent = sex2 and age
reg0 <- lm(pulse~sex2+age, data=df2)

#Given the coefficents, sex2 = -2 so controlling for age, males have 2 BPM units lower pulse
#age = -0.06 so controlling for sex, for every 1 year increase in age pulse decreases by 0.06 BPM
summary(reg0)

#Are residuals normally distributed?
hist(reg0$residuals)

#How does the model fit the data?


#logistic regression model

#Let's review the logistic function
x <- seq(from=-10, to=10, by=0.01)
y <- exp(x)/(1+exp(x))
plot(x,y,type='l')

#Unadjusted (Note that coefficients are given as ln(OR))
reg1 <- glm(day30~tx2, data=df2, family=binomial(link="logit"))
summary(reg1)
betas <- coefficients(reg1)
exp(betas[2])

sm <- summary(reg1)
se <- sm$coefficients[2,2]

CI_lower <- betas[2] - 1.96*se
CI_upper <- betas[2] + 1.96*se

exp(CI_lower)
exp(CI_upper)


#adjusted
reg2 <- glm(day30~tx2+age+sex2, data=df2, family=binomial(link="logit"))
betas2 <- coefficients(reg2)

#Get 95% confidence interval
CI_lower <- coefficients(reg2)[2] - 1.96*summary(reg2)$coefficients[2,2]
CI_upper <- coefficients(reg2)[2] + 1.96*summary(reg2)$coefficients[2,2] 

#Make a prediction for a 65 y/o female
df3 <- df2[1,]
df3$age <- 65
df3$sex <- 0

predict(reg2, newdata=df3, type='response')





