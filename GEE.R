#install.packages("geepack")
#install.packages("MESS")
library(geepack)
library(MESS)
data("respiratory")
str(respiratory)
a=glm(outcome ~ baseline + center + sex + treat + age + I(age^2),data = respiratory, family = binomial)
summary(a)
coef(summary(a))
m.ex <- geeglm(outcome ~ baseline + center + sex + treat + age + I(age^2),
                data = respiratory, id = interaction(center, id),
                family = binomial, corstr = "exchangeable")
summary(m.ex)
QIC(m.ex)
