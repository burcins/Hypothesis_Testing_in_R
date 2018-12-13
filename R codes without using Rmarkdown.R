setwd("E:/dersler/Statistics 1/assignment 3")
library(tidyverse)
library(foreign)
library(nortest)
library(Hmisc)
library(car)
library(gmodels)
#Q1
salary <- read.spss("salary.sav", to.data.frame = T)
salary <- salary[,-1]
str(salary)
#Q2
sum(which(is.na(salary)))
summary(salary)
for(i in 1:ncol(salary)){
  if (class(salary[,i]) == "numeric"){
    hist(salary[i])
  }}
salary[which(salary$salbeg==max(salary$salbeg)),]
#Q3
beginning <- salary$salbeg
lillie.test(beginning)
shapiro.test(beginning)
qqnorm(beginning)
qqline(beginning)
multifunctions <- function(x){
  c(length=length(x),mean=mean(x),median=median(x))
}
multifunctions(beginning)
t.test(beginning, mu=1000)
#Q4
diff <- salary$salnow - salary$salbeg
lillie.test(diff)
shapiro.test(diff)
qqnorm(diff)
qqline(diff)
multifunctions(diff)
wilcox.test(diff, mu=0)
boxplot(salary$salbeg, salary$salnow)
boxplot(diff)
#Q5
males <- salary$salbeg[salary$sex=="MALES"]
females <- salary$salbeg[salary$sex=="FEMALES"]
by(salary$salbeg, salary$sex, lillie.test)
by(salary$salbeg, salary$sex, shapiro.test)
multifunctions(males)
multifunctions(females)
by(salary$salbeg, salary$sex, wilcox.test)
boxplot(males, females)
#Q6
salary$age_cut <- cut2(salary$age, g=3)   
anova <- aov(salbeg~age_cut, salary)
summary(anova)
lillie.test(anova$residuals)
shapiro.test(anova$residuals)
qqnorm(anova$residuals)
qqline(anova$residuals)

young <- salary$age[salary$age_cut=="[23.0,29.7)"]
middle <- salary$age[salary$age_cut=="[29.7,39.8)"]
old <- salary$age[salary$age_cut=="[39.8,64.5]"]
multifunctions(young)
multifunctions(middle)
multifunctions(old)

bartlett.test(salbeg~age_cut, salary)
fligner.test(salbeg~age_cut, salary)
leveneTest(salbeg~age_cut, salary)


oneway.test(salbeg~age_cut, salary, var.equal=FALSE)
kruskal.test(salbeg~age_cut, salary)


boxplot(salary$salbeg~salary$age_cut)
pairwise.t.test(salary$salbeg, salary$age_cut, pool.sd = F)
#Q7
whitef <- subset(salary, salary$sex=="FEMALES" & salary$minority=="WHITE")
whitem <- subset(salary, salary$sex=="MALES" & salary$minority=="WHITE")

(tab <- table(salary$sex, salary$minority))
prop.table(tab)
prop.table(tab,1)
prop.table(tab,2)
prop.test(tab)
chisq.test(tab)
summary(xtabs(~sex+minority, data=salary))
fisher.test(tab)
CrossTable(tab)
CrossTable(tab, digits = 1, format = "SPSS", prop.r = T, 
          chisq = T, 
           fisher = T)







