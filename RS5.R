#Today we will be performing a simple linear regression and multiple linear regression
#We'll be using some cell count data for this analysis

#Note: I'm not a statistician
#You'll most likely need to do some research
#to figure out what type of regression is best for your data

#set working directory
setwd("~/Mitchell Lab Stuff/Data")

#load data set
mydata <- read.csv("RS5_data.csv")

#quickview
head(mydata)

#load packages- you may have to install tidyverse
library(ggplot2)
library(tidyverse)

#First let's look at our data for Ph_Index
#For reference Ph_Index= (#MiG TUNEL)/(#total TUNEL)

#basic plot- will produce warning messages
ggplot(mydata, aes(x=genotype, y=Ph_Index))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.1))

#basic plot- tell R to remove the NA values- no warning message
ggplot(mydata, aes(x=genotype, y=Ph_Index))+
  geom_boxplot(na.rm = TRUE)+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE)

#add color to genotype so you can see outliers
ggplot(mydata, aes(x=genotype, y=Ph_Index))+
  geom_boxplot(na.rm = TRUE)+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE, aes(color=genotype))

#We want to assess differences among genotypes for Ph_Index
#Phagocytic index is a proportion with values ranging from 0-1
#we could run an anova
res.aov<- aov(Ph_Index ~ genotype, data = mydata)

summary(res.aov)
TukeyHSD(res.aov)

#but regression is a better statistical test for this type of data
#we should actually be performing logistic regression for this data...
#but we're going to go through linear regression first

#basic linear model

mod <-lm(Ph_Index ~genotype, data=mydata)

summary(mod)

#P-value of F-statistic is non-significant
#so genotype is not a significant predictor of the outcome Ph_Index

#multiple linear model 
#Assess the effects of multiple predictor values on the outcome at the same time
model <-lm(Ph_Index ~genotype +Number_MiG + MiG_TUNEL + phagocytic_load, 
           data = mydata)

#print summary of your model
summary(model)
#P-value of F statistic is significant 
#at least one predictor variable is related to the outcome variable

#which variable are significant?
summary(model)$coefficient
#het, mut, Number_MiG, and phagocytic load are significant predictors of ph_index

#Looking at the estimate, mutant genotype has a negative affect on ph_index
#while the other significant predictors have positive effects.

#run model on significant predictors only
model2 <-lm(Ph_Index ~genotype +Number_MiG+phagocytic_load, data = mydata)
#produces a better model
summary(model2)
#print confident intervals
confint(model2)

#residual standard error or sigma
#tells you the error rate of your model

sigma(model2)/mean(mydata$Ph_Index) #produces NA

mydata.clean <-na.omit(mydata) #remove NA values

sigma(model2)/mean(mydata.clean$Ph_Index) 
#32% error rate in your model

#checking normality and homoscedasticity

#define residuals
res <- resid(model2)

#produce residual vs. fitted plot
plot(fitted(model2), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 



#additional info

#run one variable against all other variables in your dataset
model3 <- lm(Ph_Index~., data = mydata)

summary(model3)

#all variables except batch
model4 <- lm(Ph_Index ~. -batch, data = mydata)
summary(model4)

#or
model3B <- update(model3,  ~. -batch)
summary(model3B)



#the glm() function can also be used when you want to fit more complext models
# or designate a "family" 
#for example, logistic regression (family=binomial)
#or Poisson regression (family=poisson)

glm.model1 <-glm(Ph_Index ~ genotype, 
                family=binomial, data=mydata.clean)

summary(glm.model1)

#Now let's put this into R markdown...
