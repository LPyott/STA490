#Chapter 13 Apr 3–9: Missing Data

#Identify situations that lead to missing data in health professions education data.
#Explore different strategies for imputing missing data.
#Use and compare three strategies to compensate for missing data.
#https://bookdown.org/anshul302/HE902-MGHIHP-Spring2020/Missing.html

#Chapter 13.2
if (!require(foreign)) install.packages('foreign')
library(foreign)

diabetes <- read.spss("diabetes.sav", to.data.frame = TRUE)
library(dplyr)

d <- diabetes %>%
  dplyr::select(total_cholesterol, age, gender, weight)

names(d)

if (!require(missForest)) install.packages('missForest')
library(missForest)

d.na <- prodNA(d, noNA = 0.3)

nrow(d)
nrow(d.na)

#Chapter 13.3
head(d.na,n=10)

#Chapter 13.3.2
if (!require(mice)) install.packages('mice')
library(mice)

#Chapter 13.3.2.1
md.pattern(d.na)
md.pattern(d)

#Chapter 13.3.2.2
md.pairs(d.na)

#Ch 13.4
summary(reg.true <- lm(total_cholesterol ~ weight + age + gender, data = d))

#Ch 13.5.1
head(d.na, n=10)

summary(reg.del <- lm(total_cholesterol ~ weight + age + gender, data = d.na))
nobs(reg.del)

#Ch 13.5.1.1
library(dplyr)

NewData <- OldData %>%
  dplyr::select(var1, var2, var3)

EvenNewerData <- na.omit(NewData)

#Ch 13.5.3 Mean Imputation
summary(d.na)
head(d.na,n=10)

d.na.mean <- d.na # make a copy

d.na.mean$total_cholesterol <- ifelse(is.na(d.na$total_cholesterol),mean(d.na$total_cholesterol,na.rm = T),d.na.mean$total_cholesterol)

d.na.mean$age <- ifelse(is.na(d.na$age),mean(d.na$age,na.rm = T),d.na.mean$age)

d.na.mean$weight <- ifelse(is.na(d.na$weight),mean(d.na$weight,na.rm = T),d.na.mean$weight)

d.na.mean[which(is.na(d.na.mean$gender)),]$gender <- "female"

head(d.na.mean,n=10)

summary(reg.mean <- lm(total_cholesterol ~ weight + age + gender, data = d.na.mean))

#Ch 13.5.4.2 Imputation via MICE (multivariate imputations via chained equations)
library(mice)

d.na.mice <- mice(d.na)

class(d.na.mice)

temp <- complete(d.na.mice,1)
summary(temp)
temp <- complete(d.na.mice,2)
summary(temp)

#Ch 13.5.4.3 
reg.mice <- with(d.na.mice, lm(total_cholesterol ~ weight + age + gender))
reg.mice.pooled <- pool(reg.mice)
summary(reg.mice.pooled)

d.na.mice

reg.mice.pooled

#CH 13.8 Assignment

