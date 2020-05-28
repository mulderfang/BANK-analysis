library(tidyverse)
library(forecast)
library(dummies)
#105304028 方品謙
# Q1
#(1)
bank<- read.csv("banks.csv")
logit.reg = glm(Financial.Condition~ TotExp.Assets + TotLns.Lses.Assets, data = bank ,family="binomial")
logit.reg
summary(logit.reg)
#(2)
TotLns.Lses.Assets = 0.6 ; TotExp.Assets = 0.11
# logit 
(logit.reg$coefficients[1]+ logit.reg$coefficients[2]*TotExp.Assets+logit.reg$coefficients[3]*TotLns.Lses.Assets)
# odds
exp(logit.reg$coefficients[1]+ logit.reg$coefficients[2]*TotExp.Assets+logit.reg$coefficients[3]*TotLns.Lses.Assets)
# probability
exp(logit)/(1+exp(logit))

# Q2
# Q3
universal<- read.csv("UniversalBank.csv")
#(1)
#Drop the columns of ID and ZIP code
universal <- universal[,-which(names(universal)=="ZIP.Code")]
universal <- universal[,-which(names(universal)=="ID")]
#Create dummy variables Under (Education=1)
universal <- universal %>% mutate( Under = ifelse(universal$Education == 1 ,1 ,0))
# Grad (Education=2)
universal <- universal %>% mutate( Grad = ifelse(universal$Education == 2 ,1 ,0))
# After that drop Education. 
universal <- universal[,-which(names(universal)=="Education")]

#(2)
library(caTools)
set.seed(9527)
index = sample.split(universal$Personal.Loan , SplitRatio=0.7)
train <- subset(universal, index == TRUE)
test <- subset(universal, index == FALSE)
nrow(universal)
nrow(train)
nrow(test)

# (3)
library(rpart)
library(rpart.plot)
library(partykit)

# CART model
universal_tree = rpart(Personal.Loan~., 
                    data = train, method="class", minbucket=25,
                    parms = list(split="information"))

plot(universal_tree)
text(universal_tree,pretty=0)

prp(universal_tree)
prp(universal_tree, type = 1, extra = 1, split.font = 1, varlen = -10)

plot(as.party(universal_tree))

# Make predictions
PredictCART = predict(universal_tree, newdata = test, type = "class")
table(test$Personal.Loan , PredictCART)
