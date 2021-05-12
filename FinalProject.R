

library(tidyverse)
library(dplyr)
#install.packages("remotes")
library(remotes)
library(MASS)
#install.packages("corrr")
library(corrr)
library(corrplot)
#install.packages("glmnet")

#import dataset
covid <- read_csv("~/R/Machine-Learning/data/COVID.csv")
head(covid)

summary(covid)

#check how many samples have missing salary
sum(is.na(covid))
sum(is.na(covid$GDPCAP)) 
sum(is.na(covid$POP)) 
sum(is.na(covid$TC))
sum(is.na(covid$TD))
sum(is.na(covid$STI))
sum(is.na(covid$HDI)) 

#HDI feature has 6202 missing values
#Thus, we create a data set with fully completely observed samples

covid = na.omit(covid)
covid_clean <- covid %>%
  select(HDI,TC,TD,STI,GDPCAP)

dim(covid_clean)
sum(is.na(covid_clean))
str(covid_clean)

# correlation matrix
cor(covid_clean)

#ridge regression

library(glmnet)


# pass in an x matrix as well as a y vector

x = model.matrix(GDPCAP~.,covid_clean)
y = covid_clean$GDPCAP


grid =10^seq (10,-2, length =100)

# If alpha=0 then a ridge regression model is fit
#ridge.mod =glmnet (x,y,alpha =0, lambda = grid)

#split training set and testing set; 

set.seed (1)
train  = sample (1: nrow(x), nrow(x)*0.7)
test = (- train )
y.test = y[test]


# fit a ridge regression model on the training set
ridge.mod = glmnet(x[train,], y[train], alpha =0, lambda =grid)

#use cross-validation to choose the tuning parameter ??.
cv.out =cv.glmnet (x[train,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam # 0.09829758

# The test MSE associated with this value of ??.

ridge.pred=predict(ridge.mod ,s = bestlam , newx = x[test,])
mean((ridge.pred -y.test)^2)

# test MSE = 2.123723

# we refit our ridge regression model on the full data set, using the value of ?? chosen by cross-validation, and examine the coefficient estimates.

out = glmnet(x,y,alpha =0, lambda=bestlam)
coef(out)[,1]


#lasso

# fit a lasso regression model on the training set
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)

#use cross-validation to choose the tuning parameter ??.
cv.out_lasso =cv.glmnet(x[train,],y[train],alpha = 1)
plot(cv.out_lasso)
bestlam_lasso =cv.out$lambda.min
bestlam_lasso # 0.09829758

# The test MSE associated with this value of ??.

lasso.pred=predict(lasso.mod ,s = bestlam_lasso , newx = x[test,])
mean((lasso.pred -y.test)^2) #2.118945

out_lasso = glmnet(x,y,alpha = 1, lambda=bestlam_lasso)
coef(out_lasso)[,1]

lasso.coef=coef(out_lasso)[,1]
lasso.coef[lasso.coef!=0]
