library(tidyverse)
library(dplyr)
#install.packages("remotes")
library(remotes)
library(MASS)
#install.packages("corrr")
library(corrr)
library(corrplot)
#install.packages("glmnet")
#install.packages("randomForest")

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

covid_clean = na.omit(covid)

covid_clean <-  covid_clean %>%
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
#ridge.mod =glmnet (x,y,alpha =0, lambda =grid)

#split training set and testing set; 


for (i in 1:5) {
  
  set.seed(i)
  train  = sample (1: nrow(x), nrow(x)*0.7)
  test = (- train )
  y.test = y[test]
  
  
  # fit a ridge regression model on the training set
  ridge.mod = glmnet(x[train,], y[train], alpha =0, lambda =grid)
  
  #use cross-validation to choose the tuning parameter ??.
  cv.out_ridge =cv.glmnet (x[train,],y[train],alpha =0)
  plot(cv.out_ridge)
  bestlam_ridge = cv.out_ridge$lambda.min
  bestlam_ridge # 0.09829758
  
  # The test MSE associated with this value of ??.
  
  ridge.pred=predict(ridge.mod ,s = bestlam_ridge , newx = x[test,])
  mse_ridge[i] = mean((ridge.pred - y.test)^2)
  
  # test MSE = 2.123723
  
  
  #lasso
  
  # fit a lasso regression model on the training set
  lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
  
  #use cross-validation to choose the tuning parameter ??.
  cv.out_lasso = cv.glmnet(x[train,],y[train],alpha = 1)
  plot(cv.out_lasso)
  bestlam_lasso =cv.out_lasso$lambda.min
  bestlam_lasso # 0.001006106
  
  # The test MSE associated with this value of ??.
  
  lasso.pred=predict(lasso.mod, s = bestlam_lasso , newx = x[test,])
  mse_lasso[i] = mean((lasso.pred -y.test)^2) #2.121526
  
}


# we refit our ridge regression model on the full data set, using the value of ?? chosen by cross-validation, and examine the coefficient estimates.

ridge.out = glmnet(x,y,alpha =0, lambda = bestlam_ridge)
coef(ridge.out)[,1]
ridge.out

lasso.out = glmnet(x, y, alpha = 1, lambda=bestlam_lasso)
coef(lasso.out)[,1]
lasso.coef=coef(lasso.out)[,1]
lasso.coef[lasso.coef!=0]
summary(lasso.out)

mse_ridge
mse_lasso






# regression tree
library (randomForest)

#split data into training set and testing set
tree_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
tree.train=covid_clean[tree_train, ]
tree.test=covid_clean[-tree_train,]

bag.covid = randomForest(GDPCAP~.,data=tree.train, mtry=4, importance =TRUE)
bag.covid
importance(bag.covid)

yhat.bag = predict(bag.covid,newdata = tree.test)
mean((yhat.bag - tree.test$GDPCAP)^2)