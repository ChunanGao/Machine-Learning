
# install packages & library ----------------------------------------------
library(tidyverse)
library(dplyr)
library(MASS)
#install.packages("corrr")
library(corrr)
library(corrplot)
#install.packages("glmnet")
#install.packages("randomForest")
#install.packages("Hmisc")
library("Hmisc")
#install.packages("gbm")



# Clean data --------------------------------------------------------------

#import dataset
covid <- read_csv("~/R/Machine-Learning/data/COVID.csv")
head(covid)
summary(covid)

#check how many samples have missing values
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
covid_clean = covid[,c('HDI','TC','TD','STI','GDPCAP')]

dim(covid_clean)
sum(is.na(covid_clean))
str(covid_clean)
sumstat <- summary(covid_clean)
write.table(sumstat, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)

# correlation matrix (results: all correlations are significant)
cor(covid_clean)

cor_data <- rcorr(as.matrix(covid_clean), type = c("pearson","spearman"))
cor_data
# The p-value will be zero when it's very, very, very, very small, so small that R can't compute it anymore
print(cor_data$P, digits = 20)




# Method1&2: ridge regression and lasso regression ------------------------

# GDPCAP as Dependent Variable 
#ridge regression

library(glmnet)

# pass in an x matrix as well as a y vector

x = model.matrix(GDPCAP~.,covid_clean)
y = covid_clean$GDPCAP


grid =10^seq (10,-2, length =100)

# If alpha=0 then a ridge regression model is fit
#ridge.mod =glmnet (x,y,alpha =0, lambda =grid)


length <- 10
mse_ridge <- rep(NA, length)
mse_lasso <- rep(NA, length)

for (i in 1:10) {
  
  set.seed(i)
  #split training set and testing set
  train  = sample (1: nrow(x), nrow(x)*0.7)
  test = (- train )
  y.test = y[test]
  
  
  # fit a ridge regression model on the training set
  ridge.mod = glmnet(x[train,], y[train], alpha =0, lambda =grid)
  
  #use cross-validation to choose the tuning parameter.
  cv.out_ridge =cv.glmnet (x[train,],y[train],alpha =0)
  plot(cv.out_ridge)
  bestlam_ridge = cv.out_ridge$lambda.min
  bestlam_ridge # 0.09829758
  
  # The test MSE associated with this value of.
  
  ridge.pred=predict(ridge.mod ,s = bestlam_ridge , newx = x[test,])
  mse_ridge[i] = mean((ridge.pred - y.test)^2)
  
  # test MSE = 2.123723
  
  
  #lasso
  
  # fit a lasso regression model on the training set
  lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
  
  #use cross-validation to choose the tuning parameter.
  cv.out_lasso = cv.glmnet(x[train,],y[train],alpha = 1)
  plot(cv.out_lasso)
  bestlam_lasso = cv.out_lasso$lambda.min
  bestlam_lasso # 0.001006106
  
  # The test MSE associated with this value of bestlam.
  
  lasso.pred=predict(lasso.mod, s = bestlam_lasso , newx = x[test,])
  mse_lasso[i] = mean((lasso.pred - y.test)^2) #2.121526
  
}


# we refit our ridge regression model on the full data set, using the value of bestlam chosen by cross-validation, and examine the coefficient estimates.

ridge.out = glmnet(x,y,alpha =0, lambda = bestlam_ridge)
coef(ridge.out)[,1]
ridge.out

lasso.out = glmnet(x, y, alpha = 1, lambda=bestlam_lasso)
coef(lasso.out)[,1]
lasso.coef=coef(lasso.out)[,1]
lasso.coef[lasso.coef!=0]
summary(lasso.out)


mean_mse_ridge = mean(mse_ridge)
mean_mse_lasso = mean(mse_lasso)


# HDI as Dependent Variable 
#ridge

# pass in an x matrix as well as a y vector

x = model.matrix(HDI~.,covid_clean)
y = covid_clean$HDI

grid =10^seq (10,-2, length =100)

length <- 10
mse2_ridge <- rep(NA, length)
mse2_lasso <- rep(NA, length)

for (i in 1:10) {
  
  set.seed(i)
  train2  = sample (1: nrow(x), nrow(x)*0.7)
  test2 = (- train2 )
  y.test2 = y[test2]
  
  
  # fit a ridge regression model on the training set
  ridge.mod2 = glmnet(x[train2,], y[train2], alpha =0, lambda =grid)
  
  #use cross-validation to choose the tuning parameter.
  cv.out2_ridge =cv.glmnet (x[train2,],y[train2],alpha =0)
  plot(cv.out2_ridge)
  bestlam2_ridge = cv.out2_ridge$lambda.min
  bestlam2_ridge #0.009006384
  
  # The test MSE associated with this value of bestlam.
  
  ridge.pred2=predict(ridge.mod2 ,s = bestlam2_ridge , newx = x[test2,])
  mse2_ridge[i] = mean((ridge.pred2 - y.test2)^2)
  
  
  #lasso
  
  # fit a lasso regression model on the training set
  lasso.mod2 = glmnet(x[train2,], y[train2], alpha = 1, lambda = grid)
  
  #use cross-validation to choose the tuning parameter.
  cv.out2_lasso = cv.glmnet(x[train2,],y[train2],alpha = 1)
  plot(cv.out2_lasso)
  bestlam2_lasso = cv.out2_lasso$lambda.min
  bestlam2_lasso #0.0001110349
  
  # The test MSE associated with this value of bestlam.
  
  lasso.pred2=predict(lasso.mod2, s = bestlam2_lasso , newx = x[test2,])
  mse2_lasso[i] = mean((lasso.pred2 - y.test2)^2)
  
}

mean_mse2_ridge = mean(mse2_ridge)
mean_mse2_lasso = mean(mse2_lasso)



# we refit our lasso regression model on the full data set, using the value of bestlam chosen by cross-validation, and examine the coefficient estimates.

ridge2.out = glmnet(x,y,alpha =0, lambda = bestlam2_ridge)
coef(ridge2.out)[,1]
ridge2.out

lasso2.out = glmnet(x, y, alpha = 1, lambda=bestlam2_lasso)
coef(lasso2.out)[,1]
lasso2.coef=coef(lasso2.out)[,1]
lasso2.coef[lasso.coef!=0]
summary(lasso2.out)



# Method3&4: regression tree: bagging and boosting ------------------------

# regression tree, bagging
library (randomForest)

# GDPCAP as dependent variable
length <- 10
mse_bag <- rep(NA, length)
for (i in 1:10) {
#split data into training set and testing set
bag_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
bag.train = covid_clean[bag_train, ]
bag.test = covid_clean[-bag_train,]

bag.GDP = randomForest(GDPCAP~.,data = bag.train, mtry=4, importance =TRUE)
bag.GDP
importance(bag.GDP)

yhat.bag = predict(bag.GDP,newdata = bag.test)
mse_bag[i] = mean((yhat.bag - bag.test$GDPCAP)^2)
}


mean_mse_bag= mean(mse_bag)


# HDI as dependent variable
length <- 10
mse_bag2 <- rep(NA, length)

for (i in 1:10) {
  #split data into training set and testing set
  bag2_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
  bag2.train = covid_clean[bag2_train, ]
  bag2.test = covid_clean[-bag2_train,]
  
  bag.HDI = randomForest(HDI~.,data=bag2.train, mtry=4, importance =TRUE)
  bag.HDI
  importance(bag.HDI)
  
  yhat.bag2 = predict(bag.HDI, newdata =bag2.test)
  mse_bag2[i] = mean((yhat.bag2 - bag2.test$HDI)^2)
}

mean_mse_bag2 = mean(mse_bag2)


# regression tree, Boosting 
library (gbm)

# run a basic GBM model, GDPCAP as response
set.seed(123)  
BoostGDP <- gbm(formula = GDPCAP ~ ., data = boosting.train, distribution = "gaussian", n.trees = 5000,
  shrinkage = 0.1, interaction.depth = 4, n.minobsinnode = 10, cv.folds = 10)

# find index for number trees with minimum CV error
best <- which.min(BoostGDP$cv.error) #best=4998

# get MSE and compute RMSE
sqrt(BoostGDP$cv.error[best])
# plot error curve
gbm.perf(BoostGDP, method = "cv")



# run a basic GBM model, HDI as response
set.seed(123)  
BoostHDI <- gbm(formula = HDI ~ ., data = boosting.train, distribution = "gaussian", n.trees = 5000,
  shrinkage = 0.1, interaction.depth = 4, n.minobsinnode = 10, cv.folds = 10 )

# find index for number trees with minimum CV error
best <- which.min(BoostHDI$cv.error) #best=4987

# get MSE and compute RMSE
sqrt(BoostHDI$cv.error[best])
# plot error curve
gbm.perf(BoostHDI, method = "cv")



## GDPCAP as response

length <- 10
mse_boosting <- rep(NA, length)


for (i in 1:10) {
  
  boosting_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
  boosting.train = covid_clean[boosting_train, ]
  boosting.test = covid_clean[-boosting_train,]
  
  set.seed (i)
  boost.GDP = gbm(GDPCAP~.,data= boosting.train, distribution= "gaussian", n.trees =5000 , interaction.depth = 4)

  yhat.boosting = predict(boost.GDP,newdata = boosting.test)

  mse_boosting[i] = mean((yhat.boosting - boosting.test$GDPCAP)^2)
}

mean_mse_boosting = mean(mse_boosting)
summary(boost.GDP)
par(mfrow =c(1,2))
plot(boost.GDP ,i="HDI")
plot(boost.GDP ,i="STI")




## HDI as response
length <- 10
mse2_boosting <- rep(NA, length)


for (i in 1:10) {
  set.seed (i)
  
  boosting2_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
  boosting2.train = covid_clean[boosting2_train, ]
  boosting2.test = covid_clean[-boosting2_train,]
  
  boost.HDI = gbm(HDI~.,data= boosting2.train, distribution= "gaussian", n.trees =5000 , interaction.depth = 4)
  
  yhat.boosting2 = predict(boost.HDI,newdata = boosting2.test)
  
  mse2_boosting[i] = mean((yhat.boosting2 - boosting2.test$HDI)^2)
}

mean_mse2_boosting = mean(mse2_boosting)
summary(boost.HDI)
par(mfrow =c(1,2))
plot(boost.HDI ,i="TD")
plot(boost.HDI ,i="GDPCAP")


