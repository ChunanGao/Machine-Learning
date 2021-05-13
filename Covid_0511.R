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
#install.packages("Hmisc")
library("Hmisc")

#install.packages("gbm")

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

#ridge regression

library(glmnet)


# pass in an x matrix as well as a y vector

x = model.matrix(GDPCAP~.,covid_clean)
y = covid_clean$GDPCAP


grid =10^seq (10,-2, length =100)

# If alpha=0 then a ridge regression model is fit
#ridge.mod =glmnet (x,y,alpha =0, lambda =grid)

#split training set and testing set; 


length <- 10
mse_ridge <- rep(NA, length)
mse_lasso <- rep(NA, length)

for (i in 1:10) {
  
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
  bestlam_lasso = cv.out_lasso$lambda.min
  bestlam_lasso # 0.001006106
  
  # The test MSE associated with this value of ??.
  
  lasso.pred=predict(lasso.mod, s = bestlam_lasso , newx = x[test,])
  mse_lasso[i] = mean((lasso.pred - y.test)^2) #2.121526
  
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
?????????????????????refit


mean_mse_ridge = mean(mse_ridge)
mean_mse_lasso = mean(mse_lasso)


# HDI as Dependent Variable -----------------------------------------------

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
  bestlam2_ridge 
  
  # The test MSE associated with this value of ??.
  
  ridge.pred2=predict(ridge.mod2 ,s = bestlam2_ridge , newx = x[test2,])
  mse2_ridge[i] = mean((ridge.pred2 - y.test2)^2)
  
  
  #lasso
  
  # fit a lasso regression model on the training set
  lasso.mod2 = glmnet(x[train2,], y[train2], alpha = 1, lambda = grid)
  
  #use cross-validation to choose the tuning parameter.
  cv.out2_lasso = cv.glmnet(x[train2,],y[train2],alpha = 1)
  plot(cv.out2_lasso)
  bestlam2_lasso = cv.out2_lasso$lambda.min
  bestlam2_lasso 
  
  # The test MSE associated with this value of ??.
  
  lasso.pred2=predict(lasso.mod2, s = bestlam2_lasso , newx = x[test2,])
  mse2_lasso[i] = mean((lasso.pred2 - y.test2)^2)
  
}

mean_mse2_ridge = mean(mse2_ridge)
mean_mse2_lasso = mean(mse2_lasso)



# we refit our ridge regression model on the full data set, using the value of ?? chosen by cross-validation, and examine the coefficient estimates.

ridge2.out = glmnet(x,y,alpha =0, lambda = bestlam2_ridge)
coef(ridge2.out)[,1]
ridge2.out

lasso2.out = glmnet(x, y, alpha = 1, lambda=bestlam2_lasso)
coef(lasso2.out)[,1]
lasso2.coef=coef(lasso2.out)[,1]
lasso2.coef[lasso.coef!=0]
summary(lasso2.out)
?????????????????????refit



SSE <- sum((predicted - true)^2)
SST <- sum((true - mean(true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(df))




# Regression tree ---------------------------------------------------------
# regression tree
library (randomForest)

length <- 10
mse_tree <- rep(NA, length)
for (i in 1:2) {
#split data into training set and testing set
tree_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
tree.train = covid_clean[tree_train, ]
tree.test = covid_clean[-tree_train,]

bag.covid = randomForest(GDPCAP~.,data=tree.train, mtry=4, importance =TRUE)
bag.covid
importance(bag.covid)

yhat.bag = predict(bag.covid,newdata = tree.test)
mse_tree[i] = mean((yhat.bag - tree.test$GDPCAP)^2)
}

mse_tree


length <- 10
mse_tree2 <- rep(NA, length)
for (i in 1:2) {
  #split data into training set and testing set
  tree2_train = sample(dim(covid_clean)[1],dim(covid_clean)[1]*0.7)
  tree2.train = covid_clean[tree2_train, ]
  tree2.test = covid_clean[-tree2_train,]
  
  bag.HDI = randomForest(HDI~.,data=tree2.train, mtry=4, importance =TRUE)
  bag.HDI
  importance(bag.HDI)
  
  yhat.bag2 = predict(bag.HDI,newdata = tree2.test)
  mse_tree2[i] = mean((yhat.bag2 - tree2.test$HDI)^2)
}

mse_tree2



library (gbm)
set.seed (1)
boost.GDP = gbm(GDPCAP???.,data= tree2.train, distribution= "gaussian", n.trees =5000 , interaction.depth = 4)
summary(boost.GDP)
par(mfrow =c(1,2))
plot(boost.GDP ,i="HDI")
plot(boost.GDP ,i="STI")

