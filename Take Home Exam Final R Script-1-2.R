# Chapter 2, Question 10:
# Part A:
library(MASS)
Boston = Boston # creating variable for dataset
?Boston # information on dataset
# 506 rows which represent the 506 suburbs taken into account in this study, 14 columns represent the different attributes that can impact housing values.

# Part B:
pairs(Boston) 
# Higher tax for homes closer to rad, negative correlation between dis and nox, higher ptratio indicates higher medv, high crim indicates a high lstat, high rm indicates a higher medv, negative correlation between black and dis, no strong correlations with zn

# Part C:
cor(Boston) # correlation of variables
# strong correlation between crim and rad, crim and tax, and crim and lstat

# Part D:
par(mfrow = c(1,3)) # dividing plot screen into 3 for multiple plots
hist(Boston$crim[Boston$crim > 1], breaks = 20, main = "Crime Rate by Suburb", xlab = "Suburbs per Crime Level") # Largely safe areas, tail-end of histogram shows roughly 20 suburbs with crime rates > 20
hist(Boston$tax, breaks = 25, main = "Tax Rates by Suburb", xlab = "Suburbs per Tax Rate") # Large separation between areas with lowest tax rate and areas with highest tax rate
hist(Boston$ptratio, breaks = 25, main = "{Pupil-Teacher Ratio by Suburb", xlab = "Suburbs per Pupil-Teacher Ratio") # Slightly skewed towards a high ratio

# Part E:
sum(Boston$chas == 1) # 35

# Part F:
median(Boston$ptratio) # 19.05

# Part G:
t(subset(Boston, medv == min(medv))) # 406 and 309: high crim, indus, nox, age, rad, tax, ptratio, black, and lstat; low zn, chas, rm, dis, and medv

# Part H:
sum(Boston$rm > 7) # 64
sum(Boston$rm > 8) # 13
summary(subset(Boston, rm > 8)) # Low crim, ptratio, indus; high medv, age


# Chapter 3, Question 15:
library(MASS)
attach(Boston)

# Part A:
zn.fit = lm(crim ~ zn) # creating a fitting simple linear regression model for each predictor
summary(zn.fit)
plot(zn.fit)
# p-value is < 0.05 so there is statistically significant association between crim and zn
# this means that changes in zn are related to changes in crim

indus.fit = lm(crim ~ indus)
summary(indus.fit)
plot(indus.fit)
# p-value is < 0.05 so there is statistically significant association between indus and zn
# this means that changes in indus are related to changes in crim


chas = as.factor(chas) # converting 1 and 0 to factors
chas.fit = lm(crim ~ chas)
summary(chas.fit)
plot(chas.fit)
# p-value is > 0.05 so there is not a statistically significant association between crim and chas
# this means that changes in chas are likely not related to changes in crim


nox.fit = lm(crim ~ nox)
summary(nox.fit)
plot(nox.fit)
# p-value is < 0.05 so there is statistically significant association between crim and nox
# this means that changes in nox are related to changes in crim

rm.fit = lm(crim ~ rm)
summary(rm.fit)
plot(rm.fit)
# p-value is < 0.05 so there is statistically significant association between crim and rm
# this means that changes in rm are related to changes in crim

age.fit = lm(crim ~ age)
summary(age.fit)
plot(age.fit)
# p-value is < 0.05 so there is statistically significant association between crim and age
# this means that changes in age are related to changes in crim

dis.fit = lm(crim ~ dis)
summary(dis.fit)
plot(dis.fit)
# p-value is < 0.05 so there is statistically significant association between crim and dis
# this means that changes in dis are related to changes in crim

rad.fit = lm(crim ~ rad)
summary(rad.fit)
plot(rad.fit)
# p-value is < 0.05 so there is statistically significant association between crim and rad
# this means that changes in rad are related to changes in crim

tax.fit = lm(crim ~ tax)
summary(tax.fit)
plot(tax.fit)
# p-value is < 0.05 so there is statistically significant association between crim and tax
# this means that changes in tax are related to changes in crim

ptratio.fit = lm(crim ~ ptratio)
summary(ptratio.fit)
plot(ptratio.fit)
# p-value is < 0.05 so there is statistically significant association between crim and ptratio
# this means that changes in ptratio are related to changes in crim

black.fit = lm(crim ~ black)
summary(black.fit)
plot(black.fit)
# p-value is < 0.05 so there is statistically significant association between crim and black
# this means that changes in black are related to changes in crim

lstat.fit = lm(crim ~ lstat)
summary(lstat.fit)
plot(lstat.fit)
# p-value is < 0.05 so there is statistically significant association between crim and lstat
# this means that changes in lstat are related to changes in crim

medv.fit = lm(crim ~ medv)
summary(medv.fit)
plot(medv.fit)
# p-value is < 0.05 so there is statistically significant association between crim and medv
# this means that changes in medv are related to changes in crim

# Chas has a p-value greater than 0.05, so it cannot be determined to have a statistically significant association with crim. All other variables do have a statistically significant association.

# Part B:
lm.fit=lm(crim ~.,data=Boston) 
summary(lm.fit)

# Reject H0 for zn (p<.1), dis (p<.001), rad (p<.001), black (p<.1), medv (p<.01)

# Part C:
uni.reg = vector('numeric', 0) # create numeric vector with a length of 0
uni.reg = c(uni.reg, zn.fit$coefficients[2]) # adds each variable's coefficient from uni.reg into the empty vector 
uni.reg = c(uni.reg, indus.fit$coefficients[2])
uni.reg = c(uni.reg, chas.fit$coefficients[2])
uni.reg = c(uni.reg, nox.fit$coefficients[2])
uni.reg = c(uni.reg, rm.fit$coefficients[2])
uni.reg = c(uni.reg, age.fit$coefficients[2])
uni.reg = c(uni.reg, dis.fit$coefficients[2])
uni.reg = c(uni.reg, rad.fit$coefficients[2])
uni.reg = c(uni.reg, tax.fit$coefficients[2])
uni.reg = c(uni.reg, ptratio.fit$coefficients[2])
uni.reg = c(uni.reg, black.fit$coefficients[2])
uni.reg = c(uni.reg, lstat.fit$coefficients[2])
uni.reg = c(uni.reg, medv.fit$coefficients[2])
multiple.reg = vector('numeric', 0) # create another vector with a length of 0
multiple.reg = c(multiple.reg, lm.fit$coefficients) # populate vector with the multiple regression coefficients
multiple.reg = multiple.reg[-1] 
plot(uni.reg, multiple.reg, col = 'green')

# The difference between univariate and multiple regression comes from univariate regression's slope showing how an increase in a predictor can impact the model. Multiple regression shows the impact of an increase of a predictor, all things constant, which is quite different from univariate. This means that the multiple regression model would show no connection between response and predictors, which is not the case in univariate.

# Part D:

zn.fitd = lm(crim ~ poly(zn, 3)) # followed formatting from lab in chapter 3 for poly(), third-order fit to match equation in the problem
summary(zn.fitd)

indus.fitd = lm(crim ~ poly(indus, 3))
summary(indus.fitd)

nox.fitd = lm(crim ~ poly(nox, 3))
summary(nox.fitd)

rm.fitd = lm(crim ~ poly(rm, 3))
summary(rm.fitd)

age.fitd = lm(crim ~ poly(age, 3))
summary(age.fitd)

dis.fitd = lm(crim ~ poly(dis, 3))
summary(dis.fitd)

rad.fitd = lm(rad ~ poly(rad, 3))
summary(rad.fitd)

tax.fitd = lm(crim ~ poly(tax, 3))
summary(tax.fitd)

ptratio.fitd = lm(crim ~ poly(ptratio, 3))
summary(ptratio.fitd)

black.fitd = lm(crim ~ poly(black, 3))
summary(black.fitd)

lstat.fitd = lm(crim ~ poly(lstat, 3))
summary(lstat.fitd)

medv.fitd = lm(crim ~ poly(medv, 3))
summary(medv.fitd)

# zn, rm, rad, tax, and lstat predictors' cubic coefficients are not statistically significant as shown by their p-values
# indus, nox, age, dis, ptratio, and medv predictors' cubic coefficients are startically significant as shown by their p-values


# Chapter 6, Question 9:
library(ISLR)
set.seed(2)
college = attach(College)
College[,-1] = apply(College[,-1], 2, scale)

# Part A:
train = sample(1:nrow(College), nrow(College)/2)
train.college = College[train, ]
test.college = College[-train, ]

# Part B:
fit = lm(Apps~., data=test.college)
pred = predict(fit, test.college)
mean((test.college[, 'Apps']-pred)^2)
# Test error obtained is 0.0612919

# Part C:
library(glmnet)
test.matrix = model.matrix(Apps~.-1, data=test.college)
train.matrix = model.matrix(Apps~.-1,data=train.college)
table = 10^seq(4,-2,length=100)
ridge = cv.glmnet(train.matrix, train.college[, 'Apps'], alpha=0,lambda=table,thresh=1e-12)
best = ridge$lambda.min
best
# 0.01
predict.ridge = predict(ridge,newx=test.matrix,s=best)
mean((test.college[,'Apps']-predict.ridge)^2)
# 0.07042784

# Part D:
lasso = cv.glmnet(train.matrix,train.college[,'Apps'],alpha=1,lambda=table,thresh=1e-12)
best = lasso$lambda.min
best
# 0.01
predict.lasso = predict(lasso,newx=test.matrix,s=best)
mean((test.college[,'Apps']-predict.lasso)^2)
# 0.06997521
lasso = glmnet(model.matrix(Apps~.-1,data=College),College[,'Apps'],alpha=1)
predict(lasso,s=best,type='coefficients')
# There are 11 non-zero coefficients

# Part E:
library(pls)
set.seed(2)
fit.pcr = pcr(Apps~.,data=College,scale=TRUE,validation='CV')
validationplot(fit.pcr,val.type ='MSEP')
set.seed(1)
fit.pcr = pcr(Apps~.,data=College,scale=TRUE,validation='CV')
validationplot(fit.pcr,val.type ='MSEP')
predict.pcr=predict(fit.pcr,test.college,ncomp=16)
mean((test.college[,'Apps']-c(predict.pcr))^2)
# 0.06348735

# Part F:
set.seed(1)
fit.pls = plsr(Apps~.,data=train.college,scale=TRUE,validation='CV')
validationplot(fit.pls,val.type='MSEP')
predict.pls = predict(fit.pls,test.college,ncomp=6)
mean((test.college$Apps-predict.pls)^2)
# .07142963

# Part G:
average = mean(test.college[,'Apps'])
r2.test = 1-mean((test.college[,'Apps']-pred)^2)/mean((test.college[,'Apps']-average)^2)
r2.ridge = 1-mean((test.college[,'Apps']-predict.ridge)^2)/mean((test.college[,'Apps']-average)^2)
r2.lasso = 1-mean((test.college[,'Apps']-predict.lasso)^2)/mean((test.college[,'Apps']-average)^2)
r2.plot = barplot(c(r2.test,r2.ridge,r2.lasso),col='blue',names.arg=c('OLS','Ridge','Lasso'),main='R-Squared')
# All of the models have high R^2 and are relatively similar, meaning they are likely not too different. 


# Chapter 6, Question 11
# Part A
library(MASS)
attach(Boston)
dim(Boston)
str(Boston)
summary(Boston)
set.seed(1)
subset = sample(nrow(Boston),nrow(Boston)/2)
train = Boston[subset,]
test = Boston[-subset,]

# Subset Selection:
library(leaps)
best_subset = regsubsets(medv~.,data=train,nbest=1,nvmax=13)
summary(best_subset)
boston.matrix = model.matrix(medv~.,data=test,nvmax=13)
errors = rep(NA, 13)
for (i in 1:13) {
  coefi = coef(best_subset, id = i)
  pred = boston.matrix[, names(coefi)] %*% coefi
  errors[i] = mean((pred - test$medv)^2)
}
plot(errors, xlab = "# predictors", ylab = "MSE", pch = 19, type = "b",col="blue")
which.min(errors) # 12
coef(best_subset,which.min(errors))
best_subset_mse = errors[12]
best_subset_mse # 26.84946
# To determine the optimal model, test set MSE is determined for each. The model with the lowest MSE is selected, which in this case is the predictor value of 11

# Ridge:
train.matrix = model.matrix(medv~.,data=train)
test.matrix = model.matrix(medv~.,data=test)
table = 10^seq(4,-2,length=100)
library(glmnet)
ridge = glmnet(train.matrix,train$medv,alpha=0,lambda=table)
cv.ridge = cv.glmnet(train.matrix,train$medv,alpha=0,lambda=table)
plot(cv.ridge)
best.ridge = cv.ridge$lambda.min
best.ridge # 0.05336699
ridge.model = glmnet(train.matrix,train$medv,alpha=0,lambda=best.ridge)
coef(ridge.model)
newridge = predict(ridge,s=best.ridge,newx=test.matrix)
MSE.ridge = mean((test$medv-newridge)^2)
MSE.ridge # 26.79522
# Ridge regression with minimum MSE is what the final model and prediction is built on. The lambda that had the lowest MSE is 0.05336699

# Lasso:
lasso = glmnet(train.matrix,train$medv,alpha=0,lambda=table)
cv.lasso = cv.glmnet(train.matrix,train$medv,alpha=0,lambda=table)
plot(cv.lasso)
best.lasso = cv.lasso$lambda.min
best.lasso # 0.08111308
lasso.model = glmnet(train.matrix,train$medv,alpha=0,lambda=best.lasso)
coef(lasso.model)
newlasso = predict(lasso,s=best.lasso,newx=test.matrix)
MSE.lasso = mean((test$medv-newlasso)^2)
MSE.lasso # 26.77717
# Lasso regression with minimum MSE is what the final model and prediction is built on. The lambda that had the lowest MSE is 0.08111308


# PCR:
library(pls)
pcr = pcr(medv~.,data=train,scale=TRUE,validation="CV")
summary(pcr)
validationplot(pcr,val.type="MSEP")
pcr.model = pcr(medv~.,data=train,scale=TRUE,ncomp=5)
summary(pcr.model)
pcr.model$coefficients
predict.pcr = predict(pcr,test,ncomp=5)
MSE.pcr = mean((test$medv-predict.pcr)^2)
MSE.pcr # 30.28912
# 5 as number of components since 5 components account for 80% of the variation


# Part B:
# The MSEs I obtained in these models were:
# Subset - 26.84946
# Ridge - 26.79522
# Lasso - 26.77717
# PCR - 30.28912
# Therefore, I would recommend using the lasso model as it has the lowest MSE of all the models


# Part C:
# The lasso models has all predictor variables
# Therefore, all of the predictor variables seem to play a role in the prediction of the response variable


# Chapter 4, Question 10:
# Part A:
library(ISLR)
cor(Weekly[,-9])
# Little correlation between the Lag variables and returns, somewhat strong correlation between Volume and Year 
pairs(Weekly)
summary(Weekly)
# Up to this point there does not seem to be patterns except for a high correlation between trading volume and years
# Up until 2008 there was a steady growth for the amount of shares traded each week, but this then decreased in 2008
plot(Weekly$Volume, xlab='Week', ylab='Number of Shares Traded', main = 'Shares traded per week')

# Part B:
log = glm(Direction~+ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
summary(log)
# Lag 2 has a 0.0296 p-value, which suggests that at a significance level of 5% there is enough evidence to reject H0
# This means there is no relation between Lag 2 and Direction

# Part C:
glm.probability = predict(log, type='response')
glm.predict = rep('Down', 1089)
glm.predict[glm.probability > 0.5] = 'Up'
table(glm.predict, Weekly$Direction)
mean(glm.predict == Weekly$Direction)
mean(glm.predict != Weekly$Direction)
# Predictions were correct 54/484 down weeks and 557/605 up weeks
# In total the predictions were correct 56.11% of the times
# Down predicitions were accurate only 54/484 weeks, which is relatively low in accuracy
# Whereas Up predictions were accurate 557/605 weeks, which is relatively high in accuracy
# There is a training error rate of 43.9% 

# Part D:
ptd.train = (Weekly$Year < 2009)
fit = glm(Direction~Lag2, data=Weekly, subset=ptd.train, family='binomial')
summary(fit)
glm.probability = predict(fit, Weekly[!ptd.train, ], type='response')
glm.predict = rep("Down", dim(Weekly[!ptd.train, ])[1])
glm.predict[glm.probability > 0.5] = "Up"
table(glm.predict, Weekly[!ptd.train, ]$Direction)
mean(glm.predict == Weekly[!ptd.train, ]$Direction)
mean(Weekly[!ptd.train, ]$Direction == "Up")
# This model accurately predicted 62.5% of the weeks tested

# Part G:
library(class)
train.X=data.frame(Weekly[ptd.train, ]$Lag2)
test.X=data.frame(Weekly[!ptd.train, ]$Lag2)
train.Direction = Weekly[ptd.train, ]$Direction
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Weekly[!ptd.train, ]$Direction)
mean(knn.pred == Weekly[!ptd.train,]$Direction)

# The prediction accuracy using KNN method is 50% 

# Part H:
# Logistic Regression had a mean success rate of 56.1% whereas KNN had a mean success rate of 50%.

# Part I:
log1 = glm(Direction~+ Lag1 + Lag2 + Lag3 + Volume, data=Weekly, family=binomial)
summary(log1)
glm.probability1 = predict(log1, type='response')
glm.predict1 = rep('Down', 1089)
glm.predict[glm.probability > 0.5] = 'Up'
table(glm.predict1, Weekly$Direction)
mean(glm.predict1 == Weekly$Direction)
mean(glm.predict1 != Weekly$Direction)
# Mean success rate of 44.44%, which is lower than the original calculation including all Lags

library(class)
train.X1=data.frame(Weekly[ptd.train, ]$Lag2)
test.X1=data.frame(Weekly[!ptd.train, ]$Lag2)
train.Direction = Weekly[ptd.train, ]$Direction
set.seed(1)
knn.pred=knn(train.X1, test.X1, train.Direction, k=3)
table(knn.pred, Weekly[!ptd.train, ]$Direction)
mean(knn.pred == Weekly[!ptd.train,]$Direction)
# Mean success rate 54.81%

library(class)
train.X1=data.frame(Weekly[ptd.train, ]$Lag2)
test.X1=data.frame(Weekly[!ptd.train, ]$Lag2)
train.Direction = Weekly[ptd.train, ]$Direction
set.seed(1)
knn.pred=knn(train.X1, test.X1, train.Direction, k=5)
table(knn.pred, Weekly[!ptd.train, ]$Direction)
mean(knn.pred == Weekly[!ptd.train,]$Direction)
# Mean success rate 53.85%

library(class)
train.X1=data.frame(Weekly[ptd.train, ]$Lag2)
test.X1=data.frame(Weekly[!ptd.train, ]$Lag2)
train.Direction = Weekly[ptd.train, ]$Direction
set.seed(1)
knn.pred=knn(train.X1, test.X1, train.Direction, k=9)
table(knn.pred, Weekly[!ptd.train, ]$Direction)
mean(knn.pred == Weekly[!ptd.train,]$Direction)
# Mean success rate 55.77%

# Lag 2 had had the highest accuracy among the variables. The logistic regression method had the highest level of accuracy, as did its confusion matrix.


# Chapter 8, Question 8:
library(tree)
library(ISLR)
attach(Carseats)

# Part A:
set.seed(1)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
carseats_train = Carseats[train,]
carseats_test = Carseats[-train,]

# Part B:
tree_carseats = tree(Sales ~.,carseats_train)
plot(tree_carseats)
text(tree_carseats, pretty = 0)
summary(tree_carseats)
# ShelveLoc is the first varibale used to sub-divide the data, followed by price. This makes sense considering the better the location, the higher likelihood of a customer purchasing the item. 
yhat = predict(tree_carseats, newdata = carseats_test)
mean((yhat - carseats_test$Sales)^2) # MSE = 4.922039

# Part C:
set.seed(1)
cv.carseats = cv.tree(tree_carseats)
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev, main = "CV Graph", xlab = "Number of Nodes", ylab = "CV Dev", type="b")
cv.carseats # Optimal level of tree complexity is 18 since lowest dev
prune.carseats=prune.tree(tree_carseats,best = 18)
plot(prune.carseats)
text(prune.carseats,pretty=0)
yhat = predict(prune.carseats, newdata = carseats_test)
mean((yhat - carseats_test$Sales)^2) # MSE = 4.922039 (the exact same as before pruning), so CV indicates that pruning does not improve MSE

# Part D:
library(randomForest)
set.seed(1)
bag.carseats = randomForest(Sales~., data = Carseats, subset=train, mtry=10, importance=TRUE)
bag.carseats
yhat.bag = predict(bag.carseats, newdata = carseats_test)
mean((yhat.bag - carseats_test$Sales)^2) # Bagging improves the test, returning an MSE of 2.605%, which is better than the pruned and original tree
importance(bag.carseats) # Price and ShelveLoc are the most important variables by a high margin

# Part E:
carseats.rf = randomForest(Sales~., data = Carseats, subset = train, mtry = 1, importance = TRUE)
yhat.rf = predict(carseats.rf, newdata = carseats_test)
mean((yhat.rf-carseats_test$Sales)^2) # MSE under random forest increased to 4.962%

carseats.rf = randomForest(Sales~., data = Carseats, subset = train, mtry = 2, importance = TRUE)
yhat.rf = predict(carseats.rf, newdata = carseats_test)
mean((yhat.rf-carseats_test$Sales)^2) # MSE under random forest increased to 3.503%

set.seed(1)
carseats.rf = randomForest(Sales~., data = Carseats, subset = train, mtry = 3, importance = TRUE)
yhat.rf = predict(carseats.rf, newdata = carseats_test)
mean((yhat.rf-carseats_test$Sales)^2) # MSE under random forest increased to 2.961%
importance(carseats.rf) # Price and ShelveLoc are still the most important variables, but both decreased in importance significantly under RF


# Chapter 8, Question 11:
# Part A:
library(tree)
library(gbm)
library(ISLR)
attach(Caravan)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == 'Yes', 1, 0)
train.caravan = Caravan[train,]
test.caravan = Caravan[-train,]

# Part B:
set.seed(1)
caravan.boost = gbm(Purchase~.,train.caravan, distribution = 'gaussian', n.trees = 1000, shrinkage = 0.01)
summary(caravan.boost)
prob.test = predict(caravan.boost, test.caravan, n.trees = 1000, type = 'response')
pred.test = ifelse(prob.test > 0.2, 1, 0)
table(test.caravan$Purchase, pred.test) 
11/51 # 21.57% of customers predicted to make the purchase actually purchased
# Most important variables are PPERSAUT and MKOOPKLA

# Part C using logistic regression:
probability.test = predict(caravan.boost, test.caravan, n.trees = 1000, type = "response")
predict.test = ifelse(probability.test > 0.2, 1, 0)
table(test.caravan$Purchase, predict.test)
11/51 # 21.57% of people actually make a purchase

caravan.log = glm(Purchase ~., data = train.caravan, family = "binomial")
probability.test.c = predict(caravan.log, test.caravan, type = "response")
predict.test.c = ifelse(probability.test > 0.2, 1, 0)
table(test.caravan$Purchase, predict.test.c)
11/51 # 21.57% of people actually make a purchase


# Problem 1:
# Part A:
library(tree)
library(randomForest)

beauty=read.csv('BeautyData.csv')
Sample = sample(1:nrow(beauty), nrow(beauty)/2)
test.beauty = beauty[-Sample,]
train.beauty = beauty[Sample,]

#Tree
tree.beauty = tree(CourseEvals~., data = train.beauty)
plot(tree.beauty)
text(tree.beauty, pretty=0)
summary(tree.beauty)

#MSE
yhat.beauty = predict(tree.beauty, newdata = test.beauty)
mean((yhat.beauty - test.beauty$CourseEvals)^2)
#MSE = 0.2364031

#CV
cv.beauty=cv.tree(tree.beauty)
plot(cv.beauty$size,cv.beauty$dev, main = "Cross-Validation",xlab="Nodes", ylab="Cross-Validation Error", type="b",)
cv.beauty
#prune to 5
prune.beauty = prune.tree(tree.beauty, best = 3)
yhat.beauty2 = predict(prune.beauty, newdata = test.beauty)
mean((yhat.beauty2-test.beauty$CourseEvals)^2)
plot(prune.beauty)
text(prune.beauty, pretty=0)
#MSE = .226428

#Bagging
bag.beauty = randomForest(CourseEvals~.,data=train.beauty,mtry=5, importance=TRUE)
yhat.bag = predict(bag.beauty, newdata=test.beauty)
mean((yhat.bag - test.beauty$CourseEvals)^2)
plot(bag.beauty)
#MSE = .2249114

importance(bag.beauty)
#BeautyScore is highly important, followed by Lower and Female

#Predictions w/o BeautyScore

library(tree)
library(randomForest)

#Tree
tree.beauty1 = tree(CourseEvals~.-BeautyScore, data = train.beauty)
plot(tree.beauty1)
text(tree.beauty1, pretty=0)
summary(tree.beauty1)

#MSE
yhat.beauty = predict(tree.beauty1, newdata = test.beauty)
mean((yhat.beauty - test.beauty$CourseEvals)^2)
##MSE = .2398354

#CV
cv.beauty1=cv.tree(tree.beauty1)
plot(cv.beauty1$size,cv.beauty1$dev, main = "Cross-Validation",xlab="Nodes", ylab="Cross-Validation Error", type="b",)
cv.beauty1
#prune to 5
prune.beauty1 = prune.tree(tree.beauty1, best = 5)
yhat.beauty3 = predict(prune.beauty1, newdata = test.beauty)
mean((yhat.beauty3-test.beauty$CourseEvals)^2)
plot(prune.beauty1)
text(prune.beauty1, pretty=0)
#MSE = .2384446

#Bagging
bag.beauty1 = randomForest(CourseEvals~.-BeautyScore,data=train.beauty,mtry=4, importance=TRUE)
yhat.bag1 = predict(bag.beauty1, newdata=test.beauty)
mean((yhat.bag1 - test.beauty$CourseEvals)^2)
#MSE = .2373452

importance(bag.beauty1)
# lower is significantly more important, large increase in importance for female as well



# I removed the BeautyScore to analyze if and how it reduces the accuracy of my model, considering it was the most important variable, followed by female and lower. 
# I observed this decreased the accruacy by 47.61045%. The MSE without BeautyScore grew when compared with that which included the BeautyScore.
# After removing BeautyScore, it became evident that the other variables still had impacts on the model at varying degrees.
# For example, with the inclusion of BeautyScore, tenuretrack and nonenglish were not even included in the tree, but after its removal, both variables appeared as end nodes.


#Part B:

# Considering human nature promotes biases, it would be impossible to fully differentiate between discrimination and productivity in the current method of data gathering for this model.
# It would be ideal to fully understand the impacts of beauty, and in fact all other variables measured, by creating a control group. This can be done by fully hiding the identity of the professor.
# Further, beauty is not quanitifiable, and each individual has a different method of measuring attractiveness, making this a rather subjective variable.
# Finally, the model relied more on other variables after removing beauty. This further entanglement of variables shows that it is likely that even without beauty, the other factors are still impacted by personal biases of participants. 


# Problem 2:
city = read.csv('MidCity.csv')
city$Nbhd = factor(city$Nbhd)
test = city[,-1]
test$Price = scale(test$Price)
test$Bedrooms = scale(test$Bedrooms)
test$Bathrooms = scale(test$Bathrooms)
test$Offers = scale(test$Offers)
test$SqFt = scale(test$SqFt)
fit = lm(Price ~ Nbhd + Offers + SqFt + Brick + Bathrooms + Nbhd:Brick, data = test)
summary(fit)
confint(fit)

test$predict = predict(fit, test)
error = sqrt(mean((test$pred - test$Price)^2)/(dim(test)[1]-10))
error # standard error is 0.03362

# Question 1: 
# There is a premium for brick houses everything being equal
# Positive .482 slope for BrickYes versus the negative .456 slope for Intercept, positive net
# So, if a house is made of bricks, there will be an increase in price by .482 standard deviations

# Question 2:
# There is a premium for houses in neighborhood 3
# Positive .801 slope for Nbhd3 versus the negative .456 slope for Intercept, positive net
# So, if a house is in Nbhd3, there will be an increase in price by .801 standard deviations

# Question 3:
# There is no premium for brick houses in neighborhood 3
# Positive .361 slope for Nbhd3:BrickYes versus the negative .456 slope for Intercept, negative net
# Confidence interval includes a zero, so further justifies that there is no premium associated with brick houses in neighborhood 3

# Question 4:
test[which(test$Nbhd == 3), 'Nb_new'] = 'new'
test[which(test$Nbhd%in%c(1,2)),'Nb_new'] = 'old'
test2 = test[,c(2:7,9)]
fit2 = lm(Price ~ Nb_new + Offers + SqFt + Brick + Bathrooms + Nb_new:Brick, data = test2)
summary(fit2)
confint(fit2)

test2$predict2 = predict(fit2, test2)
error2 = sqrt(mean((test2$predict2 - test2$Price)^2)/(dim(test2)[1]-10))
error2 # standard error is 0.03363
# Yes can combine neighborhood 1 and neighborhood 2 into one old neighborhood


# Problem 3:
# Question 1: Each city has different cause and effect for crime rates that cannot be compared against each other
# Simply taking crime and police data does not allow the study of if high police levels creates high crime levels and vice versa
# Looking at DC for example, crime was impacted by higher police presence due to increased terrorist threats leading to increased police presence
# However, on the same day the police presence in Austin might not be increased and crime rates would react differently than those in DC 

# Question 2: The researchers at UPENN isolated this by using Washington, DC as a test city for their study due to the increase in police presence on days when terrorism threats are increased,
# This means that the city had more police presence at times, unrelated to the level of crime actually being committed in the city. This allowed the study to recognize that days with higher police presence did indeed experience decreases in crime.
# The table shows that as you hold ridership rates of the DC Metro at a fixed rate on days with high terrorism alerts, an increase in police does have a negative effect on crime levels, but at a lower rate than when one does not hold the ridership rate at a fixed rate. 
# This might not be fool-proof though, given that criminals might not be as willing to commit crimes on days that are dangerous to be out, such as those with high terrorism rates.

# Question 3: The study controlled METRO ridership in DC to see if the amount of civilians outside on days during high terrorism alerts was similar to those without terrorism alerts.
# This ensures that the number of potential victims of crime did not decrease, which in turn would decrease the crime rate, rather that the variable which impacted this decrease was the higher police presence.

# Question 4: Table 4 amended the study to see if the crime rate was similiarly impacted in different areas of DC. 
# The table shows that District 1's crime rate was the only one that was impacted, taking into account location and days with a terrorism threat.
# This is a reasonable conclusion given that District 1 contains the political center of the country, meaning much of the increased law enforcement presence is likely focused on District 1.
# The impact in Districts other than District 1, is indeed negative, but far smaller especially given the high standard error rate. 


# Problem 4:
library(nnet)
library(MASS)
attach(Boston)
summary(Boston)
data = Boston

# Standardize X
min = rep(0,3)
max = rep(0,3)
datas = data
for(x in 1:3) 
{
  min[x] = min(data[[x]])
  max[x] = max(data[[x]])
  datas[[x]] = (data[[x]]-min[x])/(max[x]-min[x])
}

# Predict Median Housing Value w/ lstat
set.seed(2)
lstat.test = nnet(medv~lstat,datas,size=3,decay=.1,linout=T)
summary(lstat.test)
pred.lstat.test=predict(lstat.test,datas)
plot(datas$lstat,datas$medv)
y = order(datas$lstat)
lines(datas$lstat[y],pred.lstat.test[y],col="blue",lwd=2)
abline(lm(medv~lstat,datas)$coef)

# Predict Variables
net = nnet(medv~.,datas,size=5,decay=.1,linout=T)
fnet = predict(net,datas)
lm = lm(medv~.,datas)
flm = predict(lm,datas)
temp = data.frame(y=datas$medv,fn=fnet,fl=flm)
pairs(temp)
print(cor(temp))

# Size and Decay


# Fits

set.seed(4)
z1 = nnet(medv~lstat,datas,size=5,decay=.07,linout=T)
z2 = nnet(medv~lstat,datas,size=5,decay=.0005,linout=T)
z3 = nnet(medv~lstat,datas,size=30,decay=.07,linout=T)
z4 = nnet(medv~lstat,datas,size=30,decay=.0005,linout=T)
temp = data.frame(medv = datas$medv, lstat = datas$lstat)
zf1 = predict(z1,temp)
zf2 = predict(z2,temp)
zf3 = predict(z3,temp)
zf4 = predict(z4,temp)

# Plotting Fits

par(mfrow=c(2,2))
plot(datas$lstat,datas$medv)
lines(datas$lstat[y],zf1[y],col="blue",lwd=2)
title("size=5, decay=.07")
plot(datas$lstat,datas$medv)
lines(datas$lstat[y],zf2[y],col="blue",lwd=2)
title("size=5, decay=.0005")
plot(datas$lstat,datas$medv)
lines(datas$lstat[y],zf3[y],col="blue",lwd=2)
title("size = 30, decay = .07")
plot(datas$lstat,datas$medv)
lines(datas$lstat[y],zf4[y],col="blue",lwd=2)
title("size = 30, decay = .0005")

# Fitting w/ Random Start Value

set.seed(1)
z3 = nnet(medv~lstat,datas,size=40,decay=.7,linout=T)
z3 = nnet(medv~lstat,datas,size=40,decay=.7,linout=T,maxit=25)
z3 = nnet(medv~lstat,datas,size=40,decay=.7,linout=T,maxit=1000)

zf3 = predict(z3,temp)
par(mfrow=c(1,1))
plot(datas$lstat,datas$medv)
lines(datas$lstat[y],zf3[y],col="blue",lwd=2)



print(summary(net))
x = datas$lstat
y = datas$medv

z1 = 4.35 -0.24 *x
z2 = -7.42 +21.41*x
z3 = -9.93 +13.28*x

f1 = 12.09*exp(z1)/(1+exp(z1))
f2 = 10.7*exp(z2)/(1+exp(z2))
f3 = 22.74*exp(z3)/(1+exp(z3))

y = order(x)
plot(x,y-12.33)

lines(x[y],f1[y],col=2) 
lines(x[y],f2[y],col=3) 
lines(x[y],f3[y],col=4) 
lines(x[y],(f1+f2+f3)[y],col=5)


set.seed(10)
x = runif(1000)
x = sort(x)
y = exp(-80*(x-.5)*(x-.5)) + .05*rnorm(1000)
plot(x,y)
df = data.frame(y=y,x=x)

plot(x,y)
sz = 3

for(i in 1:20) {
  nnsim = nnet(y~x,df,size=sz,decay = 0.75^i,linout=T,maxit=1000)
  simfit = predict(nnsim,df)
  lines(x,simfit,col=i,lwd=3)
  print(i)
  readline()
}

set.seed(10)
nnsim = nnet(y~x,df,size=3,decay=0.75^12,linout=T,maxit=1000)
thefit = predict(nnsim,df)
plot(x,y)
lines(x,thefit,col="red",lwd=3,cex.axis=1.5,cex.lab=1.5)


z1 =  5.26 - 13.74*x
z2 = -6.58 + 13.98*x
z3 = -9.67 + 17.87*x

F = function(x) {return(exp(x)/(1+exp(x)))}

f1 = 2.21*F(z1)
f2 = 7.61*F(z2)
f3 = -5.40*F(z3)


rx = range(x)
ry = range(c(f1,f2,f3,y))
plot(rx,ry,type="n",xlab="x",ylab="fit",cex.axis=2,cex.lab=2)
points(x,y)
lines(x,f1,col=1,lwd=2)
lines(x,f2,col=2,lwd=2)
lines(x,f3,col=3,lwd=2)
lines(x,-2.05+f1+f2+f3,col=4,lwd=4)


# Problem 5:
# On the team project, our group was very collaborative throughout the duration of the assignment. Through a total of 7 meetings, each lasting 2 to 3 hours, we worked on every step of the project as a single unit. 
# In these meetings, I helped select the dataset with which we completed the assignment with. I also led a workshop on how to use R to generate trees by going through the Chapter 8 lab in the book step-by-step with my team. 
# We then used this knowledge as a foundation to work on our project. I worked on the code for fitting of trees on my own, and went through the code with the group who assisted in editing it to meet the standards of all members. 
# Together, we also figured out how to conduct a Random Forest and Bagging model. Once our codes were to our satisfaction, I ensured I attended office hours held by Pedro to run our code and thought processes by him. 
# He advised additions and changes that would supplement our code, and using this insight I was able to guide my team to a more accurate and complete project. We then fine-tuned our code, and once it was to our liking, we began working on our presentation. 
# I helped each team member write their talking points by dissecting each line of code with the team member assigned to a particular section for the presentation. We then created the slides as a team, and we held multiple run-throughs together and individually. 
# Taking all of this into account, I would say our team dynamic was extremely unique in that our project was truly done as a team each step of the way. 
# There were moments in which we each were able to take leadership and play an integral role, which is how collaboration in its most efficient and productive form should work. 
# I am very pleased with my performance as a team member, and believe I exceeded all expectations that were set for me by my teammates and myself coming into the project. 
# I truly grew a lot given this was the first group assignment I had completed at the Master’s level, and I am eager to continue this momentum throughout the rest of my time in the MSBA cohort and beyond. 


