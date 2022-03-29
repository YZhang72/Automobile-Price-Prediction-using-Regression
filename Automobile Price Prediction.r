library(psych)
library(caret)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)


library(ISLR)
library(rpart)
library(tree) 

automobile <- read.table("~/Desktop/BA749 Project/code/automobile_analysis.csv", dec = ",", header = TRUE, sep = ",", na.strings = "?")
automobile
# removed num of cylinders because it has no data in it
automobile = subset(automobile, select = -c(num_of_cylinders) )
automobile

# review data structure
str(automobile)
names(automobile)
head(automobile)


# Even though imported dataset set columns that should be numeric to numeric, 
# there are several columns still treated as non numeric

automobile$normalized_losses <- as.numeric(as.character(automobile$normalized_losses)) 
automobile$wheel_base <- as.numeric(as.character(automobile$wheel_base)) 
automobile$length <- as.numeric(as.character(automobile$length)) 
automobile$width <- as.numeric(as.character(automobile$width)) 
automobile$height <- as.numeric(as.character(automobile$height)) 
automobile$bore <- as.numeric(as.character(automobile$bore)) 
automobile$stroke <- as.numeric(as.character(automobile$stroke)) 
automobile$compression_ratio <- as.numeric(as.character(automobile$compression_ratio)) 
automobile$horsepower <- as.numeric(as.character(automobile$horsepower)) 
automobile$peak_rpm <- as.numeric(as.character(automobile$peak_rpm)) 
automobile$price <- as.numeric(as.character(automobile$price)) 

automobile
summary(automobile)
str(automobile)

#convert character type data to numerics 
automobile[sapply(automobile, is.character)] <- lapply(automobile[sapply(automobile, is.character)], 
                                                       as.numeric)

str(automobile)


##### Imputation of all columns with mean cite : https://statisticsglobe.com/mean-imputation-for-missing-data/

for(i in 1:ncol(automobile)){
  automobile[is.na(automobile[,i]), i] <- mean(automobile[,i], na.rm = TRUE)
}
head(automobile)


#Create a correlation matrix
res <- cor(automobile)
round(res, 2)

#find the searches through a correlation 
#matrix and returns a vector of integers corresponding to columns to remove to 
#reduce pair-wise correlations.
findCorrelation(
  res,
  cutoff = 0.8,
  verbose = FALSE,
  names = FALSE,
  exact = ncol(res) < 100
)


names(automobile)
#get the names of the columns.
names(automobile[c(7, 4, 12, 16, 15, 19, 44, 45, 66, 50, 46, 35, 60, 21, 63, 65)])

# remove highly correlated variables from our data
automobile = subset(automobile, select = -c(curb_weight, length, horsepower, 
                                            highway_mpg, rwd, diesel, gas, idi,
                                            two, std, peugot, ohcf, rear, X1bbl, X4bbl) )

names(automobile)

library ('caret')
# remove low variance variables from our data
nearZeroVar(automobile)
names(automobile[c(13,15,16,17, 18, 19, 20, 22, 23, 25, 26, 29, 30, 31, 32, 38, 40, 41, 46, 50, 52, 54, 55)])

automobile = subset(automobile, select = -c(X4wd, front, alfa.romero, 
                                            audi, bmw, chevrolet, dodge, isuzu,
                                            jaguar, mercedes.benz, mercury, plymouth, porsche, renault, saab,
                                            X., convertible, hardtop, dohcv, rotor, mfi, spdi, spfi) )

names(automobile)




### TEST TRAIN SPLIT cite https://www.listendata.com/2015/02/splitting-data-into-training-and-test.html
set.seed(123)
automobile_subset = sort(sample(nrow(automobile), nrow(automobile)*.8))
train<-automobile[automobile_subset,]
test<-automobile[-automobile_subset,]

#identify possible skewed variables
skewValues<- apply(train, 2, skew) #applies skewness to each column
skewValues

skewSE <- sqrt(6/nrow(train)) #Standard error of skewness
skewSE

#anything over 2 SEs in skew is potentially problematic
abs(skewValues)/skewSE > 2

# most of our variables are skewed !!!!!?????

##Visualize data distributions of variables with high skew identified above
multi.hist(train[,abs(skewValues)/skewSE > 2])
par(mfrow=c(1,1)) #reset plot display to 1



################## Linear Regression ####################
#first set parameters of resampling and training
# defining training controls cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

#lets fit our first linear regression model
set.seed(123)
auto.lm <- train(price ~ ., data= train, method = "lm", trControl=train_control)

#Review cross-validation performance
auto.lm

#we can review what variables were most important
varImp(auto.lm)

#we can also review the final trained model just like if we used lm()
summary(auto.lm)

#review residuals and actual observations against predictions
par(mfrow=c(1,2)) 
plot(train$price ~ predict(auto.lm), xlab="predict", ylab="actual")
plot(resid(auto.lm) ~ predict(auto.lm), xlab="predict", ylab="residual")

# MSE training Error of linear regression
yhat.lm<- predict(auto.lm, train)
mse_lm_train = mean((train$price - yhat.lm)^2)
mse_lm_train
# RMSE training error
rmse_lm_train = sqrt(mean((train$price - yhat.lm)^2))
rmse_lm_train
# MAE training error
mae_lm_train = mean(abs(train$price - yhat.lm))
mae_lm_train
# R^2 training error
R2_lm_train = 1-(sum((train$price - yhat.lm)^2)/sum((train$price-mean(train$price))^2))
R2_lm_train

# MSE train is 6766238
# RMSE train is 2601.199
# MAE train is 1791.629
# R^2 train is 0.8914985

# MSE test Error of linear regression
yhat.lm <- predict(auto.lm, test)
mse_lm = mean((test$price - yhat.lm)^2)
mse_lm
# RMSE test error
rmse_lm = sqrt(mean((test$price - yhat.lm)^2))
rmse_lm
# MAE test error
mae_lm = mean(abs(test$price - yhat.lm))
mae_lm
# R^2 test error
R2_lm = 1-(sum((test$price - yhat.lm)^2)/sum((test$price-mean(test$price))^2))
R2_lm

# MSE test is 12012566
# RMSE test is 12012566
# MAE test is 2594.578
# R^2 test is 0.7941914


######## Simple linear regression of price based on engine-size
# delete ?????
#formula notation DV ~ IV, 
hist(automobile$engine_size)
plot(price ~ engine_size, data = automobile)


engine_size_lm <- lm(price ~ engine_size, data = automobile)

#lets visualize fitted line
plot(automobile$price ~ automobile$engine_size)
abline(engine_size_lm, col="red")

#review model fit, 
summary(engine_size_lm)


#What is SE of slope?
confint(engine_size_lm) #defaults 95%, X-2*SE to X +2*SE*

#extract values from model summary
#calculate t value, simple example extracting these values can be useful
coef(summary(engine_size_lm))
coef(summary(engine_size_lm))[2] #estimate of slope
coef(summary(engine_size_lm))[2,2] #second coefficent std error
coef(summary(engine_size_lm))[2,4] #extract p value 


#to automatically create charts and reports
coef(summary(engine_size_lm))[2] / coef(summary(engine_size_lm))[2,2]

#residual standard error
summary(engine_size_lm)

#review sum of squares break down
# MSE
anova(engine_size_lm)
anova(engine_size_lm)['Residuals', 'Mean Sq']
anova(engine_size_lm)['engine_size', 'Mean Sq']

#review residuals and actual observations against predictions
par(mfrow=c(1,2)) 
plot(train$price ~ predict(engine_size_lm), xlab="predict", ylab="actual") #ERROR
plot(resid(engine_size_lm) ~ predict(engine_size_lm), xlab="predict", ylab="residual")




### Multiple Linear Regression
# remove? he said he wants us to use all variables, not make a selection 
cor(automobile$engine_size, automobile$wheel_base)
hist(automobile$wheel_base)

price.lm<-lm(price ~ engine_size + wheel_base, data = automobile)
summary(price.lm)



################## K NEAREST NEIGHBOR #####################
#lets fit our first linear regression model

#set values of k to search through, K 1 to 15
k.grid <- expand.grid(k=1:50)

set.seed(123)
auto.knn <- train(price ~ ., data= train, method = "knn", 
               tuneGrid=k.grid, trControl=ctrl)
auto.knn
varImp(auto.knn)

#we can plot parameter performance
plot(auto.knn)


#let preprocess K means performs better when standardized!
set.seed(123) 
auto.knn.pp <- train(price ~ ., data= train, method = "knn", 
                  preProcess=c("center", "scale"),
                  tuneGrid=k.grid, trControl=ctrl)
auto.knn.pp
plot(auto.knn.pp)

# MSE training Error of polynomial regression
yhat.knn <- predict(auto.knn.pp, train)
mse_knn_train = mean((train$price - yhat.knn)^2)
mse_knn_train
# RMSE training error
rmse_knn_train = sqrt(mean((train$price - yhat.knn)^2))
rmse_knn_train
# MAE training error
mae_knn_train = mean(abs(train$price - yhat.knn))
mae_knn_train
# R^2 training error
R2_knn_train = 1-(sum((train$price - yhat.knn)^2)/sum((train$price-mean(train$price))^2))
R2_knn_train


# MSE train is 11016810
# RMSE train is 3319.158
# MAE train is 2116.221
# R^2 train is 0.8233376

# MSE test Error of polynomial regression
yhat.knn <- predict(auto.knn.pp, test)
mse_knn = mean((test$price - yhat.knn)^2)
mse_knn
# RMSE test error
rmse_knn = sqrt(mean((test$price - yhat.knn)^2))
rmse_knn
# MAE test error
mae_knn = mean(abs(test$price - yhat.knn))
mae_knn
# R^2 test error
R2_knn = 1-(sum((test$price - yhat.knn)^2)/sum((test$price-mean(test$price))^2))
R2_knn

# MSE test is 16217315
# RMSE test is 4027.073
# MAE test is 2486.673
# R^2 test is 0.7221523


############# POLYNOMIAL REGRESSION ###############
set.seed(123)
names(automobile)
#Polynomial Regression, 4th degree polynomial
# should be 61 predictors 
poly.lm <- lm(price ~ poly(symboling+normalized_losses+wheel_base+width+height+engine_size+
                             bore+stroke+compression_ratio+peak_rpm+city_mpg+fwd+
                             honda+mazda+mitsubishi+nissan+subaru+toyota+volkswagen+
                             volvo+turbo+four+hatchback+sedan+wagon+dohc++l+ohc+ohcv+X2bbl+mpfi,4,raw=T), data=automobile)

summary(poly.lm)


#we can plot parameter performance
plot(poly.lm)

# MSE training Error of polynomial regression
yhat.poly_train<- predict(poly.lm, train)
mse_poly_train = mean((train$price - mse_poly_train)^2)
mse_poly_train
# RMSE training error
rmse_poly_train = sqrt(mean((train$price - mse_poly_train)^2))
rmse_poly_train
# MAE training error
mae_poly_train = mean(abs(train$price - mse_poly_train))
mae_poly_train
# R^2 training error
R2_poly_train = 1-(sum((train$price - yhat.poly_train)^2)/sum((train$price-mean(train$price))^2))
R2_poly_train

# MSE train is 62025475
# RMSE train is 7875.625
# MAE train is 5754.197
# R^2 train is 0.005377212

# MSE test Error of polynomial regression
yhat.poly_test <- predict(poly.lm, test)
mse_poly_test = mean((test$price - yhat.poly_test)^2)
mse_poly_test
# RMSE test error
rmse_poly_test = sqrt(mean((test$price - yhat.poly_test)^2))
rmse_poly_test
# MAE test error
mae_poly_test = mean(abs(test$price - yhat.poly_test))
mae_poly_test
# R^2 test error
R2_poly_test = 1-(sum((test$price - yhat.poly_test)^2)/sum((test$price-mean(test$price))^2))
R2_poly_test

# MSE test is 53179862
# RMSE test is 7292.452
# MAE test is 5503.062
# R^2 test is 0.08888123

##### K-fold cross validation
# setting seed to generate a
# reproducible random sampling
set.seed(123)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
poly_model <- train(price ~., data = automobile,
                             method = "lm",
                             trControl = train_control)

# printing model performance metrics
# along with other details
print(poly_model)

# MSE test Error of polynomial regression
yhat.poly <- predict(poly_model, test)
mse_poly = mean((test$price - yhat.poly)^2)
mse_poly
# RMSE test error
rmse_poly = sqrt(mean((test$price - yhat.poly)^2))
rmse_poly
# MAE test error
mae_poly = mean(abs(test$price - yhat.poly))
mae_poly
# R^2 test error
R2_poly = 1-(sum((test$price - yhat.poly)^2)/sum((test$price-mean(test$price))^2))
R2_poly

# MSE test is 7163806
# RMSE test is 2676.529
# MAE test is 2000.116
# R^2 test is 0.8772641


############# GAM ################
set.seed(123)
library(gam)
gam.fit <- gam(price ~symboling+normalized_losses+wheel_base+width+height+engine_size+
                 bore+stroke+compression_ratio+peak_rpm+city_mpg+fwd+
                 honda+mazda+mitsubishi+nissan+subaru+toyota+volkswagen+
                 volvo+turbo+four+hatchback+sedan+wagon+dohc++l+ohc+ohcv+X2bbl+mpfi, data = automobile)

summary(gam.fit)


# MSE training Error of gam
yhat.gam_train <- predict(gam.fit, train)
mse_gam_train = mean((train$price - yhat.gam_train)^2)
mse_gam_train
# RMSE training error
rmse_gam_train = sqrt(mean((train$price - yhat.gam_train)^2))
rmse_gam_train
# MAE training error
mae_gam_train = mean(abs(train$price - yhat.gam_train))
mae_gam_train
# R^2 training error
R2_gam_train = 1-(sum((train$price - yhat.gam_train)^2)/sum((train$price-mean(train$price))^2))
R2_gam_train

# MSE train is 7233025
# RMSE train is 2689.428
# MAE train is 1811.955
# R^2 train is 0.8840133

# MSE test Error of gam
yhat.gam_test <- predict(gam.fit, test)
mse_gam = mean((test$price - yhat.gam)^2)
mse_gam
# RMSE test error
rmse_gam = sqrt(mean((test$price - yhat.gam)^2))
rmse_gam
# MAE test error
mae_gam = mean(abs(test$price - yhat.gam))
mae_gam
# R^2 test error
R2_gam = 1-(sum((test$price - yhat.gam)^2)/sum((test$price-mean(test$price))^2))
R2_gam

# MSE test is 7163806
# RMSE test is 2676.529
# MAE test is 2000.116
# R^2 test is 0.8772641

##### K-fold cross validation
# setting seed to generate a
# reproducible random sampling
set.seed(123)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
gam_model <- train(price ~., data = automobile,
                    method = "gam",
                    trControl = train_control)

# printing model performance metrics
# along with other details
print(gam_model)


# MSE test Error of gam
yhat.gam<- predict(gam_model, test)
mse_gam = mean((test$price - yhat.gam)^2)
mse_gam
# RMSE test error
rmse_gam = sqrt(mean((test$price - yhat.gam)^2))
rmse_gam
# MAE test error
mae_gam = mean(abs(test$price - yhat.gam))
mae_gam
# R^2 test error
R2_gam = 1-(sum((test$price - yhat.gam)^2)/sum((test$price-mean(test$price))^2))
R2_gam

# MSE test is 7163806
# RMSE test is 2676.529
# MAE test is 2000.116
# R^2 test is 0.8772641

############ DECISION TREE #############
#create tree
decision_tree <- rpart(price ~., data=train)

#summarize full tree (no pruning)
decision_tree

#by default tree plot needs some adjustments and labeling
plot(decision_tree)
text(decision_tree, pretty=0)

#rather than using default lets use new library
library(rpart.plot)

#very readable defaults
rpart.plot(decision_tree)

#tree is too bushy and has too much variance (overfit)
printcp(decision_tree) #display crossvalidated error for each tree size
plotcp(decision_tree) #plot cv error

#select CP with lowest crossvalidated error 
#manually this is 0.011

#we can grab this from the plotcp table automatically with 
opt.cp <- decision_tree$cptable[which.min(decision_tree$cptable[,"xerror"]),"CP"]
opt.cp
#lets prune the tree
decision_tree.pruned <- prune(decision_tree, cp=opt.cp)
decision_tree.pruned
#lets review the final tree
rpart.plot(decision_tree.pruned)

# MSE training Error of pruned decision tree
yhat.rtree_train <- predict(decision_tree.pruned, train)
mse_decisiontree_train = mean((train$price - yhat.rtree_train)^2)
mse_decisiontree_train
# RMSE training error
rmse_decisiontree_train = sqrt(mean((train$price - yhat.rtree_train)^2))
rmse_decisiontree_train
# MAE training error
mae_decisiontree_train = mean(abs(train$price - yhat.rtree_train))
mae_decisiontree_train
# R^2 training error
R2_decisiontree_train = 1-(sum((train$price - yhat.rtree_train)^2)/sum((train$price-mean(train$price))^2))
R2_decisiontree_train

# MSE train is 8590481
# RMSE train is 2930.952
# MAE train is 1854.312
# R^2 train is 0.8622455

##MSE test Error of pruned decision tree
yhat.rtree_tree_test <- predict(decision_tree.pruned, test)
mse_decisiontree_test = mean((test$price - yhat.rtree_tree_test)^2)
mse_decisiontree_test
#RMSE test error
rmse_decisiontree_test = sqrt(mean((test$price - yhat.rtree_tree_test)^2))
rmse_decisiontree_test
# MAE test error
mae_decisiontree_test = mean(abs(test$price - yhat.rtree_tree_test))
mae_decisiontree_test
# R^2 test error
R2_decisiontree_test = 1-(sum((test$price - yhat.rtree_tree_test)^2)/sum((test$price-mean(test$price))^2))
R2_decisiontree_test

# MSE test is 12151797
# RMSE test is 3485.943
# MAE test is 2740.055
# R^2 test is 0.791806

##### K-fold cross validation
# setting seed to generate a
# reproducible random sampling
set.seed(123)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
decision_tree_model <- train(price ~., data = automobile,
                       method = "rpart",
                       trControl = train_control)

# printing model performance metrics
# along with other details
print(decision_tree_model)


names(automobile)
library(randomForest)

##MSE test Error of pruned decision tree
yhat.rtree_tree <- predict(decision_tree_model, test)
mse_decisiontree = mean((test$price - yhat.rtree_tree)^2)
mse_decisiontree
#RMSE test error
rmse_decisiontree = sqrt(mean((test$price - yhat.rtree_tree)^2))
rmse_decisiontree
# MAE test error
mae_decisiontree = mean(abs(test$price - yhat.rtree_tree))
mae_decisiontree
# R^2 test error
R2_decisiontree = 1-(sum((test$price - yhat.rtree_tree)^2)/sum((test$price-mean(test$price))^2))
R2_decisiontree

# MSE test is 14473628
# RMSE test is 3804.422
# MAE test is 3026.395
# R^2 test is 0.7520265

set.seed(123)
############ BAGGING ############# 
#bagging (boot strapped tree generation using all variables no pruning)
#we use random forest function but set m = all variables rather than a subset
#automobile has 61 predictors
bag.model <- randomForest(price ~ ., data=train, mtry=61, ntree=500)

#lets review training error just to see relative performance
#will do cross validation later to estimate test error

plot(bag.model)
# MSE training Error of bagging
yhat.bag_train <- predict(bag.model, train)
mse_bag_train = mean((train$price - yhat.bag_train)^2)
mse_bag_train
#RMSE training error
rmse_bag_train = sqrt(mean((train$price - yhat.bag_train)^2))
rmse_bag_train
# MAE training error
mae_bag_train = mean(abs(train$price - yhat.bag_train))
mae_bag_train
# R^2 training error
R2_bag_train = 1-(sum((train$price - yhat.bag_train)^2)/sum((train$price-mean(train$price))^2))
R2_bag_train

# MSE train is 1693240 
# RMSE train is 1301.246 
# MAE train is 821.2544
# R^2 train is 0.9728477

# MSE test Error of bagging
yhat.bag_test <- predict(bag.model, test)
mse_bag_test = mean((test$price - yhat.bag_test)^2)
mse_bag_test
#RMSE test error
rmse_bag_test = sqrt(mean((test$price - yhat.bag_test)^2))
rmse_bag_test
# MAE test error
mae_bag_test = mean(abs(test$price - yhat.bag_test))
mae_bag_test
# R^2 test error
R2_bag_test = 1-(sum((test$price - yhat.bag_test)^2)/sum((test$price-mean(test$price))^2))
R2_bag_test


# MSE test is 6440750 
# RMSE test is 2537.863
# MAE test is 1874.326
# R^2 test is 0.8896521


##### K-fold cross validation
# setting seed to generate a
# reproducible random sampling
set.seed(123)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
bagging_model <- train(price ~., data = automobile,
                       method = "treebag",
                       trControl = train_control)

# printing model performance metrics
# along with other details
print(bagging_model)

# MSE test Error of bagging
yhat.bag <- predict(bagging_model, test)
mse_bag = mean((test$price - yhat.bag_test)^2)
mse_bag
#RMSE test error
rmse_bag = sqrt(mean((test$price - yhat.bag_test)^2))
rmse_bag
# MAE test error
mae_bag = mean(abs(test$price - yhat.bag_test))
mae_bag
# R^2 test error
R2_bag = 1-(sum((test$price - yhat.bag_test)^2)/sum((test$price-mean(test$price))^2))
R2_bag


# MSE test is 6440750 
# RMSE test is 2537.863
# MAE test is 1874.326
# R^2 test is 0.8896521
# SAME AS ABOVE

############ BAGGING 2 ############# (with subset of variables)

#Random Forest set smaller subset of variables for each tree
#using sqrt(61) rounded down to 7

set.seed(123)
bag2.model <- randomForest(price ~ ., data=train, mtry=7)
plot(bag2.model)
# MSE training Error of bagging
yhat.bag2 <- predict(bag2.model, train)
mse_bag2_train = mean((train$price - yhat.bag2)^2)
mse_bag2_train
#RMSE training error
rmse_bag2_train = sqrt(mean((train$price - yhat.bag2)^2))
rmse_bag2_train
# MAE training error
mae_bag2_train = mean(abs(train$price - yhat.bag2))
mae_bag2_train
# R^2 training error
R2_bag2_train = 1-(sum((train$price - yhat.bag2)^2)/sum((train$price-mean(train$price))^2))
R2_bag2_train

# MSE train 2354169
# RMSE train 1534.33
# MAE train is 994.8003
# R^2 train is 0.9622492

# MSE test Error of bagging
yhat.bag2 <- predict(bag2.model, test)
mse_bag2 = mean((test$price - yhat.bag2)^2)
mse_bag2
#RMSE test error
rmse_bag2 = sqrt(mean((test$price - yhat.bag2)^2))
rmse_bag2
# MAE test error
mae_bag2 = mean(abs(test$price - yhat.bag2))
mae_bag2
# R^2 test error
R2_bag2 = 1-(sum((test$price - yhat.bag2)^2)/sum((test$price-mean(test$price))^2))
R2_bag2

# MSE test 8189110  
# RMSE test 2861.662 
# MAE test is 2040.215
# R^2 test is 0.8596978



############ BOOSTING #############

#lets try boosting, creating sequential trees based on residuals
library(gbm)
set.seed(123)
# stochastic Gradient Boosting
#using default parameters of tree depth, tree size, and slowness lambda
auto.boost <- gbm(price ~ ., data=train)
summary(auto.boost) #see variable importance

plot(auto.boost)
# MSE training Error of boosting
yhat.boost <- predict(auto.boost, train, n.trees = 100)
mse_boost_train = mean((train$price - yhat.boost)^2)
mse_boost_train
#RMSE training error
rmse_boost_train = sqrt(mean((train$price - yhat.boost)^2))
rmse_boost_train
# MAE training error
mae_boost_train = mean(abs(train$price - yhat.boost))
mae_boost_train
# R^2 training error
R2_boost_train = 1-(sum((train$price - yhat.boost)^2)/sum((train$price-mean(train$price))^2))
R2_boost_train

# MSE train 6144206
# RMSE train 2478.751
# MAE train is 1655.052
# R^2 train is 0.9014733


# MSE test Error of boosting
yhat.boost_test <- predict(auto.boost, test, n.trees = 100)
mse_boost_test = mean((test$price - yhat.boost_test)^2)
mse_boost_test
#RMSE test error
rmse_boost_test = sqrt(mean((test$price - yhat.boost_test)^2))
rmse_boost_test
# MAE test error
mae_boost_test = mean(abs(test$price - yhat.boost_test))
mae_boost_test
# R^2 test error
R2_boost_test = 1-(sum((test$price - yhat.boost_test)^2)/sum((test$price-mean(test$price))^2))
R2_boost_test

# MSE test 10771922
# RMSE test 3282.061
# MAE test is 2507.263
# R^2 test is 0.815447

##### K-fold cross validation
# setting seed to generate a
# reproducible random sampling
set.seed(123)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
boosting_model <- train(price ~., data = automobile,
               method = "gbm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(boosting_model)


# MSE test Error of boosting
yhat.boost <- predict(boosting_model, test, n.trees = 100)
mse_boost = mean((test$price - yhat.boost)^2)
mse_boost
#RMSE test error
rmse_boost = sqrt(mean((test$price - yhat.boost)^2))
rmse_boost
# MAE test error
mae_boost = mean(abs(test$price - yhat.boost))
mae_boost
# R^2 test error
R2_boost = 1-(sum((test$price - yhat.boost)^2)/sum((test$price-mean(test$price))^2))
R2_boost

# MSE test 2629598
# RMSE test 1621.604
# MAE test is 1252.203
# R^2 test is 0.9549477

################# RANDOM FOREST ######################
# Random Forest: set smaller subset of variables for each tree
# we use the function below to decide the the number of variables tried at each split
set.seed(123)
library(randomForest)

floor(sqrt(ncol(train) - 1))

#we choose mtry=7
price.rf <- randomForest(price ~ ., data=train, mtry=7)

plot(price.rf)

#check the MSE training Error of random forest
yhat.rf <- predict(price.rf, train)
mse_rf_train = mean((train$price - yhat.rf)^2)
mse_rf_train
#RMSE training error
rmse_rf_train = sqrt(mean((train$price - yhat.rf)^2))
rmse_rf_train
# MAE training error
mae_rf_train = mean(abs(train$price - yhat.rf))
mae_rf_train
# R^2 training error
R2 = 1-(sum((train$price - yhat.rf)^2)/sum((train$price-mean(train$price))^2))
R2

# MSE train 2354169
# RMSE train 1534.33
# MAE train is 994.8003
# R^2 train is 0.9622492


# MSE test Error of random forest
yhat.rf_test <- predict(price.rf, test)
mse_rf_test = mean((test$price - yhat.rf_test)^2)
mse_rf_test
#RMSE test error
rmse_rf_test = sqrt(mean((test$price - yhat.rf_test)^2))
rmse_rf_test
# MAE test error
mae_rf_test = mean(abs(test$price - yhat.rf_test))
mae_rf_test
# R^2 test error
R2_rf_test = 1-(sum((test$price - yhat.rf_test)^2)/sum((test$price-mean(test$price))^2))
R2_rf_test

# MSE test 8189110
# RMSE test 2861.662
# MAE test is 2040.215
# R^2 test is 0.8596978


###cross-validation on random forest model
set.seed(123)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
model.rf <- train(price ~., data = automobile,
                  method = "rf",
                  trControl = train_control)

# printing model performance metrics along with other details
print(model.rf)

# MSE test Error of random forest
yhat.rf <- predict(model.rf, test)
mse_rf = mean((test$price - yhat.rf)^2)
mse_rf
#RMSE test error
rmse_rf = sqrt(mean((test$price - yhat.rf)^2))
rmse_rf
# MAE test error
mae_rf = mean(abs(test$price - yhat.rf))
mae_rf
# R^2 test error
R2_rf = 1-(sum((test$price - yhat.rf)^2)/sum((test$price-mean(test$price))^2))
R2_rf

# MSE test 1811835
# RMSE test 1346.044
# MAE test is 950.2813
# R^2 test is 0.9689582

################### RIDGE REGRESSION #####################
#glmnet package requires the response variable to be a vector 
#and the set of predictor variables to be of the class data.matrix
#define the response variable and set of predictor variables for training dataset
y <- train$price
x <- data.matrix(train[, names(train) != "price"])
#define the response variable and set of predictor variables for test dataset
x1 <- data.matrix(test[, names(test) != "price"])
y1 <- test$price

#implement ridge regression model
#cite: https://www.statology.org/ridge-regression-in-r/
set.seed(123)
ridge_model <- glmnet(x, y, alpha=0)

#perform k-fold cross-validation to find optimal lambda value
#build the model using that lambda
ridge_cv_model <- cv.glmnet(x, y, alpha = 0)
ridge_best_lambda <- ridge_cv_model$lambda.min
ridge_best_model <- glmnet(x, y, alpha = 0, lambda = ridge_best_lambda)

#produce a trace plot
plot(ridge_cv_model)
#produce plot of MSE by lambda value
plot(ridge_cv_model)

set.seed(123)
#check the MSE training Error of ridge regression
yhat.ridge <- predict(ridge_model, s = ridge_best_lambda, newx = x)
mse_ridge_train = mean((yhat.ridge-train$price)^2)
mse_ridge_train
#RMSE training error
rmse_ridge_train = sqrt(mean((train$price - yhat.ridge)^2))
rmse_ridge_train
# MAE training error
mae_ridge_train = mean(abs(train$price - yhat.ridge))
mae_ridge_train
# R^2 training error
R2_ridge_train = 1-(sum((train$price - yhat.ridge)^2)/sum((train$price-mean(train$price))^2))
R2_ridge_train

# MSE train 7726115
# RMSE train 2779.589
# MAE train is 1864.966
# R^2 train is 0.8761062

#check the MSE test Error of ridge regression
yhat.ridge <- predict(ridge_model, s = ridge_best_lambda, newx = x1)
mse_ridge = mean((yhat.ridge-y1)^2)
mse_ridge
#RMSE test error
rmse_ridge = sqrt(mean((y1 - yhat.ridge)^2))
rmse_ridge
# MAE test error
mae_ridge = mean(abs(y1 - yhat.ridge))
mae_ridge
# R^2 test error
R2_ridge = 1-(sum((y1 - yhat.ridge)^2)/sum((y1-mean(y1))^2))
R2_ridge

# MSE test 14791555
# RMSE test 3845.979
# MAE test is 2814.821
# R^2 test is 0.7465796

# defining training control as cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
model.ridge <- train(price ~ ., data = automobile, 
                     method = "ridge", 
                     trControl = train_control)

# printing model performance metrics
# along with other details
print(model.ridge)

#MSE test Error of ridge regression
yhat.ridge <- predict(model.ridge, test)
mse_ridge = mean((yhat.ridge-test$price)^2)
mse_ridge
#RMSE test error
rmse_ridge = sqrt(mean((test$price - yhat.ridge)^2))
rmse_ridge
# MAE test error
mae_ridge = mean(abs(test$price - yhat.ridge))
mae_ridge
# R^2 test error
R2_ridge = 1-(sum((test$price - yhat.ridge)^2)/sum((test$price-mean(test$price))^2))
R2_ridge

# MSE test 10685828
# RMSE test 3268.919
# MAE test is 2468.963
# R^2 test is 0.8169221

##################### LASSO ########################
#implement lasso regression model
set.seed(123)
library(glmnet)

#perform k-fold cross-validation to find the optimal lambda value
#https://www.statology.org/lasso-regression-in-r/ 
lasso_cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
#build the model using that lambda
lasso_best_lambda <- lasso_cv_model$lambda.min
lasso_best_model <- glmnet(x, y, alpha = 1, lambda = lasso_best_lambda)

#produce plot of test MSE by lambda value
plot(lasso_cv_model)

#check the MSE training Error of lasso regression
yhat.lasso <- predict(lasso_best_model, s = lasso_best_lambda, newx = x)
mse_lasso_train = mean((yhat.lasso-train$price)^2)
mse_lasso_train
#RMSE training error
rmse_lasso_train = sqrt(mean((train$price - yhat.lasso)^2))
rmse_lasso_train
# MAE training error
mae_lasso_train = mean(abs(train$price - yhat.lasso))
mae_lasso_train
# R^2 training error
rsq_lasso_train <- 1 - sum((yhat.lasso - y)^2)/sum((y - mean(y))^2)
rsq_lasso_train

# MSE train 8019561
# RMSE train 2831.883
# MAE train is 1905.12
# R^2 train is 0.8714006

#check the MSE test Error of lasso regression
yhat.lasso <- predict(lasso_best_model, s = lasso_best_lambda, newx = x1)
mse_lasso = mean((yhat.lasso-y1)^2)
mse_lasso
#RMSE test error
rmse_lasso = sqrt(mean((y1 - yhat.lasso)^2))
rmse_lasso
# MAE test error
mae_lasso = mean(abs(y1 - yhat.lasso))
mae_lasso
# R^2 test error
rsq_lasso <- 1 - sum((yhat.lasso - y1)^2)/sum((y1 - mean(y1))^2)
rsq_lasso

# MSE test 14576508
# RMSE test 3817.919
# MAE test is 2845.39
# R^2 test is 0.7502639

###cross-validation on lasso 
set.seed(123)

# defining training controls cross-validation and value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning price column
# as target variable and others as independent variable
# rdocument of"train" function: https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
model.lasso <- train(price ~ ., data=automobile, 
                     method = "lasso", 
                     trControl=train_control)

# printing model performance metrics
# along with other details
print(model.lasso)

#MSE test Error of ridge regression
yhat.lasso <- predict(model.lasso, test)
mse_lasso = mean((yhat.lasso -test$price)^2)
mse_lasso
#RMSE test erro
rmse_lasso = sqrt(mean((test$price - yhat.lasso)^2))
rmse_lasso
# MAE test error
mae_lasso = mean(abs(test$price - yhat.lasso))
mae_lasso
# R^2 test error
R2_lasso = 1-(sum((test$price - yhat.lasso)^2)/sum((test$price-mean(test$price))^2))
R2_lasso

# MSE test 7543288
# RMSE test 2746.505
# MAE test is 2061.111
# R^2 test is 0.8707625

##################### additional TESTING #####################
library(caret)
#lets gather the models
#first lets put all trained models in a list object

models<- list("Polynomial" = poly_model,
              "GAM" = gam_model,
              "DecisionTree" = decision_tree_model,
              "BaggingTree"= bagging_model,
              "BoostingTree" = boosting_model,
              "RandomForest" = model.rf, 
              "Lasso" = model.lasso,
              "Ridge" = model.ridge,
              "KNN" = auto.knn)

models
automobile.resamples<- resamples(models)
summary(automobile.resamples)

#plot performances
bwplot(automobile.resamples, metric="RMSE")
bwplot(automobile.resamples, metric="MAE")
bwplot(automobile.resamples, metric="Rsquared")

# Random Forest and Bagging best 

# Put error metrics into one place
mse_metrics <- c(mse_lm, mse_poly, mse_gam, mse_decisiontree, mse_bag, 
                 mse_bag2, mse_boost, mse_rf, mse_lasso, mse_ridge)
mse_metrics2 <- c(mse_lm, mse_poly, mse_gam, mse_decisiontree, mse_bag, 
                 mse_bag2, mse_boost, mse_rf)
rmse_metrics <- c(rmse_lm, rmse_poly, rmse_gam, rmse_decisiontree, rmse_bag, 
                 rmse_bag2, rmse_boost, rmse_rf, rmse_lasso, rmse_ridge)
rmse_metrics2 <- c(rmse_lm, rmse_poly, rmse_gam, rmse_decisiontree, rmse_bag, 
                  rmse_bag2, rmse_boost, rmse_rf)
mae_metrics <- c(mae_lm, mae_poly, mae_gam, mae_decisiontree, mae_bag, 
                 mae_bag2, mae_boost, mae_rf, mae_lasso, mae_ridge)
mae_metrics2 <- c(mae_lm, mae_poly, mae_gam, mae_decisiontree, mae_bag, 
                 mae_bag2, mae_boost, mae_rf)
R2_metrics <- c(R2_lm, R2_poly, R2_gam, R2_decisiontree, R2_bag, 
                R2_bag2, R2_boost, R2_rf, R2_lasso, R2_ridge)
R2_metrics2 <- c(R2_lm, R2_poly, R2_gam, R2_decisiontree, R2_bag, 
                R2_bag2, R2_boost, R2_rf)
names <- c("Linear",
          "Polynomial",
           "GAM",
           "DecisionTree",
           "BaggingTree",
           "BaggingTree2",
           "BoostingTree",
           "RandomForest", 
           "Lasso",
           "Ridge")

names2 <- c("Linear",
            "Polynomial",
            "GAM",
            "DecisionTree",
            "BaggingTree",
            "BaggingTree2",
            "BoostingTree",
            "RandomForest")

# Plot the bar chart 
barplot(mse_metrics, names.arg=names, xlab="Model Type",ylab="MSE", col="orange",
        main="MSE per Model",border="black")
barplot(rmse_metrics, names.arg=names, xlab="Model Type",ylab="RMSE", col="blue",
        main="RMSE per Model",border="black")
barplot(mae_metrics, names.arg=names, xlab="Model Type",ylab="MAE", col="purple",
        main="MAE per Model",border="black")
barplot(R2_metrics, names.arg=names, xlab="Model Type",ylab="R2", col="pink",
        main="R2 per Model",border="black")
 
# WITHOUT lasso and ridge
barplot(mse_metrics2, names.arg=names2, xlab="Model Type",ylab="MSE", col="orange",
        main="MSE per Model",border="black")
barplot(rmse_metrics2, names.arg=names2, xlab="Model Type",ylab="RMSE", col="blue",
        main="RMSE per Model",border="black")
barplot(mae_metrics2, names.arg=names2, xlab="Model Type",ylab="MAE", col="purple",
        main="MAE per Model",border="black")
barplot(R2_metrics2, names.arg=names2, xlab="Model Type",ylab="R2", col="pink",
        main="R2 per Model",border="black")


##################### FINE TUNING ########################

# we found that Random Forest and Boosting perform the best
# we will fine tune these to try and get the best results

################# RANDOM FOREST FINE TUNING ######################


###cross-validation on random forest model
set.seed(123)
# grid search 
rmforest.grid<- expand.grid(.mtry=c(1:20))

model.rf <- train(price ~., data = automobile,
                  method = "rf",
                  tunlength = 10,
                  trControl = train_control,
                  tuneGrid=rmforest.grid)

# printing model performance metrics along with other details
print(model.rf)
result <- getTrainPerf(model.rf)
# BEST: 7
# RMSE = 1358.87, R-squared = 0.965315, MAE = 986.54

rf_modify_RMSE = c(rmse_rf, result$TrainRMSE)
rf_modify_MAE = c(mae_rf, result$TrainMAE)
rf_modify_R2 = c(R2_rf, result$TrainRsquared)

rf_names = c("Original", "Modified")

barplot(rf_modify_RMSE, names.arg=rf_names, xlab="Random Forest",ylab="RMSE", col="blue",
        main="RMSE per Model",border="black")
barplot(rf_modify_MAE, names.arg=rf_names, xlab="Random Forest",ylab="MAE", col="purple",
        main="MAE per Model",border="black")
barplot(rf_modify_R2, names.arg=rf_names, xlab="Random Forest",ylab="R2", col="pink",
        main="R-Squared per Model",border="black")


############ BOOSTING FINE TUNING #############


##### K-fold cross validation
set.seed(123)

# grid search
boost.grid<- expand.grid(n.trees=seq(5,150,by=5), interaction.depth=seq(1,15),
                         shrinkage=c(.0001,.001,.01, .1, .5),
                         n.minobsinnode=c(1,10,20))

boosting_model <- train(price ~., data = automobile,
                        method = "gbm",
                        trControl = train_control,
                        tuneGrid=boost.grid)

# printing model performance metrics
# along with other details
print(boosting_model)
result2 <- getTrainPerf(boosting_model)

# BEST : n.trees = 25, interaction.depth = 14, shrinkage =  0.1, n.minobsinnode = 1
# RMSE: 1312.061, R2 = 0.9836467, MAE = 941.753
boost_modify_RMSE = c(rmse_boost, result2$TrainRMSE)
boost_modify_MAE = c(mae_boost, result2$TrainMAE)
boost_modify_R2 = c(R2_boost, result2$TrainRsquared)

boost_names = c("Original", "Modified")

barplot(boost_modify_RMSE, names.arg=boost_names, xlab="Boosting Tree",ylab="RMSE", col="blue",
        main="RMSE per Model",border="black")
barplot(boost_modify_MAE, names.arg=boost_names, xlab="Boosting Tree",ylab="MAE", col="purple",
        main="MAE per Model",border="black")
barplot(boost_modify_R2, names.arg=boost_names, xlab="Boosting Treee",ylab="R2", col="pink",
        main="R-Squared per Model",border="black")



