#CLASSIFICATION MACHINE LEARNING:LOGISTIC REGRESSION
#==============================================-
#CLEAR WORK SPACE
rm(list = ls())
#=========================================-
#SET WORKING PATH
setwd("C:/CDK")
####Loading Packages
library(readxl)
library(dplyr)
library(caret)
library(doSNOW)
library(xgboost)
#LOAD DATA SET==============-
CDK <- read_excel("CDK.xlsx",sheet="framingham")
####==========================================-
Framigh<-CDK|>
  select(male,currentSmoker,sysBP,diaBP,TenYearCHD)|>
  rename(Gender=male)
#==============================================-
###Set up factors
Framigh$TenYearCHD<-as.factor(Framigh$TenYearCHD)
Framigh$currentSmoker<-as.factor(Framigh$currentSmoker)
####==============================
##Use caret to create a 80/20% split of the training data
set.seed(124)
Indexes<-createDataPartition(Framigh$TenYearCHD,
                             times = 1,
                             p=0.7,
                             list = FALSE)
Framigh.train<-Framigh[Indexes,]
Framigh.test<-Framigh[-Indexes,]
##Examine the proportions
library(summarytools)
freq(Framigh.train$TenYearCHD,report.nas = FALSE)
freq(Framigh.test$TenYearCHD,report.nas = FALSE)
freq(Framigh$TenYearCHD,report.nas = FALSE)
#============================================
## Setting caret to perform 5-fold cross validation
train.control<-trainControl(method = "repeatedCV",
                            number = 5,
                            #repeats = 3,
                            search = "grid")
#Leverage a grid search of hyperparameter for xgboost
tune.grid<-expand.grid(eta=c(0.05,0.075,0.1),
                       nrounds=c(50,75,100),
                       max_depth=6:8,
                       min_birthweight=c(0.3,2.25,2.5),
                       colsample_bytree=c(0.3,0.4,0.5),
                       gamma=0,
                       subsample=1)
View(tune.grid)
#doSNOW package to enable caret to train in parallel 
Cl<-makeCluster(5,type="SOCK")
# Register cluster so that caret will know train in parallel
registerDoSNOW(Cl)
# Train the xgboost model using 10-fold cv 
caret.cv<-train(TenYearCHD~.,
                data = Framigh.train,
                method="xgbTree",
                tunegrid=tune.grid,
                trcontrol=train.control)
stopCluster(Cl)

#Make predictions on the test set using a xgboost model
#trained on all 133 rows of the training set 
pred<-predict(caret.cv,Framigh.test)
pred
#==================================================
# Examine caret's processing result
caret.cv
#==================================================
# Use caret's confusion matrix() to estimate the effectiveness
#of this model
confusionMatrix(pred,Framigh.test$TenYearCHD)
#===================================================================