## SVM

## Remove Variables
rm(list=ls())

## Libraries
library(MASS)
library(e1071)
library(dplyr)

## Set Seed
set.seed(5082)

## Train Set
train <- read.csv("train.csv", header=T)
train$AdoptionSpeed <- factor(train$AdoptionSpeed, labels = c(0,1,2,3,4))
train <- select(train, -c(Name,RescuerID,PetID,Description))

## Set Training and Testing Observations
train_indices <- sample(nrow(train), nrow(train) * .75)
train_pets <- train[train_indices,]
test_pets <- train[-train_indices,]

## SVM Model
svm_pets <- svm(AdoptionSpeed~., data = train_pets, kernel = "radial", cost=10, gamma = 1)

## Summary
summary(svm_pets)

## Display results and error rate
table(svm_pets$fitted, train_pets$AdoptionSpeed)
mean(svm_pets$fitted==train_pets$AdoptionSpeed)

## Test Prediction
pred.pets <- predict(svm_pets, test_pets)
table(pred.pets, test_pets$AdoptionSpeed)
mean(pred.pets==test_pets$AdoptionSpeed) ## .3246199

