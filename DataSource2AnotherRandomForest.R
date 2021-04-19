# DATA SOURCE 2
#data source: https://www.kaggle.com/akanumur/random-forest-r

#clean the environment
rm(list=ls())
#import library packages
library(tidyverse)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(tibble)
library(caret)
library(stringr)
library(randomForest)

#list.files(path = "../input")

# read dataset

pet_test <- read.csv("test.csv", stringsAsFactors = T, header=T)
pet_train <- read.csv("train.csv", stringsAsFactors = T, header=T)
pet_color<- read.csv("color.csv", stringsAsFactors = T, header=T)
pet_state<- read.csv("state.csv", stringsAsFactors = T, header=T)
pet_breed<- read.csv("breed.csv", stringsAsFactors = T, header=T)
head(pet_test)
head(pet_train)
str(pet_test)
str(pet_train)
dim(pet_test)
dim(pet_train)
summary(pet_test)
summary(pet_train)

#remove just state variable

#removing categorical variables
#pet_tr<-pet_train[,-c(2,18,19,21)]
#pet_ts<-pet_test[,-c(2,18,19,21)]

#Classifying Breeds
pet_train <- mutate(pet_train, PureBreed = ifelse(Breed1 == 0 | Breed2 == 0, "Pure Breed", "Not Pure"))
#pet_train <- pet_train %>% left_join(StateName, by = c("State" = "StateID"))

#Extract the length of Description
pet_train$nlen_Desc<-str_length(pet_train$Description)

#Model Building
#Build a random forest model
pet_train$AdoptionSpeed <- as.factor(pet_train$AdoptionSpeed)
pet_train$PureBreed <- as.factor(pet_train$PureBreed)
#pet_train$StateName <- as.factor(pet_train$StateName)

rf<- randomForest(AdoptionSpeed~.-Name-RescuerID-PetID-Description, data=pet_train, ntree=100)
print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)

#Applying mtry to get optimum number of trees
pet_train <- select (pet_train,-c(Name,RescuerID,PetID,Description))
summary(pet_train)

mtry <- tuneRF(pet_train[-20], pet_train$AdoptionSpeed, ntreeTry=100, 
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)


print(best.m)


#rf <-randomForest(AdoptionSpeed~.-Name-RescuerID-PetID-Description, data=pet_train, mtry=best.m, importance=TRUE, ntree=100)
print(rf)

varImpPlot(rf)


#Model with Important variables
#rf_imp <- randomForest(AdoptionSpeed~nlen_Desc+Age+PhotoAmt+Breed1+Color2+Color1+StateName, data=pet_train, ntree=100,mtry =best.m)
#plot(rf_imp)

rf_imp <- randomForest(AdoptionSpeed~nlen_Desc+Age+PhotoAmt+Breed1+Color2+Color1, data=pet_train, ntree=100,mtry =best.m)
plot(rf_imp)

#Modifying test data

#Classifying Breeds
pet_test <- mutate(pet_test, PureBreed = ifelse(Breed1 == 0 | Breed2 == 0, "Pure Breed", "Not Pure"))

#pet_test <- pet_test %>% left_join(state, by = c("State" = "StateID"))
pet_test$nlen_Desc<-str_length(pet_test$Description)


#test$PureBreed <- as.factor(test$PureBreed)
#test$StateName <- as.factor(test$StateName)


pet_test$PureBreed <- as.factor(pet_test$PureBreed)
#pet_test$StateName <- as.factor(pet_test$StateName)

#Predicting output
pred <- predict(rf_imp, newdata=pet_test)
head(pred)


table(pred)

PetID <- pet_test$PetID
sub <- data.frame(PetID, AdoptionSpeed=pred)
write.csv(sub, file = "submission.csv", row.names=FALSE, quote = FALSE)