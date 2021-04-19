# clear environment
rm(list=ls())

#import library packages 
library(MASS)
library(e1071)

# loading dataset
pf_train <- read.csv("pf_train.csv", stringsAsFactors = T, header=T)
pf_train<-na.omit(pf_train)

# describing dataset features/variables
str(pf_train)
head(pf_train)
dim(pf_train)

#removing categorical variables: Name, RescuerID, State, Description, PetID 
pf_train<-pf_train[,-c(2,18,19,21, 22)]
pf_train$AdoptionSpeed<-as.factor(pf_train$AdoptionSpeed)

#setup seed
set.seed(1)

#split dataset as training (%80) and testing (%20)
train_index <- sample(1:nrow(pf_train), nrow(pf_train)*.8)

# Create a random index
train_pet <- pf_train[train_index,]   # Create training set
test_pet <- pf_train[-train_index,]   # Create test set
svm1 <- svm(AdoptionSpeed~., data=train_pet, #run the model on different cost and gamma values
              method="C-classification", kernal="radial", 
            ranges=list(cost=c(1), gamma=c(.2,.3, .4)))
summary(svm1) #confirm that the results are being classified

# Find the best model
bestSVM <- svm1$best.model #display the best model
summary(bestSVM) #see what the best cost and gamma are (cost = 1, gamma = 0.3)
prediction <- predict(svm1, test_pet) #predict on the test data
xtab <- table(test_pet$AdoptionSpeed, prediction) #make a confusion matrix
xtab #view the confusion matrix
1-mean(prediction==test_pet$AdoptionSpeed) ## Calculate error percentage - 0.6145382
#prediction
#    0   1   2   3   4
#0   0  31  27   3  25
#1   0 147 335  34 144
#2   0 110 412  56 193
#3   0  76 280  98 207
#4   0  56 228  38 499