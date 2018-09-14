############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Model evaluation -Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a handwritten  digit of black-and-white
#rectangular pixel displays as one of the Digits in the digit system

#####################################################################################

# 2. Data Understanding: 
# This is a multi-class classification problem, to learn from train data set
# (having 60000 image data) and test the model on test data (10000 image data)

#####################################################################################

# 3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(caTools)
library(ggplot2)
library(gridExtra)

#Loading Datasets as training and testing

train <- read.csv("mnist_train.csv",stringsAsFactors = FALSE,header = FALSE)
test <- read.csv("mnist_test.csv",stringsAsFactors = FALSE,header = FALSE)


#######Data Cleansing############

# First Column name changed to Digit 

colnames(train)[1] <- "digit"
colnames(test)[1] <- "digit"


# Lets add a column to signify train/test data in each data frame 
train$type <- "train"
test$type <- "test"


# Binding both test and train  
Dataset <- rbind(train,test)

# Check for NAs in the Data Set #
which(sapply(Dataset,function(x) sum(is.na(x))) != 0)    ####### No NAs

# Check for duplicate values
duplicates <- which(sapply(Dataset,function(x) length(unique(x)) == 1))
length(duplicates)                            ######## 65 columns have duplicates

# Since the value in these columns is same , we can say that axis of the dimension is a constant
# and those columns can be removed since they don't give any valuable information

Dataset_2 <- Dataset %>% select(-duplicates)


### Check for values more than 9 and less than 0
###colnames(Dataset_2)[1]<-'digit'

which((Dataset_2 %>% select(digit)) > 9 | (Dataset_2 %>% select(digit)) < 0) # No digit given is more than 9 or less than zero.

# Lets get the data back in train and test sets now.

traindata <- subset(Dataset_2,type == "train")
traindata<-traindata[,-721]

testdata <- subset(Dataset_2,type='test')
testdata<-testdata[,-721]

# Convert the digit column to factors for both data set #
traindata$digit <- as.factor(traindata$digit)
testdata$digit <- as.factor(testdata$digit)

############# Data Preparation###########

set.seed(100)

train_indices <- sample.split(traindata$digit,SplitRatio = 0.6667)
train_dataset <- traindata[train_indices,]
# nrow(train_dataset) = 40003

test_dataset<-traindata[!train_indices,]

#######################################################################################

######### 4.  Model Building ######################

########### 4.1 Linear Kernel and performance check ###############

svm_linear_model_1 <- ksvm(digit~., data=train_dataset,scale=FALSE,kernel="vanilladot")

svm_linear_model_1
# Hyper Parameter : C = 1
# Support Vectors : 13709

# check the training accuracy #
eval_train_linear_model_1 <- predict(svm_linear_model_1,train_dataset)
confusionMatrix(eval_train_linear_model_1,train_dataset$digit)
# Train Set Net Accuracy = 0.959

# check test accuracy #
eval_test_linear_model_1 <- predict(svm_linear_model_1,test_dataset)
confusionMatrix(eval_test_linear_model_1,test_dataset$digit)
# Test Set Net Accuracy of model = 0.892

################ 4.2 Non-Linear (RBF - Kernel) SVM and performance check############
# Build an RBF to see if performance increases #

svm_non_linear_RBF_model_1 <- ksvm(digit~.,data=train_dataset,scale=FALSE,kernel="rbfdot")
svm_non_linear_RBF_model_1
  # Hyper Parameter :     C = 1
#                 : sigma = 1.63635843183793e-07
# Support Vectors : 9501

# Lets check the training accuracy #
eval_train_non_linear_model_1 <- predict(svm_non_linear_RBF_model_1,train_dataset)
confusionMatrix(eval_train_non_linear_model_1,train_dataset$digit)
# Train Set Net Accuracy = 0.9851

# Lets Check test accuracy #
eval_test_non_linear_RBF_model_1 <- predict(svm_non_linear_RBF_model_1,test_dataset)
confusionMatrix(eval_test_non_linear_RBF_model_1,test_dataset$digit)
# Net Accuracy of model = 0.973

# Conclusion from modelling exercise :
# Results show that the RBF model performs better (even with default kernel parameters) #
# With non-linear model (default parameters) we can tune non-linear model through cross validation for better results.

# Building the cross validation model based on this
# The Sigma built by default RBF model says , sigma = 1.63635843183793e-07
# As we saw that in linear model also we got an accuracy of 0.8932, sigma of non-linear
# model is of the order 1e-7, that means there is not much non-linearity in the data.

######################################################################################


###### 5. Model Evaluation - Cross Validation ##########

# Cross Validation folds = 3
# Range of sigma = 0.63e-7, 1.63e-7, 2.63e-7
# Range of C = 1 2 3

trainControl <- trainControl(method = "cv", number = 3,verboseIter=TRUE)

metric <- "Accuracy"

set.seed(90)

grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))


SVM_non_linear_RBF.fit <- train(digit~.,data=train_dataset,method="svmRadial",
                                metric=metric,tuneGrid=grid,trControl=trainControl)
SVM_non_linear_RBF.fit
#Support Vector Machines with Radial Basis Function Kernel 

#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#Resampling: Cross-Validated (3 fold) 

#The final values used for the model with highest accuracy were sigma = 2.63e-07 and C = 3.

plot(SVM_non_linear_RBF.fit)


# Build a model with C = 3 and sigma = 2.63e-07

SVM_non_linear_RBF_model_final <- ksvm(digit~.,data=train_dataset,kernel="rbfdot",
                                       scale=FALSE,C=3,kpar=list(sigma=2.63e-7))
SVM_non_linear_RBF_model_final
# Hyper Parameters :     C = 3 ,sigma = 2.63e-7
# Support vectors  : 9709

# Lets check training accuracy #
eval_train_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,train_dataset)
confusionMatrix(eval_train_non_linear_RBF_model_final,train_dataset$digit)
# Net Train Accuracy = 0.998 #

# Lets check the test accuracy #
eval_test_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,test_dataset)
confusionMatrix(eval_test_non_linear_RBF_model_final,test_dataset$digit)
# Net test Accuracy = 0.982 #

# There is very less difference between train and test accuracy, hence we can say that 
# our model is able to predict correct digits using Non-Linear SVM to a large extent
# The  over-fitting is very less and model is more generalisable.

# Inference #
# Non-linearity in the data set seems to be present to a less extent as the value of hyperparameter
# sigma is of the order of 1e-7. But the accuracy of the model also increases.
# After performing cross validation for rbfdot kernel, it was observed that
# maximum accuracy can be seen with C=3 and sigma=2.63e-7.
# With these hyper parameters, the test accuracy is comparable to the train accuracy.
# The model is more generalisable and does not overfit the data.

# Final Hyper Parameters :
# C=3 , sigma = 2.63e-7