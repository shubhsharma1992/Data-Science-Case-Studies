library(randomForest) ### library 0gives an error if package is not available
# read the data
datafile <- 'credit-card-default.csv'
df <- read.csv(datafile)
#df$defaulted <- as.factor(df$defaulted)
# df[] <- lapply(df, factor)
df[, 1] <- NULL
numericcols <- c('AGE', 'PAY_0', 'PAY_2', 'PAY_3', 'PAY_4', 'PAY_5', 'PAY_6',
                 'BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 'BILL_AMT4', 'BILL_AMT5',
                 'BILL_AMT6', 'PAY_AMT1', 'PAY_AMT2', 'PAY_AMT3', 'PAY_AMT4', 'PAY_AMT5',
                 'PAY_AMT6')
factorcols <- c('SEX', 'EDUCATION', 'MARRIAGE', 'defaulted')
df[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(df[, x])))
df[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(df[, x])))
# Shuffle the data
shuffledata <- df[sample(nrow(df)), ]

# Split the data into train and test
ntrain <- as.integer(nrow(shuffledata)*0.8)
traindata <- shuffledata[1:ntrain, ]
testdata <- shuffledata[(ntrain+1):nrow(shuffledata), ]

# Build the random forest
set.seed(71)
data.rf <- randomForest(defaulted ~ ., data=traindata, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)

data.rf$err.rate
data.rf
dim(traindata)

## Using Tune RF function #### mtry value with minimum out of bag(OOB) error.
bestmtry <- tuneRF(traindata[,-10], traindata[,10], stepFactor=1.5, improve=0.01, ntree=500)

## nTreeTry in tuneRF is 50 by default
## mtryStart is sqrt(p) and p/3 by default
## stepFactor	: at each iteration, mtry is inflated (or deflated) by this value
## improve : the (relative) improvement in OOB error must be by this much for the search to continue
## trace : specifies whether to print the progress of the search
## plot : specifies whether to plot the OOB error as function of mtry

print(bestmtry)

testPred <- predict(data.rf, newdata=testdata)
table(testPred, testdata$defaulted)

## Peformance

pred1<-predict(data.rf,type='prob')
library(ROCR)
perf = prediction(pred1[,2], data.rf$Creditability)

# 1. Area under curve
auc = performance(perf, "auc")
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
