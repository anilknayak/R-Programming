# Training SVM Models

#installing new packages if required

packages_required = c('caret','dplyr','kernlab','pROC')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}

library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	         # plot the ROC curves

data = read.table("ecoli.data")
pdf("SupportVectorMachineSigmoidalSeparable.pdf")
names(data)[1] <- "Sequence"
names(data)[2] <- "mcg"
names(data)[3] <- "gvh"
names(data)[4] <- "lip"
names(data)[5] <- "chg"
names(data)[6] <- "aac"
names(data)[7] <- "alm1"
names(data)[8] <- "alm2"
names(data)[9] <- "class"

trainIndex <- createDataPartition(data$class,p=.5,list=FALSE)
trainData <- data[trainIndex,]
testData  <- data[-trainIndex,]
trainX <-trainData[,2:]        # Pull out the variables for training
print(sapply(trainX,summary))           # Look at a summary of the training data

## SUPPORT VECTOR MACHINE MODEL
# First pass
set.seed(1492)
# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)

#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

print(svm.tune)


set.seed(1492)
# Use the expand.grid to specify the search space	
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

print(svm.tune)