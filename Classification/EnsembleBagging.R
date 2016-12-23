#Bagging Modeling
packages_required = c('mlbench','caret','caretEnsemble','ggplot2','C50','gbm','plyr','ipred','randomForest')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}

# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)
library(ggplot2)
library(C50)
library(gbm)
library(plyr)
library(ipred)
library(randomForest)

#data
data = read.table("ecoli.data")
names(data)[1] <- "Sequence"
names(data)[2] <- "mcg"
names(data)[3] <- "gvh"
names(data)[4] <- "lip"
names(data)[5] <- "chg"
names(data)[6] <- "aac"
names(data)[7] <- "alm1"
names(data)[8] <- "alm2"
names(data)[9] <- "class"

data = data[,-1]
data = data[,c("mcg","gvh","aac","alm1","alm2","class")]

formula = class ~ mcg + gvh + aac + alm1 + alm2 

# Example of Bagging algorithms
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3,preProc = c("center", "scale"))
# treebag 
set.seed(7)
treebag_model <- train(formula, data=data, method="treebag", metric="Accuracy", trControl=trainControl)
# gbm 
set.seed(7)
gbm_model <- train(formula, data=data, method="gbm", metric="Accuracy", trControl=trainControl)
# Random Forest
set.seed(7)
randomForest_model <- train(formula, data=data, method="rf", metric="Accuracy", trControl=trainControl)

summary(treebag_model)
summary(gbm_model)
summary(randomForest_model)

# summarize results
bagging_results_1 <- resamples(list(treebag=treebag_model, rf=randomForest_model,gbm=gbm_model))
summary(bagging_results_1)
dotplot(bagging_results_1)
