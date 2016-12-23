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

#Boosting Algorithms
# C5.0
# Stochastic Gradient Boosting

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
c50_model <- train(formula, data=data, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
gbm_model <- train(formula, data=data, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# summarize results
boosting_results <- resamples(list(c5.0=c50_model, gbm=gbm_model))
summary(boosting_results)
dotplot(boosting_results)


#Bagging Modeling
# Bagged CART
# Random Forest

# Example of Bagging algorithms
control_bagging <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
treebag_model <- train(formula, data=data, method="treebag", metric=metric, trControl=control_bagging)
# Random Forest
set.seed(seed)
randomForest_model <- train(formula, data=data, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results_1 <- resamples(list(treebag=treebag_model, rf=randomForest_model))
summary(bagging_results_1)
dotplot(bagging_results_1)


#'glm',
# create submodels
control_stacking <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart','rf' , 'knn', 'svmRadial')
set.seed(seed)
models <- caretList(formula, data=data, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# correlation between results
modelCor(results)
splom(results)

# stack using glm
# stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
# set.seed(seed)
# stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
# print(stack.glm)



# stack using random forest
# set.seed(seed)
# stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
# print(stack.rf)


