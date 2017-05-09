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
# C5.0
set.seed(7)
c50_model <- train(formula, data=data, method="C5.0", metric="Accuracy", trControl=control,preProc = c("center", "scale"))

predictors(c50_model)

c50_model$finalModel$tuneValue

print(summary(c50_model$finalModel))

# Stochastic Gradient Boosting
set.seed(7)
gbm_model <- train(formula, data=data, method="gbm", metric="Accuracy", trControl=control, verbose=FALSE,preProc = c("center", "scale"))

predictors(gbm_model)

gbm_model$finalModel$tuneValue

print(summary(gbm_model$finalModel))

# Stochastic Gradient Boosting
set.seed(7)
rf_model <- train(formula, data=data, method="rf", metric="Accuracy", trControl=control, verbose=FALSE,preProc = c("center", "scale"))

predictors(rf_model)

rf_model$finalModel$tuneValue

print(summary(rf_model$finalModel))

# summarize results
boosting_results <- resamples(list(c5.0=c50_model, gbm=gbm_model,rf=rf_model))
summary(boosting_results)
dotplot(boosting_results)
