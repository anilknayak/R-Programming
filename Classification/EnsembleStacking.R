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

# data$class = ifelse(data$class == 'cp',1,0)

formula = class ~ mcg + gvh + aac + alm1 + alm2 

#'glm',,'lda',
# create submodels
control <- trainControl(method="boot", number=10, repeats=3,preProc = c("center", "scale"),index=createResample(data$class, 25),classProbs = TRUE)
# control_stacking <- trainControl(method="boot", number=10, repeats=3, savePredictions='final', classProbs=TRUE,preProc = c("center", "scale"))
algorithmList <- c('lda','rf','rpart','knn','svmRadial','C5.0','gbm','nnet')
set.seed(7)
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