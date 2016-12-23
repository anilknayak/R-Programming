require(lattice)
require(ggplot2)
library(caret)

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

set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = data$class,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

#Checking distibution in origanl data and partitioned data
prop.table(table(data$class)) * 100
prop.table(table(training$class)) * 100
prop.table(table(testing$class)) * 100

training = training[,c(2,3,4,5,6,7,8,9)]
testing = testing[,c(2,3,4,5,6,7,8,9)]
# kNN requires variables to be normalized or scaled. 
# caret provides facility to preprocess data. I am going to choose centring and scaling
trainX <- training[,c(2,3,4,5,6,7,8)]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#,classProbs=TRUE,summaryFunction = twoClassSummary)
#Formula
formula = class ~ mcg+gvh+aac+alm1+alm2+lip
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(formula, data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
pdf("KnnClassificationModelApiCaret.pdf")
summary(knnFit)
print(knnFit)
plot(knnFit)
knnPredict <- predict(knnFit,newdata = testing )
print(confusionMatrix(knnPredict, testing$class ))

print(mean(knnPredict == testing$class))
dev.off()
graphics.off()

