packages_required = c('klaR','caret','rpart','MASS')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
  
}
library(caret)
library(rpart)
library(klaR)
library(MASS)

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

#suffling the data
set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]

trainIndex <- createDataPartition(data$class, p=0.7, list=FALSE)
training_data <- data[ trainIndex,]
testing_data <- data[-trainIndex,]


# train a naive bayes model
model <- NaiveBayes(class~., data=training_data)

predictions <- predict(model, testing_data[,1:5])

confusionMatrix(predictions$class, testing_data[,6])

predictions$posterior

# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(class~., data=training_data, trControl=train_control, method="nb")
# summarize results
print(model)


