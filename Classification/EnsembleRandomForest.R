packages_required = c('randomForest','ggplot2','rpart')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
  
}
require(ggplot2)
library(randomForest)
library(rpart)

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

#suffling the data
table(data$class)
set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]

#Devide the data into Training 
rows = floor(nrow(data)*0.7)
training_data = data[1:rows,]
testing_data = data[rows:nrow(data),]


rf_model = randomForest(class ~ .,data=training_data,importance=TRUE,proximity=TRUE,na.action=na.omit)

print(rf_model)

print(rf_model$confusion)
print(rf_model$votes)
print(rf_model$importance)
print(round(importance(rf_model), 2))
print(rf_model$proximity)

MDSplot(rf_model,training_data$class)

pred = predict(rf_model,testing_data)

print(table(pred,testing_data$class))
print(mean(pred==testing_data$class))

getTree(rf_model)
plot(rf_model)
#Rpart Decision Tree
dt_model = rpart(class ~.,data=training_data,control=rpart.control(10))
plot(dt_model,margin = FALSE,branch = 1)
text(dt_model)
print(dt_model$variable.importance)

pred_dt = predict(dt_model,testing_data,type="class")
print("Decision Tree Confusion Matrix")
print(table(pred_dt,testing_data$class))
print("Random Forest Confusion Matrix")
print(table(pred,testing_data$class))

print("Decision Tree Accuracy")
print(mean(pred_dt==testing_data$class))
print("Random Forest Accuracy")
print(mean(pred==testing_data$class))

