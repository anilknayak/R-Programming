packages_required = c('psych','e1071','caret','class','lattice','ggplot2','klaR','MASS')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(lattice)
library(ggplot2)
library(klaR)
library(MASS)
library(psych)
library(e1071)
library(caret)
library(class)


data = read.delim('hayes-roth.dat',skip=9,sep = ",")
names(data) <- c("Hobby","Age","EducationalLevel","MaritalStatus","Class")

data$Class <- factor(data$Class)

gp <- runif(nrow(data))
data <- data[order(gp),]

training_data=data[1:120, -5]
training_data_output=data[1:120, 5]

testing_data=data[121:159, -5]
testing_data_output=data[121:159, 5]

pdf("NaiveBayesCaretLibrary.pdf")

naiveBayesModel <- train(training_data,training_data_output,'nb',trControl = trainControl(method = 'cv',number = 10))

print(naiveBayesModel)
print(naiveBayesModel$finalModel)

pred = predict(naiveBayesModel$finalModel,training_data)
#Prediction on Training Data
print(table(pred$class, training_data_output))
print(mean(training_data_output==pred$class))

training_data$Class = training_data_output
naive_model <- NaiveBayes(Class~., data = training_data)

pred_nm = predict(naive_model,testing_data)
#Prediction on Testing Data
print(table(pred_nm$class,testing_data_output))
print(mean(training_data_output==pred$class))

plot(naive_model)

dev.off()
graphics.off()



