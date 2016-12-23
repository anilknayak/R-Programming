packages_required = c('e1071')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(e1071)

data(iris)
plot(iris)

plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)

s <- sample(150,100)
col <- c("Petal.Length","Petal.Width","Species")
iris_train = iris[s,col]
iris_test = iris[-s,col]

#linear / polynomial / sigmoid / radial 
svmfit = svm(Species~.,data=iris_train,kernel="radial", cost=.1,scale=FALSE)
#cost is to validate against tune
print(svmfit)
plot(svmfit,iris_train[,col])

print(svmfit$index)

tuned <- tune(svm,Species~.,data=iris_train,kernel="linear", range=list(cost=c(0.001,0.01,0.1,1,10,100)))
print(summary(tuned))

svmfit_tuned = svm(Species~.,data=iris_train,kernel="linear", cost=tuned$best.parameters$cost,scale=FALSE)
print(svmfit_tuned)
plot(svmfit_tuned,iris_train[,col])

print(summary(svmfit_tuned))
print(svmfit_tuned$index)

best.model = svmfit_tuned$best.model

svm.predict <- predict(svmfit_tuned,iris_test[,col],type="class") 
plot(svm.predict)
table(svm.predict,iris_test[,3])
#accuracy
accuracy = mean(svm.predict==iris_test[,3])

print(accuracy)
