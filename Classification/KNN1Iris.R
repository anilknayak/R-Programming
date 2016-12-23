data(iris)
str(iris)
table(iris$Species)
head(iris)
set.seed(9850)
gp = runif(nrow(iris))
iris = iris[order(gp),]
summary(iris[,c(1,2,3,4)])
#feature having larger value, this will lead to influencing the result
#do normalization

normalize = function(x){
  return( (x-min(x))/(max(x)-min(x)))
}

iris_n = as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))

summary(iris_n)

iris_training = iris_n[1:129,]
iris_testing = iris_n[130:150,]
iris_training_target = iris[1:129,5]
iris_testing_target = iris[130:150,5]


require(class)

#feed to function
#training and test data frame and training target and value of K

#how many nearest neighbour to find a sqrt of total rows


ml = knn(train=iris_training,test=iris_testing,cl=iris_training_target,k=13)
# this will store a prediction of all classification of testing data frame

print(ml)

print(table(iris_testing_target,ml))
