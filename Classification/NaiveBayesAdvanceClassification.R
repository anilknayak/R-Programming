library(e1071)
library(chron)
library(rminer)

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

data = data[,c(2,3,4,5,6,7,8,9)]

table(data$class)
set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]

#Devide the data into Training 
rows = floor(nrow(data)*0.7)
data_training = data[1:rows,]
data_testing = data[rows:nrow(data),]
data_training_target = data[1:rows,c("class")]
data_testing_target = data[rows:nrow(data),c("class")]

formula = class ~ mcg+gvh+aac+alm1+alm2
# Computes the conditional a-posterior probabilities of a
# categorical class variable given independent
# predictor variables using the Bayes rule
naiveBayesModel = naiveBayes(formula=formula,data=data_training)
pred = predict(naiveBayesModel, data_testing, type = "class")
print(table(pred, data_testing$class))

naiveBayesModelLaplace = naiveBayes(formula=formula,data=data_training,laplace = 3)
pred_lap = predict(naiveBayesModelLaplace, data_testing)
print(table(pred_lap, data_testing$class))

print(mmetric(data_testing$class,pred_lap,c("ACC","PRECISION","TRP","F1")))

#kappa is very pessimistic
#skewed , but the disritbution is at mean

plot(naiveBayesModelLaplace)
