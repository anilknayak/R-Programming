library(nnet)
library(caret)
library(Metrics)
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
#Devide the data into Training 
rows = floor(nrow(data)*0.7)
data_training = data[1:rows,]
data_testing = data[rows:nrow(data),]
data_training_target = data[1:rows,c("class")]
data_testing_target = data[rows:nrow(data),c("class")]

# data_training1 <- cbind(data_training[, 2:8], class.ind(data_training$class))

# cp im imL imS imU om omL pp
formula = class ~ mcg+gvh+lip+chg+aac+alm1+alm2

model <- multinom(formula, data=data_training, maxit=500, trace=T)

mostImportantVariables <- varImp(model)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(mostImportantVariables)


pred <- predict(model, type="class", newdata=data_testing)

postResample(data_testing$class,pred)

#Prediction Table
print(table(data_testing$class,pred))

#Classification Error 
classificationError <- ce(as.numeric(data_testing$class), as.numeric(pred))
print(classificationError)
