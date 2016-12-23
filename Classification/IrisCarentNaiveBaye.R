library(caret)
library(psych)
library(e1071)
library(class)


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

data = data[,c("mcg","gvh","aac","alm1","alm2","class")]

# data(iris)
x = data[,-6]
y = data$class

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
