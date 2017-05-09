require(lattice)
require(ggplot2)
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

data$mcg = as.numeric(data$mcg)
data$gvh = as.numeric(data$gvh)
data$lip = as.numeric(data$lip)
data$chg = as.numeric(data$chg)
data$aac = as.numeric(data$aac)
data$alm1 = as.numeric(data$alm1)
data$alm2 = as.numeric(data$alm2)
data$class = as.numeric(data$class)
data = data[,c("mcg","gvh","aac","alm1","alm2","class")]

data$class = ifelse(data$class == 'cp',1,0)


table(data$class)
set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]

col = c("mcg","gvh","aac","alm1","alm2")

#Devide the data into Training 
rows = floor(nrow(data)*0.7)
x = data[1:rows,col]
testing_data = data[rows:nrow(data),col]
y = data[1:rows,c("class")]
testing_output = data[rows:nrow(data),c("class")]

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
