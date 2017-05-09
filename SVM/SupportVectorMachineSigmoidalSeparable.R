packages_required = c('e1071')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(e1071)

data = read.table("ecoli.data")
pdf("SupportVectorMachineSigmoidalSeparable.pdf")
names(data)[1] <- "Sequence"
names(data)[2] <- "mcg"
names(data)[3] <- "gvh"
names(data)[4] <- "lip"
names(data)[5] <- "chg"
names(data)[6] <- "aac"
names(data)[7] <- "alm1"
names(data)[8] <- "alm2"
names(data)[9] <- "class"

# plot(data)
s <- sample(336,270)
col <- c("class","gvh","alm1")
t1 = data[s,col]
t2 = data[-s,col]

svmfit = svm(class~.,data=t1[,col],kernel="sigmoid", cost=0.1)
print(svmfit)
plot(svmfit,t1[,col],main="SVM Before Tuning")

print(svmfit$index)

tuned <- tune(svm,class~.,data=t1[,col],kernel="sigmoid", range=list(cost=c(0.001,0.01,0.1,1,10,100)))
print(summary(tuned))

svmfit_tuned = svm(class~.,data=t1[,col],kernel="sigmoid", cost=tuned$best.parameters$cost)
print(svmfit_tuned)
plot(svmfit_tuned,t1[,col],main="SVM Before Tuning")


print(summary(svmfit_tuned))
print(svmfit_tuned$index)

best.model = svmfit_tuned$best.model

svm.predict <- predict(svmfit_tuned,t2[,col],type="class") 
plot(svm.predict,main="Predicted Value for Testing Data")
#confusion Matrix
print(table(svm.predict,t2[,1]))
#accuracy
accuracy = mean(svm.predict==t2[,3])

print(accuracy)
dev.off()
graphics.off()