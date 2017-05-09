packages_required = c('e1071')

# ftp://cran.r-project.org/pub/R/web/packages/e1071/vignettes/svmdoc.pdf
# http://www.svm-tutorial.com/2014/10/support-vector-regression-r/
#   http://rischanlab.github.io/SVM.html
# https://www.r-bloggers.com/learning-kernels-svm/
#   https://www.youtube.com/watch?v=eHsErlPJWUU&list=PLqS2sO7F2t3XKyCABKHxWO_x56X_DGRc-
  

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(e1071)

data = read.table("ecoli.data")
pdf("SVMLinearlySeparable.pdf")
# plot(data)
s <- sample(336,270)
col <- c("V9","V3","V7")
t1 = data[s,col]
t2 = data[-s,col]

svmfit = svm(V9~.,data=t1[,col],kernel="linear", cost=0.1)
print(svmfit)
plot(svmfit,t1[,col])

print(svmfit$index)

tuned <- tune(svm,V9~.,data=t1[,col],kernel="linear", range=list(cost=c(0.001,0.01,0.1,1,10,100)))
print(summary(tuned))

svmfit_tuned = svm(V9~.,data=t1[,col],kernel="linear", cost=tuned$best.parameters$cost)
print(svmfit_tuned)
plot(svmfit_tuned,t1[,col])

dev.off()
graphics.off()