require(class)


data = read.table('newthyroid.dat',skip=10,sep=",")
names(data) = c('T3resin', 'Thyroxin', 'Triiodothyronine', 'Thyroidstimulating', 'TSH_value','class')

as = c('Thyroxin', 'Triiodothyronine')

#suffling the data
table(data$class)
set.seed(9850)
gp = runif(nrow(data))
data = data[order(gp),]

#summary of the data
print("summary before normalization")
print(summary(data))

#feature having larger value or smaller values, 
#this will lead to influencing the result
#do normalization 
normalize = function(x){
  return( (x-min(x))/(max(x)-min(x)))
}



#normalized data set
data_n = as.data.frame(lapply(data[,c(1,2,3,4,5)],normalize))

#summary of Normalized Data
print("summary after normalization")
print(summary(data_n))


#Devide the data into Training 
rows = floor(nrow(data_n)*0.7)
data_n_training = data_n[1:rows,]
data_n_testing = data_n[rows:nrow(data_n),]
data_n_training_target = data[1:rows,c("class")]
data_n_testing_target = data[rows:nrow(data_n),c("class")]

#how many nearest neighbour to find a sqrt of total rows
knn_k = c(1,2,5,10,floor(sqrt(nrow(data_n))))
for (i in knn_k) {
  knn_th = knn(train=data_n_training,test=data_n_testing,cl=data_n_training_target,k=i)
  print(table(data_n_testing_target,knn_th))
  knn_corr_class = 100 * sum(data_n_testing_target == knn_th)/100
  print(paste("K = ",i," correctly classifies",knn_corr_class,"% of the outcomes"))
  str=sprintf("Predicted Default by k %d Nearest Neighbors",i)
  print(str)
  plot(data_n_training[,as],col=c(4,3,6,2)[data_n_training_target],
       pch=c(1,2)[as.numeric(data_n_training_target)], main=str,cex.main=.95)
  
  points(data_n_training[,as], bg=c(4,3,6,2)[data_n_training_target],
         pch=c(21,24)[as.numeric(knn_th)],cex=1.2,col=grey(.7))
  
  legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1), legend=c("data 0","pred 0","data 1","pred 1"),
         title="default",bty="n",cex=.9)
  
  legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),
         title="Class", horiz=TRUE,bty="n",col=grey(.7),cex=.9)
}

