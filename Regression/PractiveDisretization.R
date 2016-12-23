library(arules)
data <- read.csv("student-mat.csv",sep = ";")
idx <- sample(1:nrow(data),8)
data[idx,]

labels_1=c("none","primary","5th to 9th","secondary","higher")
cat_1=c(0,1,2,3,4)


data_for_apriori = data.frame(
  Medu = discretize(data$Medu,method="fixed",categories = cat_1,labels=labels_1)
)

print(data_for_apriori)