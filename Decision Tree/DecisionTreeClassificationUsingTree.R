#installing new packages if required
if(!("tree" %in% rownames(installed.packages()))){
  install.packages('tree')
}

library(tree)
library(rpart.plot)
#Reading Data Set
data = read.table("car.data",sep = ",")

names(data)[1] <- "buying"
names(data)[2] <- "maint"
names(data)[3] <- "doors"
names(data)[4] <- "persons"
names(data)[5] <- "lug_boot"
names(data)[6] <- "safety"
names(data)[7] <- "class"

High = data$class

set.seed(2)
train = sample(1:nrow(data),nrow(data)/2)
test = -train
training_data = data[train,]
testing_data = data[test,]
high_data = High[test]

tree_model = tree(class ~ . , training_data)
plot(tree_model)
text(tree_model,all = TRUE,splits = TRUE,pretty = 0)


tree_pred = predict(tree_model,testing_data,type = "class")
print(mean(tree_pred != high_data))

#pruning tree
set.seed(3)
cv_tree = cv.tree(tree_model,FUN= prune.misclass)
names(cv_tree)


plot(cv_tree$size,cv_tree$dev,title("ABS"),type = "b")



#prune tree
prune_model = prune.misclass(tree_model,best=14)

plot(prune_model)
text(tree_model,all = TRUE,splits = TRUE,pretty = 0)


tree_pred_pruned = predict(prune_model,testing_data,type="class")
print(mean(tree_pred != high_data))