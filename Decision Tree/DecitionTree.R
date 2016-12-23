#########################################
# Decision Tree Classification Model    
# Author : Anil Kumar Nayak    
#
# this Program will generate a PDF file Named as DecisionTree.pdf
# where all the graphs will be attached
# along with that this will give you the summary and Complexity details in Console
#########################################

#installing new packages if required
if(!("rpart.plot" %in% rownames(installed.packages()))){
  install.packages('rpart.plot')
}

if(!("rpart" %in% rownames(installed.packages()))){
  install.packages('rpart')
}

if(!("tree" %in% rownames(installed.packages()))){
  install.packages('tree')
}

if(!("party" %in% rownames(installed.packages()))){
  install.packages('party')
}

#Importing Library
library(rpart)
library(rpart.plot)
library(tree)
library(party)
#Reading Data Set
data = read.table("car.data",sep = ",")

names(data)[1] <- "buying"
names(data)[2] <- "maint"
names(data)[3] <- "doors"
names(data)[4] <- "persons"
names(data)[5] <- "lug_boot"
names(data)[6] <- "safety"
names(data)[7] <- "class"

#-----------------------------------------
#attributes in data those are independent
#-----------------------------------------
# | attributes
# 
# buying:   vhigh, high, med, low.
# maint:    vhigh, high, med, low.
# doors:    2, 3, 4, 5more.
# persons:  2, 4, more.
# lug_boot: small, med, big.
# safety:   low, med, high.
#-----------------------------------------
#attributes in data those are dependent
#-----------------------------------------
# | class values
# 
# unacc, acc, good, vgood

set.seed(1)
#Devide the data into Training 
split_data = split(data,sample(1:nrow(data)>round(nrow(data)*0.7)))
training_data = split_data$"FALSE" #70 % of the data to design model

#Divide the data into Testing data
testing_data = split_data$"TRUE"  #30% to test the model

#Design formula for decision tree
formula <- class ~ buying + maint + doors + persons + lug_boot + safety

#Decision tree model
decision_tree <- rpart(formula, data = training_data)

#Writing all the graph into a pdf file
pdf('DecisionTree.pdf',width = 25,height = 15)

#Ploting Decision tree using rpart library
rpart.plot(decision_tree,extra = 101)

#Predicting the classification of testing data
pred = predict(decision_tree,testing_data,type="class")

#Printing Prediction Table
print(table(pred,testing_data$class))

summary(decision_tree)

plot(decision_tree, uniform=TRUE, main="Classification Tree for Cars")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)


dev.off()
graphics.off()