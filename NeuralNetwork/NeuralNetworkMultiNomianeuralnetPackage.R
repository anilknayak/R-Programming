packages_required = c('neuralnet')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(neuralnet)

data = read.csv("student-mat.csv",sep=";")

data$sex = as.numeric(data$sex)
data$address = as.numeric(data$address)
data$school = as.numeric(data$school)
data$Pstatus = as.numeric(data$Pstatus)
data$Mjob = as.numeric(data$Mjob)
data$Fjob = as.numeric(data$Fjob)
data$reason = as.numeric(data$reason)

data$schoolsup = as.numeric(data$schoolsup)
data$famsup = as.numeric(data$famsup)
data$activities = as.numeric(data$activities)
data$nursery = as.numeric(data$nursery)
data$higher = as.numeric(data$higher)
data$internet = as.numeric(data$internet)
data$romantic = as.numeric(data$romantic)
data$higher = as.numeric(data$higher)

#sse - sum square error
#ce - cross entropy
error_calc_func = "sse"
threshold_limit = 0.01
#most of the time the hidden layer is like 3 or 2 or 1
hidden_layer = 3
formula = school ~ age+G1+G2+G3+studytime+traveltime
# 'backprop', 'rprop+', 'rprop-', 'sag', or 'slr'. 
# 'backprop' refers to backpropagation, 
# 'rprop+' and 'rprop-' refer to the resilient backpropagation with and without weight backtracking, 
# while 'sag' and 'slr' induce the usage of the modified globally convergent algorithm (grprop). 
algorithm_used = 'backprop'
repetation = 1
learningrate  = 1
# stepmax = 1
# differentiable function that is used for smoothing the result of the cross product of the covariate or neurons and the weights. 
# Additionally the strings, 
# 'logistic' and 'tanh' are possible for the logistic function and tangent hyperbolicus.
activation_function = 'logistic'

# Classification - False
# Regression - True
linear_output = FALSE  

neuralnet = neuralnet(formula = formula, 
                      data=data,
                      hidden = hidden_layer,
                      linear.output = linear_output,
                      err.fct = error_calc_func,
                      act.fct = activation_function,
                      rep = repetation)

# print(neuralnet$call)
#input Variables
# print(neuralnet$covariate)
print(neuralnet$model.list)
#Predicted Variables between 0-1 : outcome of logistic function
# print(neuralnet$net.result[[1]])
#50% 
neuralnet1 = ifelse(neuralnet$net.result[[1]]>0.5,1)
print(neuralnet1)
misclassificationError = mean(data$school!=neuralnet1)
print(misclassificationError)
# print(neuralnet$net.result[[1]])
# print(neuralnet$weights)
# print(neuralnet$result.matrix)
# print(neuralnet$startweights)

plot(neuralnet)

