packages_required = c('neuralnet')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(neuralnet)

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

# ?infert
# dim(infert)
# ?neuralnet
data$class = as.numeric(data$class)

#sse - sum square error
#ce - cross entropy
error_calc_func = "sse"
threshold_limit = 0.01
#most of the time the hidden layer is like 3 or 2 or 1
hidden_layer = 2
formula = class ~ mcg+gvh+lip+chg+aac+alm1+alm2
# 'backprop', 'rprop+', 'rprop-', 'sag', or 'slr'. 
# 'backprop' refers to backpropagation, 
# 'rprop+' and 'rprop-' refer to the resilient backpropagation with and without weight backtracking, 
# while 'sag' and 'slr' induce the usage of the modified globally convergent algorithm (grprop). 
algorithm_used = 'rprop+'
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
                      rep = repetation,
                      lifesign="full")

# print(neuralnet$call)
#input Variables
# print(neuralnet$covariate)
# print(neuralnet$model.list)
#Predicted Variables between 0-1 : outcome of logistic function
print(neuralnet$net.result)
#50% 
# neuralnet1 = ifelse(neuralnet$net.result[[1]]>0.5,data$class)
# neuralnet1
# misclassificationError = mean(data$class!=neuralnet1)
# print(misclassificationError)
# print(neuralnet$net.result[[1]])
# print(neuralnet$weights)
# print(neuralnet$result.matrix)
# print(neuralnet$startweights)

plot(neuralnet)

