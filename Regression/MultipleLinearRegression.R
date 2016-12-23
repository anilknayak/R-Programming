#########################################
# Decision Tree Classification Model    
# Author : Anil Kumar Nayak    
#
# this Program will generate a PDF file Named as LinearRegression.pdf
# where all the graphs will be attached
# along with that this will give you the summary in Console
# and generate following csv files for the better analysis
# predict_interval_pred.csv  --- prediction using interval='predict'
# confidence_interval_pred.csv  --- prediction using interval='confidence'
#########################################
library('lattice')
library('graphics')

auto.data = read.table("auto-mpg.data")

names(auto.data)[1] <- "mpg"
names(auto.data)[2] <- "cylinders"
names(auto.data)[3] <- "displacement"
names(auto.data)[4] <- "horsepower"
names(auto.data)[5] <- "weight"
names(auto.data)[6] <- "acceleration"
names(auto.data)[7] <- "modelyear"
names(auto.data)[8] <- "origin"
names(auto.data)[9] <- "car name"

# par(mfrow=c(3,3))
pdf('MultipleLinearRegression.pdf',width = 15,height = 10)
#Devide the data into Training 70% and Testing 30%
set.seed(1)
split_data = split(auto.data,sample(1:nrow(auto.data)>round(nrow(auto.data)*0.7)))
training_data = split_data$"FALSE"
testing_data = split_data$"TRUE"

#-----------------------------------------
#Formula for linear regression
#Finding Linear Regression Model
#-----------------------------------------
formula = mpg ~ displacement + acceleration + cylinders + weight + modelyear + origin
auto.data.mlm <- lm(formula, data = training_data)

#-----------------------------------------
#Summary before removal of insignificant attributes for Multiple Linear Regression Model
#-----------------------------------------
print(summary(auto.data.mlm))


#-----------------------------------------
#New Formula for linear regression
#Finding Linear Regression Model
#-----------------------------------------
formula = mpg ~ weight + modelyear + origin
auto.data.mlm <- lm(formula, data = training_data)

#-----------------------------------------
#Summary of the  Linear Regression Model
#-----------------------------------------
print(summary(auto.data.mlm))


#When the all the all the independent variable coefficients are ZERO then this is allign to null hypothesis
#When at least one of the coefficient is not null the alternative hypothesis
#More the Multiple R-squared and closure to 1 -  better the model 
#And adjusted R square is very close to the R square

#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
plot(formula,data = training_data,main="Training data")
# abline(auto.data.mlm,col="red")



#-----------------------------------------
#Plotting Fitting values for Linear Regression Model
#-----------------------------------------
auto.data.mlm.fit = fitted(auto.data.mlm)
print(summary(auto.data.mlm.fit))

plot(auto.data.mlm.fit,training_data$weight,main="Fig 1: Fitted graph for Weight in Multiple Linear Regression")
plot(auto.data.mlm.fit,training_data$origin,main="Fig 2: Fitted graph for origin in Multiple Linear Regression")
plot(auto.data.mlm.fit,training_data$modelyear,main="Fig 3: Fitted graph for modelyear in Multiple Linear Regression")


# -----------------------------------------
#Plotting Residual values for Linear Regression Model
#-----------------------------------------
auto.data.mlm.residual = residuals(auto.data.mlm)
print(summary(auto.data.mlm.residual))

plot(auto.data.mlm.residual,training_data$weight,main="Fig 4: Residual for mpg vs weight on training data")
plot(auto.data.mlm.residual,training_data$modelyear,main="Fig 5: Residual for mpg vs modelyear on training data")

#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
plot(auto.data.mlm)


#-----------------------------------------
#Predicting Testing data on  Linear Regression Model
#-----------------------------------------
auto.data.mlm.pred = predict(auto.data.mlm,  testing_data,interval = "predict",se.fit = TRUE)
write.csv(auto.data.mlm.pred, "predict_interval_pred.csv")

auto.data.mlm.conf = predict(auto.data.mlm,  testing_data,interval = "confidence",se.fit = TRUE)
write.csv(auto.data.mlm.conf, "confidence_interval_pred.csv")


auto.data.pred.v.obsr = data.frame(
  observed = testing_data$mpg,
  predicted = auto.data.mlm.conf$fit[,1]
)

#-----------------------------------------
#Plotting Obseved and Predicted values on Linear Regression Model
#-----------------------------------------
plot(auto.data.pred.v.obsr$observed,testing_data$weight,ylab="mpg",xlab="Displacement",main="Figure 6: predicted vs observed, on Testing Data for weight independent variable",col="red")
par(new=TRUE)
plot(auto.data.pred.v.obsr$predicted,testing_data$weight,ylab="mpg",xlab="Displacement",col="green",lwd=1.5)
par(new=TRUE)
legend('topright',c("Observed","Predicted"),pch = 1,col=c("red","green"))

plot(auto.data.pred.v.obsr$observed,testing_data$origin,ylab="mpg",xlab="Displacement",main="Figure 4: predicted vs observed, on Testing Data for origin independent variable",col="red")
par(new=TRUE)
plot(auto.data.pred.v.obsr$predicted,testing_data$origin,ylab="mpg",xlab="Displacement",col="green",lwd=1.5)
par(new=TRUE)
legend('topleft',c("Observed","Predicted"),pch = 1,col=c("red","green"))

plot(auto.data.pred.v.obsr$observed,testing_data$modelyear,ylab="mpg",xlab="Displacement",main="Figure 4: predicted vs observed, on Testing Data for model year independent variable",col="red")
par(new=TRUE)
plot(auto.data.pred.v.obsr$predicted,testing_data$modelyear,ylab="mpg",xlab="Displacement",col="green",lwd=1.5)
par(new=TRUE)
legend('topleft',c("Observed","Predicted"),pch = 1,col=c("red","green"))


dev.off()
graphics.off()