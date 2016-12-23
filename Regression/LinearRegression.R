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
names(auto.data)[7] <- "model year"
names(auto.data)[8] <- "origin"
names(auto.data)[9] <- "car name"

par(mfrow=c(3,3))
pdf('LinearRegression.pdf',width = 15,height = 10)
#Devide the data into Training 70% and Testing 30%
set.seed(1)
split_data = split(auto.data,sample(1:nrow(auto.data)>round(nrow(auto.data)*0.7)))
training_data = split_data$"FALSE"
testing_data = split_data$"TRUE"

#-----------------------------------------
#Formula for linear regression
#Finding Linear Regression Model
#-----------------------------------------
formula = mpg ~ displacement
auto.data.lm <- lm(formula, data = training_data)

#-----------------------------------------
#Summary of the  Linear Regression Model
#-----------------------------------------
print(summary(auto.data.lm))

#When the all the all the independent variable coefficients are ZERO then this is allign to null hypothesis
#When at least one of the coefficient is not null the alternative hypothesis
#More the Multiple R-squared and closure to 1 -  better the model 
#And adjusted R square is very close to the R square

#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
plot(mpg ~ displacement,data = training_data,main="Figure 1: mpg vs displacement and the fit line in Red on Training Data")
abline(auto.data.lm,col="red")

#-----------------------------------------
#Plotting Fitting values for Linear Regression Model
#-----------------------------------------
auto.data.fit = fitted(auto.data.lm)
print(summary(auto.data.fit))

plot(auto.data.fit,training_data$displacement,main="Figure 2: fitted(mpg) vs displacement, as compared to the fitted red line in the figure 1 on Training Data")

#-----------------------------------------
#Plotting Residual values for Linear Regression Model
#-----------------------------------------
auto.data.residual = resid(auto.data.lm)
print(summary(auto.data.residual))

plot(auto.data.residual,training_data$displacement,main="Figure 3: residual(mpg) vs displacement, as compared to the fitted red line in the figure 1 on Training Data")

#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
plot(auto.data.lm)


#-----------------------------------------
#Predicting Testing data on  Linear Regression Model
#-----------------------------------------
auto.data.lm.pred = predict(auto.data.lm,  testing_data,interval = "predict",se.fit = TRUE)
write.csv(auto.data.lm.pred, "predict_interval_pred.csv")

auto.data.lm.conf = predict(auto.data.lm,  testing_data,interval = "confidence",se.fit = TRUE)
write.csv(auto.data.lm.conf, "confidence_interval_pred.csv")

auto.data.pred.v.obsr = data.frame(
  observed = testing_data$mpg,
  predicted = auto.data.lm.conf$fit[,1]
)

#-----------------------------------------
#Plotting Obseved and Predicted values on Linear Regression Model
#-----------------------------------------
plot(auto.data.pred.v.obsr$observed,testing_data$displacement,ylab="mpg",xlab="Displacement",main="Figure 4: predicted vs observed, on Testing Data",col="red")
par(new=TRUE)
plot(auto.data.pred.v.obsr$predicted,testing_data$displacement,ylab="mpg",xlab="Displacement",col="green",lwd=1.5)
par(new=TRUE)
legend('topright',c("Observed","Predicted"),pch = 1,col=c("red","green"))

dev.off()
graphics.off()