#########################################
# Decision Tree Classification Model    
# Author : Anil Kumar Nayak    
#
# this Program will generate a PDF file Named as LinearRegressionPlot.pdf
# where all the graphs will be attached
# along with that this will give you the summary in Console
# and generate following csv files for the better analysis
# predict_interval_pred.csv  --- prediction using interval='predict'
# confidence_interval_pred.csv  --- prediction using interval='confidence'
#########################################
library('lattice')
library('graphics')
data = read.csv("AirQualityUCI.csv",sep = ";")

#-----------------------------------------
#attributes in data those are independent
#-----------------------------------------
print(names(data))
#[1] "Date"          "Time"          "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "C6H6.GT."      "PT08.S2.NMHC." "NOx.GT."      
#[9] "PT08.S3.NOx."  "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."   "T"             "RH"            "AH"            "X"            
#[17] "X.1"		

set.seed(1)
#Devide the data into Training 
split_data = split(data,sample(1:nrow(data)>round(nrow(data)*0.7)))
training_data = split_data$"FALSE" #70 % of the data to design model

#Divide the data into Testing data
testing_data = split_data$"TRUE"  #30% to test the model

#-----------------------------------------
#Formula for linear regression
#Finding Linear Regression Model
#-----------------------------------------
formula = PT08.S3.NOx. ~ NOx.GT.
data.lm <- lm(formula, data = training_data)

#-----------------------------------------
#Summary of the  Linear Regression Model
#-----------------------------------------
print("==============================================")
print("Summary of the  Linear Regression Model")
print("==============================================")
print(summary(data.lm))

#When the all the all the independent variable coefficients are ZERO then this is allign to null hypothesis
#When at least one of the coefficient is not null the alternative hypothesis
#More the Multiple R-squared and closure to 1 -  better the model 
#And adjusted R square is very close to the R square

par(mfrow=c(2,3))
pdf('LinearRegressionPlot.pdf')

#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------

#following plot will show the graph of dependent vs independent variable 
#and the best fitted line
plot(PT08.S3.NOx. ~ NOx.GT.,data = training_data,ylab = "Dependent [PT08.S3.NOx.]",xlab="Iindependent [NOx.GT.]",main="Graph for DEPENDENT vs INDEPENDENT attribute and best fitted value for training data")
abline(data.lm,col="red")

plot(data.lm)

#-----------------------------------------
#Predict
#-----------------------------------------
#Predicting for Dependent attribute PT08.S3.NOx. from the independent attribute NOx.GT.

#Predicting by predict interval
data.lm.pred = predict(data.lm,  testing_data,interval = "predict",se.fit = TRUE)
write.csv(data.lm.pred, "predict_interval_pred.csv")

#Predicting by confidence interval
data.lm.pred.conf = predict(data.lm,  testing_data,interval = "confidence",se.fit = TRUE)
write.csv(data.lm.pred.conf, "confidence_interval_pred.csv")



# 
# lwn = length(data.lm.pred.conf$fit)/3
# 

data_fra = data.frame(
  observed = testing_data$PT08.S3.NOx.,
  predicted = data.lm.pred$fit[,1]
)

res <- stack(data.frame(Observed = testing_data$PT08.S3.NOx., Predicted = data.lm.pred$fit[,1]))
res <- cbind(res, x = rep(testing_data$PT08.S3.NOx., 2))
print(xyplot(values ~ x, data = res, type = c("p","r"), col.line = "red"))
# print(xyplot(values ~ x, data = res, group = ind ,auto.key = FALSE))
dev.off()