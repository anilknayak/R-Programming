#######################################
#Multiple Linear Regression Model     #
#Author : Anil Kumar Nayak            #
#######################################
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

data.lm <- lm(PT08.S2.NMHC. ~ NMHC.GT. + C6H6.GT., data = training_data)

print(summary(data.lm))


#Predicting by predict interval
data.lm.pred = predict(data.lm,  testing_data,interval = "predict",se.fit = TRUE)
write.csv(data.lm.pred, "predict_interval_pred_m.csv")

#Predicting by confidence interval
data.lm.pred.conf = predict(data.lm,  testing_data,interval = "confidence",se.fit = TRUE)
write.csv(data.lm.pred.conf, "confidence_interval_pred_m.csv")


par(mfrow=c(2,2))
plot(data.lm)



