#Multiple Linear Regression
data = read.csv("bank-additional-full.csv",sep = ";")

#-----------------------------------------
#attributes in data those are independent
#-----------------------------------------
#age, job, marital, education, default, loan, housing, contact, 
#day_of_week, month, duration, pdays, campaign, previous, poutcome, 
#emp.var.rate , cons.price.idx , cons.conf.idx , euribor3m , nr.employed
#-----------------------------------------
#attributes in data those are dependent
#-----------------------------------------
# y

#-----------------------------------------
#Creating dummy attributes for the categorical variable 
#-----------------------------------------

data_for = data.frame(
  age = data$age,
  job = as.numeric(factor(data$job , levels=unique(data$job))),
  marital = as.numeric(factor(data$marital , levels=unique(data$marital))),
  education = as.numeric(factor(data$education , levels=unique(data$education))),
  default = as.numeric(factor(data$default , levels=unique(data$default))),
  loan = as.numeric(factor(data$loan , levels=unique(data$loan))),
  housing = as.numeric(factor(data$housing , levels=unique(data$housing))),
  contact = as.numeric(factor(data$contact , levels=unique(data$contact))),
  day_of_week = as.numeric(factor(data$day_of_week , levels=unique(data$day_of_week))),
  month = as.numeric(factor(data$month , levels=unique(data$month))),
  duration = data$duration,
  pdays = data$pdays,
  campaign = data$campaign,
  previous = data$previous,
  poutcome = as.numeric(factor(data$poutcome , levels=unique(data$poutcome))),
  emp.var.rate = data$emp.var.rate,
  cons.price.idx = data$cons.price.idx,
  cons.conf.idx = data$cons.conf.idx,
  euribor3m = data$euribor3m,
  nr.employed = data$nr.employed,
  y = as.numeric(factor(data$y, levels=unique(data$y)))
)

set.seed(1)
#Devide the data into Training 
split_data = split(data_for,sample(1:nrow(data_for)>round(nrow(data_for)*0.7)))
training_data = split_data$"FALSE" #70 % of the data to design model

#Divide the data into Testing data
testing_data = split_data$"TRUE"  #30% to test the model

#-----------------------------------------
#Formula for linear regression
#Finding Linear Regression Model
#-----------------------------------------

fit <- y ~ age+ job+ marital+ education+ default+ loan+ housing+ contact+ day_of_week+ month+ duration+ pdays+ campaign+ previous+ poutcome+ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed
data.lm <- lm(fit, data = training_data)
# data.glm <- glm(formula, family = gaussian("log"), data = data)

#-----------------------------------------
#Summary of the  Linear Regression Model
#-----------------------------------------
print("==============================================")
print("Before Removing the max or none started  p-value attribues, summary of LM is")
print("==============================================")
print(summary(data.lm))

#when the all the all the independent variable coefficients are ZERO then this is allign to null hypothesis
#when at least one of the coefficient is not null the alternative hypothesis

#for below attribute p-value > 0.5
#age+job+loan+housing+nr.employed
#for below attribute p-value < 0.5
#marital+education+default+contact+day_of_week+month+duration+pdays+campaign+previous+poutcomeemp.var.rate +cons.price.idx+cons.conf.idx +euribor3m 
#if the P-value is not the not mentioned with **s then you have to chuck them off from the model
#age+job+marital+loan+housing+poutcome+nr.employed

#More the Multiple R-squared and closure to 1 -  better the model 
# and adjusted R square is very close to the R square
#amova is < 0.5 then it will good -- Pr(>|t|)    

fit <- y ~ education+ default+ contact+ day_of_week+ month+ duration+ pdays+ campaign+ previous+ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m 
data.lm <- lm(fit, data = training_data)

print("==============================================")
print("After Removing the max or none started  p-value attribues [age,job,marital,loan,housing,poutcome,nr.employed], summary of LM is : ")
print("==============================================")
print(summary(data.lm))

print("==============================================")
print("After Removing the max or none started  p-value attribues [day_of_week , ], summary of LM is : ")
print("==============================================")

fit <- y ~ education+ default+ contact+ month+ duration+ pdays+ campaign+ previous+ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m 
data.lm <- lm(fit, data = training_data)

print(summary(data.lm))

print("==============================================")
print("Coefficients of Linear Model")
print("==============================================")

print(data.lm)

print("==============================================")
print("Predicting for Testing data")
print("==============================================")
data.pred <- predict(data.lm, newdata = testing_data)
# data.pred = fitted(data.lm)


#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
# par(mfrow=c(2,2))
# plot(data.lm)
plot(data.pred)
# plot(testing_data$cons.conf.idx)
# abline(data.pred )
# abline(a = 0, b = 1)
# plot(data$education, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$cons.conf.idx, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$job, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$emp.var.rate, pred, xlab = "Observed", ylab = "Prediction")