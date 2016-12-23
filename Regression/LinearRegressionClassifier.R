##############################
#Linear Regression Model     #
#Author : Anil Kumar Nayak   #
##############################
library(arules)
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

print(summary(data))

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




#-----------------------------------------
#Formula for linear regression
#-----------------------------------------
#age+ job+ marital+ education+ default+ loan+ housing+ contact+ day_of_week+ month+ duration+ pdays+ campaign+ previous+ poutcome+ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed
# formula <- y ~job

#-----------------------------------------
#Finding Linear Regression Model
#-----------------------------------------
# data.glm <- glm(formula, family = gaussian("log"), data = data)

# data.lm <- lm(formula, data = data)

#-----------------------------------------
#Finding summary of Linear Regression Model
#-----------------------------------------
# print(summary(data.lm))
# print(residuals(data.lm)[1:10])
# 
# pred <- predict(data.lm, type = "response")
#-----------------------------------------
#Plotting Linear Regression Model
#-----------------------------------------
# par(mfrow=c(2,3))
# plot(data$age, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$education, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$cons.conf.idx, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$job, pred, xlab = "Observed", ylab = "Prediction")
# abline(a = 0, b = 1)
# plot(data$emp.var.rate, pred, xlab = "Observed", ylab = "Prediction")


