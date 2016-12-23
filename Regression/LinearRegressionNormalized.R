##############################
#Linear Regression Model     #
#Author : Anil Kumar Nayak   #
##############################
library(clusterSim)
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


for(i in 1:length(training_data)){
  training_data[i]=data.Normalization(training_data[i],type="n1",normalization="column")
  testing_data[i]=data.Normalization(testing_data[i],type="n1",normalization="column")
}

fit <- y ~ education+ default+ contact+ month+ duration+ pdays+ campaign+ previous+ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m 
data.lm <- lm(fit, data = training_data)
print(summary(data.lm))