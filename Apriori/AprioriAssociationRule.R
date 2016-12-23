######################################
# Author : Anil Kumar Nayak          #
# Date : 25th Oct 2016               #
# Data Set : Student Performance Data#
######################################

library(arules)
library(arulesViz)

######################################
#Reading Data
######################################

data <- read.csv("student-mat.csv",sep = ";")
idx <- sample(1:nrow(data),8)
data[idx,]

######################################
# Discretizing the Data 
######################################

labels_1=c("none","primary","5th to 9th","secondary","higher")
labels_2=c("<15 min.", "15 to 30 min.", "30 min. to 1 hour", ">1 hour")
labels_3=c("<2 hours","2 to 5 hours", "5 to 10 hours", ">10 hours");
labels_4=c("Less","More")
labels_5=c("Very Bad","Bad", "Good", "Very Good","Excellent");
labels_6=c("Very Low","Low", "Average", "High","Very High");
labels_7=c("Low", "Average", "High","Excellent");

cat_1=c(0,1,2,3,4)
cat_2=c(1,2,3,4)
cat_3=c(0,2,4)
cat_5=c(1,2,3,4,5)
cat_6=c(0,5,10,15,20)

data_for_apriori = data.frame(
  school = data$school,
  sex = data$sex,
  age = discretize(data$age,method="frequency",categories = 3),
  address = data$address,
  famsize = data$famsize,
  Pstatus = data$Pstatus,
  Medu = discretize(data$Medu,method="fixed",categories = cat_1,labels=labels_1) ,
  Fedu = discretize(data$Fedu,method="fixed",categories = cat_1,labels=labels_1) , 
  Mjob = data$Mjob,
  Fjob = data$Fjob,
  reason = data$reason,
  guardian = data$guardian,
  traveltime = discretize(data$traveltime,method="fixed",categories = cat_2,labels=labels_2) ,
  studytime = discretize(data$studytime,method="fixed",categories = cat_2,labels=labels_3) ,
  failures = discretize(data$failures,method="fixed",categories = cat_3,labels=labels_4) ,
  schoolsup = data$schoolsup,
  famsup = data$famsup,
  paid = data$paid,
  activities = data$activities,
  nursery = data$nursery,
  higher = data$higher,
  internet = data$internet,
  romantic = data$romantic,
  famrel = discretize(data$famrel,method="fixed",categories = cat_5,labels=labels_5) ,
  freetime = discretize(data$freetime,method="fixed",categories = cat_5,labels=labels_6) ,
  goout = discretize(data$goout,method="fixed",categories = cat_5,labels=labels_6) ,
  Dalc = discretize(data$Dalc,method="fixed",categories = cat_5,labels=labels_6) ,
  Walc = discretize(data$Walc,method="fixed",categories = cat_5,labels=labels_6) ,
  health = discretize(data$health,method="fixed",categories = cat_5,labels=labels_6) ,
  absences = discretize(data$absences,method="frequency",categories =10) ,
  G1 = discretize(data$G1,method="fixed",categories = cat_6,labels=labels_7) ,
  G2 = discretize(data$G2,method="fixed",categories = cat_6,labels=labels_7) ,
  G3 = discretize(data$G3,method="fixed",categories = cat_6,labels=labels_7) 
)


######################################
# Data Summary
######################################

print(summary(data_for_apriori))

######################################
# 1) All rules using Apriori
# Apriori Association Rule Apply
######################################

rules.all <- apriori(data_for_apriori, parameter=list(support=0.5, confidence=0.5,minlen=2))

######################################
# Inspecting all Rules
######################################

rules.sorted <- sort(rules.all, by="support")

######################################
# 2) Show redudant rules
# Finding Duplicate Rules
######################################

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
rules.duplicate <- colSums(subset.matrix, na.rm = T) >= 1


######################################
# 3) Remove redudant rules
######################################

rules.redundantremoved <- rules.sorted[!rules.duplicate]
rules.redundant <- rules.sorted[rules.duplicate]

print("--------------------------------------")
print("-----------Redundant Rules--------------")
print("--------------------------------------")

print(as(rules.redundant, "data.frame"))

######################################
# 4) Interpret your results
######################################

print("----------------------------------------------------------------")
print("-----------TOP 10 After Removal of Redundant Rules--------------")
print("-----------------------------------------------------------------")
inspect(sort(rules.redundantremoved[1:10]))

######################################
# 5) Show graphs
######################################

#--------------------------------------------
# Generate Scatter Plot for All rules
#--------------------------------------------
# plot(rules.all)
# plot(rules.redundantremoved)

#--------------------------------------------
# Generate Graph Charts
#--------------------------------------------

# plot(rules.all, method = "grouped")

#--------------------------------------------
# All Rules Group Charts
#--------------------------------------------

# plot(rules.redundantremoved, method = "graph")
plot(rules.redundantremoved[1:20], method = "graph")

######################################
#writing all the rules to CSV files
######################################
write.csv(as(rules.redundant, "data.frame"), "redundant_rules.csv")
write.csv(as(rules.all, "data.frame"), "all_rules.csv")
write.csv(as(rules.redundantremoved, "data.frame"), "final_redundant_removed_rules.csv")


print("End")


