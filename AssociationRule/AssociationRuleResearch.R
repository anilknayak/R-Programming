######################################
# Author : Anil Kumar Nayak          #
# Date : 25th Oct 2016               #
# Data Set : Wholesale Customer Data #
######################################

library(arules)
library(arulesViz)

######################################
#Reading Data
######################################

#Set the path First to read and write to output files

data <- read.csv("student-mat.csv",sep = ";")
idx <- sample(1:nrow(data),8)
data[idx,]

######################################
# Data Cleaning
######################################


######################################
#Transforming Data Column
######################################
# 1 school - student's school (binary: "GP" - Gabriel Pereira or "MS" - Mousinho da Silveira) == NONE
# 2 sex - student's sex (binary: "F" - female or "M" - male)
# 3 age - student's age (numeric: from 15 to 22)
data <- transform(data, age = ifelse(age <= 15, '0-15',
                                     ifelse(age > 15 | age <= 20, '15-30', '')))
# 4 address - student's home address type (binary: "U" - urban or "R" - rural)
# 5 famsize - family size (binary: "LE3" - less or equal to 3 or "GT3" - greater than 3)
# 6 Pstatus - parent's cohabitation status (binary: "T" - living together or "A" - apart)
# 7 Medu - mother's education (numeric: 0 - none,  1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)
data <- transform(data, Medu = ifelse(Medu == 0, 'none',
                                      ifelse(Medu == 1, 'primary',
                                             ifelse(Medu  == 2, '5th to 9th',
                                                    ifelse(Medu  == 3, 'secondary',
                                                           ifelse(Medu  == 4, 'higher', ''))))))
# 8 Fedu - father's education (numeric: 0 - none,  1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)
data <- transform(data, Fedu = ifelse(Fedu == 0, 'none',
                                      ifelse(Fedu  == 1, 'primary',
                                             ifelse(Fedu  == 2, '5th to 9th',
                                                    ifelse(Fedu  == 3, 'secondary',
                                                           ifelse(Fedu  == 4, 'higher', ''))))))
# 9 Mjob - mother's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")
# 10 Fjob - father's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")
# 11 reason - reason to choose this school (nominal: close to "home", school "reputation", "course" preference or "other")
# 12 guardian - student's guardian (nominal: "mother", "father" or "other")
# 13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)
data <- transform(data, traveltime = ifelse(traveltime == 1, '<15',
                                            ifelse(traveltime  == 2, '15-30',
                                                   ifelse(traveltime  == 3, '30-60',
                                                          ifelse(traveltime  == 4, '>1','')))))
# 14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)
data <- transform(data, studytime = ifelse(studytime == 1, '<2',
                                           ifelse(studytime  == 2, '2-5',
                                                  ifelse(studytime  == 3, '5-10',
                                                         ifelse(studytime  == 4, '>10','')))))
# 15 failures - number of past class failures (numeric: n if 1<=n<3, else 4)
data <- transform(data, failures = ifelse(failures <= 2, 'Less',
                                          ifelse(failures > 2 | failures <= 4, 'More','')))
# 16 schoolsup - extra educational support (binary: yes or no)
# 17 famsup - family educational support (binary: yes or no)
# 18 paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)
# 19 activities - extra-curricular activities (binary: yes or no)
# 20 nursery - attended nursery school (binary: yes or no)
# 21 higher - wants to take higher education (binary: yes or no)
# 22 internet - Internet access at home (binary: yes or no)
# 23 romantic - with a romantic relationship (binary: yes or no)
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
data <- transform(data, famrel = ifelse(famrel == 1, 'Very Bad',
                                        ifelse(famrel  == 2, 'Bad',
                                               ifelse(famrel  == 3, 'Good',
                                                      ifelse(famrel  == 4, 'Very Good',
                                                             ifelse(famrel  == 5, 'Excellent',''))))))
# 25 freetime - free time after school (numeric: from 1 - very low to 5 - very high)
data <- transform(data, freetime = ifelse(freetime == 1, 'Very Low',
                                          ifelse(freetime  == 2, 'Low',
                                                 ifelse(freetime  == 3, 'Average',
                                                        ifelse(freetime  == 4, 'High',
                                                               ifelse(freetime  == 5, 'Very High',''))))))
# 26 goout - going out with friends (numeric: from 1 - very low to 5 - very high)
data <- transform(data, goout = ifelse(goout == 1, 'Very Low',
                                       ifelse(goout  == 2, 'Low',
                                              ifelse(goout  == 3, 'Average',
                                                     ifelse(goout  == 4, 'High',
                                                            ifelse(goout  == 5, 'Very High',''))))))
# 27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
data <- transform(data, Dalc = ifelse(Dalc == 1, 'Very Low',
                                      ifelse(Dalc  == 2, 'Low',
                                             ifelse(Dalc  == 3, 'Average',
                                                    ifelse(Dalc  == 4, 'High',
                                                           ifelse(Dalc  == 5, 'Very High',''))))))
# 28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
data <- transform(data, Walc = ifelse(Walc == 1, 'Very Low',
                                      ifelse(Walc  == 2, 'Low',
                                             ifelse(Walc  == 3, 'Average',
                                                    ifelse(Walc  == 4, 'High',
                                                           ifelse(Walc  == 5, 'Very High',''))))))
# 29 health - current health status (numeric: from 1 - very bad to 5 - very good)
data <- transform(data, health = ifelse(health == 1, 'Very Bad',
                                        ifelse(health  == 2, 'Bad',
                                               ifelse(health  == 3, 'Average',
                                                      ifelse(health  == 4, 'Good',
                                                             ifelse(health  == 5, 'Very Good',''))))))
# 30 absences - number of school absences (numeric: from 0 to 93)
data <- transform(data, absences = ifelse(absences < 5, 'Very Good',
                                          ifelse(absences >= 5 | absences < 10, 'Good',
                                                 ifelse(absences >= 10 | absences < 20, 'Average',
                                                        ifelse(absences >= 20 | absences < 30, 'Poor',
                                                               ifelse(absences >= 30, 'Very Poor',''))))))

# 31 G1 - first period grade (numeric: from 0 to 20)
data <- transform(data, G1 = ifelse(G1 == 20 , 'Excellent',
                                    ifelse(G1 >= 17  | absences < 20, 'Very good',
                                           ifelse(G1 >= 14 | absences < 17, 'Good',
                                                  ifelse(G1 >= 10 | absences < 14, 'Poor',
                                                         ifelse(G1 < 10, 'Very Poor',''))))))
# 31 G2 - second period grade (numeric: from 0 to 20)
data <- transform(data, G2 = ifelse(G2 == 20 , 'Excellent',
                                    ifelse(G2 >= 17  | absences < 20, 'Very good',
                                           ifelse(G2 >= 14 | absences < 17, 'Good',
                                                  ifelse(G2 >= 10 | absences < 14, 'Poor',
                                                         ifelse(G2 < 10, 'Very Poor',''))))))
# 32 G3 - final grade (numeric: from 0 to 20, output target)
data <- transform(data, G3 = ifelse(G3 == 20 , 'Excellent',
                                    ifelse(G3 >= 17  | absences < 20, 'Very good',
                                           ifelse(G3 >= 14 | absences < 17, 'Good',
                                                  ifelse(G3 >= 10 | absences < 14, 'Poor',
                                                         ifelse(G3 < 10, 'Very Poor',''))))))


######################################
# Peforming Selection of Data
######################################
data_for_apriori = data.frame(
  school = data$school,
  sex = data$sex,
  age = data$age,
  address = data$address,
  famsize = data$famsize,
  Pstatus = data$Pstatus,
  Medu = data$Medu,
  Fedu = data$Fedu,
  Mjob = data$Mjob,
  Fjob = data$Fjob,
  reason = data$reason,
  guardian = data$guardian,
  traveltime = data$traveltime,
  studytime = data$studytime,
  failures = data$failures,
  schoolsup = data$schoolsup,
  famsup = data$famsup,
  paid = data$paid,
  activities = data$activities,
  nursery = data$nursery,
  higher = data$higher,
  internet = data$internet,
  romantic = data$romantic,
  famrel = data$famrel,
  freetime = data$freetime,
  goout = data$goout,
  Dalc = data$Dalc,
  Walc = data$Walc,
  health = data$health,
  absences = data$absences,
  G1 = data$G1,
  G2 = data$G2,
  G3 = data$G3
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

rules.sorted <- sort(rules.all, by="lift")

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

plot(rules.redundantremoved[1:20], method = "graph")


# plot(rules.inspected, measure=c("support","lift"), shading="confidence")

######################################
#writing all the rules to CSV files
######################################
write.csv(as(rules.redundant, "data.frame"), "C:\\Users\\MyLife\\Dropbox\\R Programs\\AssociationRule\\redundant_rules.csv")
write.csv(as(rules.all, "data.frame"), "C:\\Users\\MyLife\\Dropbox\\R Programs\\AssociationRule\\all_rules.csv")
write.csv(as(rules.redundantremoved, "data.frame"), "C:\\Users\\MyLife\\Dropbox\\R Programs\\AssociationRule\\final_redundant_removed_rules.csv")


print("End")


