library(class)
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

s <- sample(336,270)
t1 = data[s,]
t2 = data[-s,]
train.def <- data$class[s]
test.def <- data$class[-s]

knn.1 <-  knn(t1, t2, train.def, k=1)
knn.5 <-  knn(t1, t2, train.def, k=5)
knn.20 <- knn(t1, t2, train.def, k=20)