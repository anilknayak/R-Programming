#Generalized Linear Regression
data("bodyfat", package="TH.data")
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.glm <- glm(myFormula, family = gaussian("log"), data = bodyfat)
print(summary(bodyfat.glm))

pred <- predict(bodyfat.glm, type = "response")
plot(bodyfat$DEXfat, pred, xlab = "Observed", ylab = "Prediction")
abline(a = 0, b = 1)