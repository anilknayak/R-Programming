#Car Example

library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
residualPlots(reg1)