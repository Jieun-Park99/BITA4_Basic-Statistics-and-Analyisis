lm_result = lm(formula = dist~speed, data=cars)
par(mfrow=c(2,2))
plot(lm_result)
