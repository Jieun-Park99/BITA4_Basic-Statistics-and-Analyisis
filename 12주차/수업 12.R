year = 1971:1982
Y = c(5.6,3.2,4.5,4.2,5.2,2.7,4.8,4.9,4.7,4.1,4.4,5.4)
X = c(116.37,82.77, 110.68,97.5,115.88,80.19,125.24,116.15,117.36,93.31,107.46,122.3)
data = data.frame(year,Y,X)
data
plot(data$X, data$Y,xlab = "열매개수", ylab="수확량",main = "수확량과 열매개수의 산점도",col="red")

lm(cars$dist~cars$speed)

library(MASS)
data(cats)
str(cats)
summary(cats)
plot(cats)

model1 = lm(cats$Hwt~cats$Bwt,data=cats)
model1
par(mfrow=c(2,2))
plot(model1)
summary(model1)

anova(model1)

states = as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
head(states)

par(mfrow=c(2,2))
plot(Murder~.,data=states)

model2 = lm(Murder~Population + Illiteracy+Income+Frost, data=states)
model2 = lm(Murder~., data=states)
plot(model2)
summary(model2)

model3 = lm(Murder~Population +Illiteracy, data=states)
summary(model3)
