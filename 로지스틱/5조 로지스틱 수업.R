data(iris)
d = subset(iris, Species == "setosa" | Species == "versicolor")
str(d)
d$Species = factor(d$Species)
str(d)
(model = glm(Species~. , data=d, family = binomial))
fitted(model)[c(1:5, 51:55)]
f = fitted(model)
as.numeric(d$Species)
ifelse(f > 0.5, 1, 0) == as.numeric(d$Species) - 1

is_correct = (ifelse(f > 0.5, 1, 0) == as.numeric(d$Species) - 1)
sum(is_correct)
sum(is_correct)/NROW(is_correct)
predict(model, newdata = d[c(1,10,55), ], type="response")

library(nnet)
(m = multinom(Species~., data=iris))
head(fitted(m))
apply(fitted(m), 1, max)
a = apply(fitted(m), 1, max)
ifelse(a == 1, "setosa", ifelse(a == 2, "versicolor", "virginica"))
predict(m)

predict(m, newdata = iris[c(1,51,101), ], type="class")
predict(m, newdata = iris[c(1, 50, 101), ], type = "probs")
predicted = predict(m, newdata=iris)
sum(predicted == iris$Species)/NROW(iris)
xtabs(~predicted + iris$Species)


library(car)
str(Chile)

chile_data = na.omit(Chile)
chile_data$vote[chile_data$vote != 'Y'] = 'N'
chile_data$vote = factor(chile_data$vote)
head(chile_data)

chile_data_num = nrow(chile_data)
train_chile = sample(1:chile_data_num) < (chile_data_num*0.8)
test_chile = chile_data[!train_chile, ]; head(test_chile)
train_chile = chile_data[train_chile,];head(train_chile)

outcome = glm(vote~. , family = binomial(), data=train_chile); summary(outcome)

predict_data = predict(outcome, newdata=test_chile, type="response") ; predict_data


# 요약 #
outcome = glm(vote~., family=binomial(), data=train_chile) ; summary(outcome)
predict_data = predict(outcome, newdata=test_chile,type="response");predict_data

install.packages("ROCR")
library(ROCR)
cm = prediction(predict_data, test_chile$vote)
pfrf = performance(cm, measure="tpr", x.measure="fpr")
plot(pfrf,main='ROC Curve')

auc = performance(cm, measure="auc")
auc
