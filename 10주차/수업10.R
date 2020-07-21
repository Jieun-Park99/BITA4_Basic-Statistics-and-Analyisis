qnorm(p = 0.025, mean=0, sd=1, lower.tail=T)
qnorm(p=0.975, mean=0, sd=1, lower.tail=T)

rnorm(100,mean=0,sd=1)

x = seq(-10,10, length=200)
plot(x,dnorm(x,mean=0,sd=1),type='l',main="Probability Density Function")

sample = seq(-5,5,length=101)
plot(sample,dnorm(sample,mean=0,sd=1),type='l',
     main="Probability Density Funciton")

height = rnorm(n=1000000,mean=175,sd=5)
hist(height, breaks=100,probability=T)

lines(density(height))

# 정규성 판정
set.seed(7777)
data = rnorm(100,mean=0,sd=1)
install.packages("moments")
library(moments)
skewness(data)
kurtosis(data)
shapiro.test(data)
agostino.test(data)
anscombe.test(data)

qqnorm(data)
qqline(data)

hist(rnorm(100,mean=0,sd=1))

hist(rnorm(100000,mean=0,sd=1))

#Example2
#1
set.seed(0529)
score=sample(1:100,40,replace=T)
mean(score)

students = NULL
students = sapply(1:2000,function(i)mean(sample(score,3,replace=T)))
hist(students)
mean(students)

students = NULL
students = sapply(1:2000,function(i)mean(sample(score,30,replace=T)))
hist(students,breaks=30)
mean(students)

hist(rpois(100,10))
hist(rpois(10000,10))

hist(rpois(100000,10))
hist(rpois(1000000,10))

hist(rbinom(30,20,0.5))
hist(rbinom(100,20,0.5))

hist(rbinom(100000,20,0.5))
hist(rbinom(100000,20,0.9))

hist(rbinom(100000,20,0.5))
hist(rbinom(100000,20,0.9))
