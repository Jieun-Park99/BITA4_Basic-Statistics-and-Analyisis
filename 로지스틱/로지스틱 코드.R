## -----Chapter 1 
## ----start-----------------
# Install packages from CRAN
p_needed <- c("Epi", "ROCR", "lme4", "gee","geepack","VGAM","brglm", 
              "icda","MCMCpack","coin","DescTools","boot","BradleyTerry2","dplyr", 
              "glmnet")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

#install.packages("icda",repos="http://www.stat.ufl.edu/~presnell/R",type="source")

lapply(p_needed, require, character.only = TRUE)
load("glm_data.RData")

## ---- binom-ex -----------------------

dbinom(0:10,10,.2) ## P(Y=0), P(Y=1),...,P(Y=10)

plot(0:10, dbinom(0:10, 10, .2), type = "h",
     xlab = "y", ylab = "P(y)")

## ---- lik-ex
y<-8; n<-10

theta<- seq(0,1,len=100)
like <- dbinom(y,n,theta)
like <- like/max(like)

plot(theta,like,type='l',xlab=expression(theta),
     ylab='Likelihood', main="Likelihood function when y=8")

y<-0; n<-10

theta<- seq(0,1,len=100)
like <- dbinom(y,n,theta)
like <- like/max(like)

plot(theta,like,type='l',xlab=expression(theta),
     ylab='Likelihood', main="Likelihood function when y=0")

## ---- lik-ex2 ----------


par(mfrow=c(1,1))
x<-2; n<-10

theta<- seq(0.0,1,len=100)
like <- dbinom(x,10,theta)
like<- like/max(like)

plot(theta,like,type='n',xlab=expression(theta),
     ylab='Likelihood')
lines(theta,like,lwd=.3)
title(expression('Likelihood functions'))

xx<- c(0,5,10)
for (i in 1:3){
  x<- xx[i]
  like <- dbinom(x,10,theta)
  like<- like/max(like)
  lines(theta,like,lwd=.3)
}

## ---- prop-test ---------

x<-0; n<-10
prop.test(x, n, correct=FALSE)

## ---- prop-test2 ---------
library(prevalence)
propCI(x=x, n=n)

## ---- LRT-1 -----
# computing likelihood intervals at certain alpha levels
li<- function(th,like,alpha=0.15){
  that <- mean(th[like==max(like)])
  lowth <- th[th < that]  
  lowlik <- like[th < that] 
  if (length(lowth)<2) {lowval <- min(th) }
  if (length(lowth)>1)
  {  lowval <- approx(lowlik,lowth,xout=alpha)$y}
  
  upth <- th[th > that]
  if (length(upth)<2 ) {return(c(lowval,max(th))) }
  if (length(upth)> 1){
    uplik <- like[th > that]  
    upval <- approx(uplik,upth,xout=alpha)$y
    return(c(lowval,upval))
  }
}

## ---- LRT-2---------

th<- seq(0,1,len=100)
x<- 0
n<- 10
like <- dbinom(x,n,th)
like<-  like/max(like)
a<- exp(-0.5*qchisq(0.95,1))

plot(th,like,xlab=expression(theta),
     ylab='Likelihood',type='l')
abline(h=a,lwd=.3)
cat('cutoff=',a, 'Interval=',li(th,like,a),'\n')
title(expression('Likelihood intervals'))



## ---- Chapter 2  ------


## ----OR-ex ------ 

tab<-matrix(c(60,41,682,692),2,2)
tab
n1<-rowSums(tab)[1]; n2<-rowSums(tab)[2]
n11<-tab[1,1];n21<-tab[2,1];n12<-tab[1,2];n22<-tab[2,2]

p1<-n11/n1; p2<-n21/n2

odds.1<- p1/(1-p1); odds.2<- p2/(1-p2)
odds.1
odds.2 
OR<- odds.1/odds.2
OR

## ----OR-ex2 -------

logOR<-log(OR)
SE<-sqrt(1/n11+1/n12+1/n21+1/n22)
L<-logOR-1.96*SE
U<-logOR+1.96*SE
c(L,U) ## CI for logOR
c(exp(L),exp(U)) ## CI for OR

## ----OR-ex3 -------

fit<-glm(tab~c(1,0),family=binomial())
exp(confint(fit)) 


## ----party1 ---------

party <- matrix(c(762,327,468,
                  484,239,477),ncol=3,byrow=TRUE)
colnames(party) <- c("Democrat","Independent","Repubican")
rownames(party) <- c("Females","Males")
party <- as.table(party)
party


## ----party2 ---------
XTest<-chisq.test(party, correct=FALSE)

XTest$observed
XTest$expected




## ----party3 ---------
XTest ## Pearshon chi-squared test 

library(DescTools)
GTest(party) ## LR test


## ----alcohol-1 ---------

alcohol<- matrix(c(17066,14464,788,126,37,
                   48,38,5,1,1),5,2)
colnames(alcohol)<-c("Absent","Present")
rownames(alcohol)<-c("A","B","C","D","E")
alcohol
prop.table(alcohol,1)*100

## ----alcohol-2 ---------
chisq.test(alcohol,correct=FALSE)

##2*J or I*2 table
prop.trend.test(alcohol[,2],rowSums(alcohol),score=c(0,0.5,1.5,4,7))

## ----tea ----------

TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Truth = c("Milk", "Tea"),
                         Guess = c("Milk", "Tea")))
## H0: OR=1 vs H1: OR>1
fisher.test(TeaTasting, alternative = "greater")


## ---- dp-1 -------------
library(catdata)
data(deathpenalty)
dp <- xtabs(Freq ~ VictimRace + DefendantRace + DeathPenalty,
            data=deathpenalty)
dpflat <- ftable(DeathPenalty ~ VictimRace + DefendantRace,
                 data=dp)
dpflat


## ---- dp-2 -------------
round(100*prop.table(dpflat,1), 1)

## ---- dp-3 ---------------
ftable(DeathPenalty ~ DefendantRace, data=dp)
round(prop.table(ftable(DeathPenalty ~ DefendantRace,
                        data=dp),1),2)
