sales = c(45,32,39,29,64,55,38,212,187,124,320,188)
qqnorm(sales)
qqline(sales)

shapiro.test(sales)

x = c(11.2,11.0,12.0,13.0,12.5,10.5,5.8,6.2,14.0,12.2)
sign.test= function(median,y){
  z = sort(y)
  n=length(z)
  b=sum(z>median)
  pbinom(b-1,n,0.5,lower.tail=F)
}
sign.test(10,x)

t.test(x,alternative="two.sided")

a = c(750,850,600,550,560,640,800,640)
b = c(850,840,625,535,620,670,820,680)
d=b-a
T=length(d[d>0])
binom.test(T, length(d[d!=0]), alternative="greater")

t.test(a,b,alternative = "two.sided", paired=T)


A = c(156,183,120,113,138,145,142)
B = c(109,107,119,162,121,123,76,111,130,115)

R = 9999
all = c(A,B)
k = 1:length(all)
reps = numeric(R)
ts = mean(A) - mean(B)
ts

for (i in 1:R) {
  #크기 A만큼의 sample을 뽑아 A에 저장한다.
  m = sample(k, size=length(A),replace=F)
  a1 = all[m]
  b1 = all[-m]
  reps[i] = mean(a1) - mean(b1)
}
pvalue = mean(c(ts,reps) >= ts)
pvalue

A = c(156,183,120,113,138,145,142)
B = c(109,107,119,162,121,123,76,111,130,115)
install.packages("exactRankTests")
library(exactRankTests)

wilcox.exact(A, B, paired=F, alternative="greater")

x = c(23.3,26.1,19.0,28.8,29.0)
x
bootsample1 = sample(x, size=length(x), replace=TRUE)
bootsample1
bootsample2 = sample(x, size=length(x),replace=T)
bootsample2
bootsample3 = sample(x, size=length(x), replace=T)
bootsample3
