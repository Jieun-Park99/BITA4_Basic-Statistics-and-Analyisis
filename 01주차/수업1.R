"Hello R"
plot(airquality$Ozone)
round(10.2345,1)
round(10.2345,1) 반올림 처리하는 함수 
round(10.2345,1) #반올림을 처리하는 함수 
#반올림 처리하는 함수
round(10.2345,1)
ROUND(10.2345,1)
Round(10.2345,1)
round(10.2345,1
      
      
      
      )
print(love)
love=1
print(love)
love="안녕하세요"
love

love<-print
love("이제 나는 함수가 되었다!")

love_num=1
love_str="안녕하세요"
love_vec=c(1,1,1,1)
love_fun=print

str(love_num)
str(love_str)
str(love_vec)
str(love_fun)

A=1
B=A
A ; B

A=9
A ; B

vec_t=c(1,2,3,4)
vec_t
str(vec_t)
length(vec_t)

vec_T=c(1,"hi",2)
vec_T
str(vec_t)

numeric_vector=c(0.2,-1,2,-0.5)
mode(numeric_vector)

min(numeric_vector)
max(numeric_vector)
mean(numeric_vector)
median(numeric_vector)
sum(numeric_vector)

numeric_vector2=c(1/0,2/2,-2/2,-1/0,0/0)
numeric_vector2

ex_logical1=c(TRUE,FALSE,TRUE,FALSE)
ex_logical1
mode(ex_logical1)

ex_logical2=c(T,F,T,F)
ex_logical2
mode(ex_logical2)

ex_logical3=c(true,false,true,false); ex_logical3
ex_logical4=c("TRUE","FALSE","TRUE","FALSE"); ex_logical4
mode(ex_logical4)

ex_logical=c(TRUE,T,FALSE,F)
ex_logical
!ex_logical #역변환

ex_logical=as.logical(c(0,-1,1,100,-7)); ex_logical
as.numeric(ex_logical)

V_character=c("문자열","문자열2","A","1")
V_character
mode(V_character)

nchar(c("F123","F124","F125","F1227"))
substr("1234567",2,4)
substr(c("F123","F124","F125","F1227"),2,4)
a=strsplit('2014/11/22',split="/");a
a[[1]][1]


paste("50=","30+","20")
paste("50=","30+","20",sep="")
paste("50","30","20",sep="*")
toupper("AbCdEfGhIjKlMn")
tolower("AbCdEfGhIjKlMn")


V_character=c("사과","복숭아","사과","오렌지","사과","오렌지","복숭아")
V_factor=factor(V_character);V_factor
mode(V_factor)
str(V_factor)


V_num=c(1000,2000,1000,2000,3000,2000,3000)
V_num_factor=factor(V_num);V_num_factor
V_char=as.character(V_num_factor);V_char
V_num=as.numeric(V_char);V_num

V_character=c("사과","복숭아","사과","오렌지","사과","오렌지","복숭아")
V_factor=factor(V_character,levels=c("사과","복숭아"));V_factor
V_factor=factor(V_character,levels=c("복숭아","오렌지","사과"));V_factor

ex_label=c("하하","중하","중","중상","상상")
ordered_factor=factor(ex_label,ordered=T); ordered_factor
factor(ex_label,levels=c("하하","중하","중","중상","상상"),ordered=T)

table(mtcars$cyl)
table(mtcars$cyl,mtcars$gear)
table(mtcars$cyl,mtcars$gear,dnn=c("Cylinder","Gear"))

################ BREAK TIME #############

t_vector=c(11,12,13,14,15,16,17,18,19,20)
t_vector
t_vector[3]

idx=c(1,3,5,6)
t_vector[idx]
t_vector[c(1,3,5,6)]
t_vector[c(6,5,3,1)]

seq_vector=11:20
seq_vector
t_vector
seq_vector=51:100
seq_vector
seq_vector[30:40]

log_vector=11:15
log_idx=c(F,F,T,F,F)
log_vector[log_idx] #true인 것만!
log_vector[log_idx==F]
log_vector[!log_idx]

log_vector>13
log_vector[log_vector>13]
log_vector[log_vector>=13]
log_vector[log_vector<=13 & log_vector>=13]


?seq
seq(from=10,to=20,by=2)
seq(10,20,2)
seq(20,10,-2)

rep(0,10)
rep(c(0,1),10)

#which함수
which(t_vector==11)
t_vector[1]

wh_vec=rep(c(0:5),10)
which(wh_vec==4)

m_vector=1:5
m_vector[3]=999
m_vector

m_vector=1:5
m_vector[seq(1,5,2)]=0
m_vector[seq(2,5,2)]=1
m_vector

m_vector=1:5
m_vector[3]=10
m_vector
m_vector=10
m_vector

m_vector=1:5
length(m_vector)
m_vector[1:length(m_vector)]=10
m_vector

m_vector=1:5
add_vector=c(999,m_vector);add_vector
add_vector=c(m_vector,999);add_vector
add_vector=c(m_vector,100:102);add_vector

c(m_vector,add_vector)


vec_a=c("가","나","마","바")
vec_b=c("다","라")
vec_a;vec_b
append(vec_a,vec_b,2)

t_vector=11:20
t_vector[c(1,3,5,6)]
t_vector[-c(1,3,5,6)]
t_vector[-length(t_vector)]

#논리형에서 제거
log_var=c(F,T,T,F,F)
t_vector[log_var] #갯수만큼 반복

a=c(1,2,3,4);b=c(5,6,7,8)
a+b;a*b
b=2:3;a+b
b=2:4;a+b


#sampling
?sample
dice=sample(c(1:6),1);dice
sample(c(1:6),7,T)

sample(c(1:10),2)
sample(c(1:10),2)
sample(c(1:10),2)

set.seed(0804)
sample(c(1:10),2)
set.seed(0804)
sample(c(1:10),2)

set.seed(0921)
coin=sample(c("앞","뒤"),100,replace=T);table(coin)
coin=sample(c("앞","뒤"),100,replace=T);table(coin)


table(coin)
coin=sample(c("앞","뒤"),100,replace=T)
table(coin)
coin=sample(c("앞","뒤"),100,replace=T)
table(coin)


na.vector=c(1,2,NA,4,5,NA)
is.na(na.vector)
sum(is.na(na.vector)) #T는1 F는 0

sum(na.vector)
omit.vetor=na.omit(na.vector)
sum(omit.vetor)
sum(na.vector,na.rm=T)

na.vector[is.na(na.vector)]=999
na.vector

install.packages("rmarkdown")
install.packages("knitr")

---
  title: "markdown"
author: "박지은"
date: "2019년 8월 9일"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
```{r}
t_vector=c(11,12,13,14,15,16,17,18,19,20)
t_vector
t_vector[3]
```
