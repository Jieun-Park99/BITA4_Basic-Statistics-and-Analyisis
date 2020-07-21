myfunc01 = function(){
  return(1)
}
myfunc01()

my_fun = function(i,j){
  k = (i+j)*2
  return(k)
}
# print는 계산 결과를 보여준다면 
# return은 계산 결과를 반환하고 함수 종료

#생성된 객체의 형식 확인
class(my_fun)

my_fun(2,3)

my_fun = function(i,j){(i+j)*2}
my_fun(2,3)


#연습문제 1
mypower01 = function(i){return(i*3)}
mypower01(3)

#연습문제 2
mypower02 = function(x,y){
  return(x^y)
}
mypower02(3,4)

input = c(1,2,3)
my_fun2 = function(obj){
  obj=obj*2
  input = obj
  return(obj)
}
my_fun2(input)
input

input = my_fun2(input)
input

### <<- 사용한 함수 밖의 객체 수정###
input = c(1,2,3)
my_fun2 = function(obj){
  obj = obj*2
  input <<- obj
  return(obj)
}
my_fun2(input)
input


#### 조건문 ####
my_fun3 = function(score){
  if(score >=80){
    "합격"
  } else {
    "불합격"
  }
}
70>=80
my_fun3(70)
90>80
my_fun3(90)


#연습문제 1
myindex = function(x){
  if(x >1){
    1
  }else{
    0
  }
}
myindex(3)
myindex(-3)

#연습문제 2
mydistance = function(a,b){
  dif=a-b
  abs(dif)
}

#### 제어문 사용해서 ####
mydistance = function(a,b){
  if(a-b>= 0){
    return(a-b)
  }else{
    return(b-a)
  }
}

mydistance(2,3)
mydistance(4,9)

my_fun4 = function(score){
  ifelse(score >= 80, "합격", "불합격")
}
my_fun4(70)
my_fun4(90)


my_fun4_1 = function(score){
  ifelse(score >= 80 , "고득점",ifelse(score>=60 & score<80,"평타","망함"))
}
my_fun4_1(70)
my_fun4_1(59)

#연습문제 
iris
attach(iris)
new = ifelse(Sepal.Length <5,"very short",
             ifelse(Sepal.Length >= 5 & Sepal.Length < 6, "short",
                    ifelse(Sepal.Length >= 6 & Sepal.Length < 7, "long",
                           ifelse(Sepal.Length >= 7, "very long","else"))))
detach(iris)
iris$Sepal.Length = new
head(iris)
str(iris)
iris$Sepal.Length = factor(iris$Sepal.Length)
levels(iris$Sepal.Length)

############### with ####################
iris$Sepal.Length = with(iris,ifelse(Sepal.Length<5,"very short",
                          ifelse(Sepal.Length>=5 & Sepal.Length<6,"short",
                                ifelse(Sepal.Length>=6 & Sepal.Length<7,"long","very long"))))
# 범주 확인 #
table(iris$Sepal.Length)

my_fun5 = function(score){
  if(score >= 80){
    "합격"
  }else if(score >=75){
    "대기"
  } else{
    "불합격"
  }
}

my_fun5(70)
my_fun5(78)
my_fun5(90)

my_fun5_1 = function(score){
  ifelse(score>=80,"합격",
         ifelse(score>= 75,"대기","불합격"))
}
my_fun5_1(78)

############# for ################
my_fun6 = function(num){
  for(x in 1:10){
    print(num+x)
  }
}
my_fun6(10)

myloop2 = function(){
  a = 0
  for( i in 3:7){
    a = a+1
  }
  return(a)
}
myloop2()

#############################
mysum = function(n){
  i = 0
  for( j in 1:n){
    i=i+j
  }
  return(i)
}
mysum(10)

mysum_1 = function(n){
  i = 0
  for( j in 1:n){
    i=i+j
    print(i)
  }
  return(i)
}
mysum_1(10)



b=c()
num=10
for(i in 1:10){
  b = c(b,num+i)
}
b

n = 3
m = 1:9
for(i in m){
  times = n*i
  print(paste(n , "X", i , "=", times))
  }

#############################
#연습문제 1
myeven = function(x){
  a = 0
  for(i in 1:x){
    if(i%%2 == 0){
      a=a+i
    }else{
      next
    }
  }
  return(a)
}
myeven(10)


#연습문제 2
x = c(1,2,3,NA,4,5,NA)
myplus=function(vec){
  d=0
  for(i in 1:length(vec)){
    if(is.na(vec[i])){
      next
    }else{
      d= d+vec[i]
    }
  }
  return(d)
}
myplus(x)

# !is.na는 결과가 반대로 나옴. NA인게 FALSE로 

##############################

x=0
y=0
while(y<=10){
  x=x+y
  y=y+1
  print(x);print(y)
}
x;y

rm(list=ls())
v_vector = c(1,2,3,4,5)
v_df = data.frame(v_vector)
v_df
ls()
getwd()
save(v_vector,v_df,file="save_test.rdata")
rm(list=ls())
load("save_test.rdata")
v_vector


#################### 데이터 불러오기 ############

getwd()
ex_csv_df = read.table(file="exam.csv",header=T,sep=",",stringsAsFactors = F)
str(ex_csv_df)
ex_csv_df

ex_csv_df = read.csv("C:/Users/danlo/OneDrive/문서/exam.csv")
str(ex_csv_df)

write.table(ex_csv_df,file="test_file_save.csv",sep=",",col.names=T,row.names=F,append=F)
write.csv(ex_csv_df,"test_file_save.csv",row.names=F)
write.table(ex_csv_df,file="test_file_save.csv",sep=",",col.names=F,append=T)

str_detect(c("hello bebe","oh happy day"),"b")
library("stringr")
install.packages("stringr")
library("stringr")
getwd()
