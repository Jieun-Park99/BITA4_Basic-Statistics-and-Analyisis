storeA = c(15,25,70,100)
mean(storeA)

J = c(1,1,100,100)
H = c(30,40,40,50)
mean(J)
mean(H)

storeA = c(15,25,70,100)
median(storeA)

storeB = c(10,20,50,70,100)
median(storeB)

nums = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
quantile(nums)

boxNums = c(21,22,23,24,25)
quantile(boxNums)
boxplot(boxNums)

boxNums_1 = c(16,21,22,23,24,25,30)
quantile(boxNums_1)
boxplot(boxNums_1)

boxplot(storeB)
storeB = storeB[storeB<600]
boxplot(storeB)

storeA 
storeB
boxplot(storeA,storeB,names=c("A식당","B식당"))

boxplot(storeA,storeB,names=c("A식당","B식당"))
points(c(mean(storeA),mean(storeB)),pch=2,col="red",cex=2)


points(c(mean(storeA),mean(storeB)),pch=1,col=1,cex=3)

boxplot(storeA,storeB,names=c("A삭당","B식당"))
points(c(mean(storeA),mean(storeB)),pch=1,col=1,cex=3)

storeB=c(5,6,11,13,15,16,20,20,21,23,22,27,27,30,30,32,36,37,37,40,40,43,44,45,51,54,70)
mean(storeB)
quantile(storeB)

storeC=c(5,5,5,12,10,11,20,20,20,20,20,21,20,30,32,31,31,31,36,40,40,51,61,51,61,61,70)
mean(storeC)
quantile(storeC)

boxplot(storeB, storeC,names=c("B식당","C식당"))

bpdat=c(1,50,50,50,100,100,100,150)
boxplot(bpdat)

boxNums=c(16,21,22,23,24,25,30)
boxplot(boxNums)
boxplot(boxNums,range=2) 
#여기서 range가 1.5배를 수정할 수 있는 옵션임


iris
boxplot(iris$Sepal.Length~iris$Species)

boxplot(storeA)$stats 
#뒤에 이걸 하면 상자그림과 통계치를 한 번에 보여줌 
quantile(storeA)

nums=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
hist(nums,main='숫자의분포',xlab='숫자',ylab='분포')

nums = c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17)
hist(nums,4)
hist(nums,6)
hist(nums,breaks=c(0,2,9,15,17))
#### histogram ####


var(storeB)
var(storeC)
sd(storeB)
sd(storeC)


bloodType=c('A','B','A','AB','O','A','O','B','A','O','O','B','O','A','AB','B','O','A','A','B')
length(bloodType)
table(bloodType)
model_table = table(bloodType)
prop.table(model_table) # 비율 계산

table_bloodType=table(bloodType)
table_bloodType
names(table_bloodType)
sum(table_bloodType)

addmargins(table(bloodType))

pie(x=table(bloodType))
pie(x=table(bloodType)
    ,labels=c("A형","AB형","B형","O형")
    ,col=c("chocolate1","chartreuse2","darkgoldenrod1","darkorchid1")
    ,lty=2
    ,main="이슬반 혈액형 분포")

colors()

pie(x=table(bloodType),col=rainbow(4))

table(bloodType)
barplot(table(bloodType))
barplot(table(bloodType),
        names.arg=c("A형","AB형","B형","O형"),
        main="현우반의 혈액형 분포",
        xlab="혈액형",
        ylab="학생수",
        col=heat.colors(4))

name = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20')
gender = c("남",'남','여','남','남','여','여','남','남','여','남','여','남','남','여','여','남','남','남','여')

classDf=data.frame(name,gender,bloodType)
str(classDf)
head(classDf[,c(2,3)])
table(classDf[,c(2,3)])
addmargins(table(classDf[,c(2,3)]))

classTable = table(classDf[,c(2,3)])
classTable
barplot(classTable)
barplot(classTable,legend=TRUE)
barplot(classTable,legend=TRUE,ylim=c(0,8))
barplot(classTable,legend=TRUE,ylim=c(0,9),
        col=c("skyblue","lightpink"))
barplot(classTable,legend=TRUE,ylim=c(0,8),
        col=c("skyblue","lightpink"),
        beside=T)
