#######2.4.1########
id=c(1:4)
name=c("갑","을","병","정")
age=c(22,34,30,28)
isMarried=c(F,T,F,T)
df=data.frame(id,name,age,isMarried)
df

data.frame(c(1:4),c("갑","을","병","정"),c(22,34,30,28),c(F,T,F,T))
data.frame(id=c(1:4),name=c("갑","을","병","정"),age=c(22,34,30,28),isMarried=c(F,T,F,T))
str(df)

df=data.frame(id,name,age,isMarried,stringsAsFactors = F)
str(df)

#########2.4.2#########
df
df[2,3]
df[c(1,2),c(3,4)]
df[,c(3,4)]
df[,c("id","name")]
df$name
df$name[2]
df$name[c(1,3)]

########2.4.3#########
iris
str(iris)
str(iris$Sepal.Length)
head(iris)
head(iris,10)
tail(iris)
levels(iris$Species)

nrow(iris)
ncol(iris)
table(iris$Species)
summary(iris)
min(iris$Sepal.Length) ; max(iris$Sepal.Length)
median(iris$Sepal.Length)
mean(iris$Sepal.Length) ; quantile(iris$Sepal.Length)

############2.4.4###########
View(iris)
iris

############2.4.4.2###########
subset(iris,Sepal.Length > 7)
subset(iris,Sepal.Length > 7 & Petal.Length < 6.5)
subset(iris,Sepal.Length  > 7 | Petal.Length > 6.5)
subset(iris,Sepal.Length > max(Sepal.Length)-1)
subset(iris,Sepal.Length > 5.1 & Species == "setosa")
subset(iris,Sepal.Length > 7 & Petal.Length<6.5, select=c(1,5))
subset(iris,Sepal.Length > 7 & Petal.Length < 6.5,select=c(Sepal.Length,Species))
subset(iris,Sepal.Length > 7 & Petal.Length < 6.5,select=c(5,1))
subset(iris,Sepal.Length > 7 & Petal.Length < 6.5,select=-c(1,5))
subset(iris,Sepal.Length > 7 & Petal.Length < 6.5,select=-c(Sepal.Length,Species))

############2.4.4.3#############
iris[iris$Sepal.Length > 7 & iris$Pepal.Length <= 6.5]
iris[iris$Sepal.Length > 7 & iris$Petal.Length <= 6.5,]
iris[iris$Sepal.Length > 7 & iris$Petal.Length <= 6.5,][,c(1,5)]

Sepal.Length[1:30]
attach(iris)
Sepal.Length[1:30]
iris[Sepal.Length > 7 & Petal.Length <= 6.5,]
detach(iris)
iris[Sepal.Length > 7 & Petal.Length <= 6.5,]

head(iris[,c("Sepal.Length")])
head(iris[,c("Sepal.Length"),drop=F])
str(iris[,c("Sepal.Length"),drop=F])

df
which(df==T)
which(df==T,arr.ind = T)

###########2.4.4.4############
ex_df=data.frame(name=c("이정민","김영석","고광민","최지은"),age=c(24,24,23,23),sex=c("남","남","남","여"),stringsAsFactors=F)
str(ex_df)
order(ex_df$age)
ex_df[order(ex_df$age),]
ex_df[order(ex_df$age,decreasing=T),]
ex_df[order(ex_df$age,ex_df$sex,decreasing=T),]

###########2.4.5############
ex_df[3,3]="여"
ex_df
wh_idx=which(ex_df=="여",arr.ind=T)
wh_idx
ex_df[wh_idx]="남"
ex_df
summary(iris)
attach(iris)

###########2.4.6#############
data(iris)
str(iris)
iris_new=iris
iris_new$new=rep(1,150)
iris_new
iris_new$new="최지은"
head(iris_new)
head(iris)

iris_new=iris_new[,-6]
head(iris_new)
iris_new=iris_new[,-1]
head(iris_new,3)
iris_new <- iris_new[,-1]
head(iris_new,3)

iris_odidx=order(iris$Sepal.Length,decreasing = T)
iris_od=iris[iris_odidx,]
head(iris_od)

iris_od$rank=1:nrow(iris)
head(iris_od)

iris_new=iris
colnames(iris_new)
colnames(iris_new)=c("a","b","c","d","e")
colnames(iris_new)
head(iris_new)
colnames(iris_new)[3]="3rd"
head(iris_new)

##########2.4.6.2#############
df=data.frame(id,name,age,isMarried)
str(df)

df$name=as.character(df$name)
str(df$name)
sum(df$age)

df$age=as.character(df$age)
sum(df$age)

############2.4.7############
ex_df=data.frame(name=c("이정민","김영석","고광민","최지은"),age=c(24,24,23,23),sex=c("남","남","남","여"),stringsAsFactors=F)
ex_df2=data.frame(name=c("김영석","최지은","고광민","이정민"),birth=c("0617","0921","1013","0216"))
cbind(ex_df,ex_df2)

merge(ex_df,ex_df2,by=c("name"))
rbind(ex_df,c("주은혁",24,"남"))
rbind(ex_df,c("주은혁",24,"남","0903"))

############2.5.1##########
vec_1=c(1:5)
vec_2=rep(c(T,F),c(2,3))
vec_3=data.frame(name=c("a","b","c","d"),age=seq(22,28,2))
vec_list=list(vec_1,vec_2,vec_3)                 
vec_list
vec_list=list(v1=vec_1,v2=vec_2,v3=vec_3)
vec_list
names(vec_list)
names(vec_list)=c("lst","2nd","3rd")
names(vec_list)

#############2.5.2##########
vec_list$`lst`
vec_list[1]
vec_list[[1]]
str(vec_list[1])
str(vec_list[[1]]) 
vec_list[[1]][1]

############2.5.3#########
str(vec_list)
vec_list[[3]]=c(2:6)
vec_list[[3]]
vec_list[[3]][2]=99
vec_list[[3]]
vec_list$`3rd`=c(10:15)
vec_list$`3rd`
vec_list$`3rd`=NULL
vec_list
vec_list[[-2]]
vec_list
vec_list$`3rd`=c(T,F,F,T)
vec_list
vec_list[["4th"]]=c("new vector")
vec_list
vec_list[[5]]=c("5th vector")
vec_list

#########apply & aggregate 함수########
str(iris)
iris_setosa=subset(iris,Species=="setosa",select=c(-Species))
apply(iris_setosa,1,mean)
apply(iris_setosa,2,mean)

########lappy & sapply #######
korea_temp=list("경기"=c(-10,2,1,-2),"강원"=c(0,-4,-5,-10))
korea_temp
result_lapply=lapply(korea_temp,mean)
result_sapply=sapply(korea_temp,mean)
result_lapply
result_sapply

lapply(iris[,-5],mean)
sapply(iris[,-5],mean)

###########tapply & aggregate##########
tapply_dat=tapply(iris$Sepal.Length,Species,mean)
tapply_dat
tapply(iris$Sepal.Width,Species,mean)
tapply(iris$Petal.Length,Species,mean)
tapply(iris$Petal.Width,Species,mean)

aggre_dat=aggregate(Sepal.Length~Species,iris,mean)
aggregate(.~Species,iris,mean)
str(tapply_dat); str(aggre_dat)

tapply_dat[2]
aggre_dat[2]
aggre_dat[2,2]
