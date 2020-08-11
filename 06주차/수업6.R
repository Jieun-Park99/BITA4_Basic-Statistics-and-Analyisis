gender = c("남","여","남","여","남",
           "여","남","여","남","여",
           "남","여","남","여","남",
           "여","남","여","남","여")
bloodType = c('A','B','A','AB',
              'O','A','O','B',
              'A','O','O','B',
              'O','A','AB','B',
              'O','A','A','B')
classDF = data.frame(gender,bloodType)
classTable = table(classDF)
classTable

install.packages("ggplot2")
library(ggplot2)

classCovDF = as.data.frame(classTable)
classCovDF

#그래프 그릴 빈 공간 만들어주기
ggplot(classCovDF, aes(x = bloodType,y = Freq))

#막대 차트 만들기
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+geom_col()
#aes에 색상 채우기(fill)와 성별 항목 연결 추가
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+geom_col(aes(fill=gender))

#막대의 높이 정보가 존재할 때 geom_col 사용
install.packages("plyr")
library(plyr)
col_data = data.frame(count(bloodType))
names(col_data) = c('bloodType','Freq')
col_data
ggplot(col_data, aes(x=bloodType,y=Freq))+geom_col()

#항목의 수치를 직접 계산해서 높이를 산출할때 Geom_bar()
bar_data = data.frame(bloodType)
bar_data
ggplot(bar_data,aes(x=bloodType))+geom_bar()

#position 따로따로 보기 position='stack'이 default
ggplot(bar_data,aes(bloodType,fill=gender))+geom_bar()
ggplot(bar_data,aes(bloodType,fill=gender))+geom_bar(position ='dodge')


#오로지 각 카테고리의 빈도 수만 보고 싶다면 카테고리를 구별해 계산
Bloodtype = c('A','B','A','AB',
              'O','A','O','B',
              'A','O','O','B',
              'O','A','AB','B',
              'O','A','A','B')
Vision = c(1.0,1.2,0.3,0.4,
           0.5,1.5,0.6,0.4,
           0.8,0.7,1.0,2.0,
           1.2,0.2,0.1,0.2,
           0.4,0.5,0.7,0.8)
Blood_Vision = data.frame(Bloodtype,Vision)
ggplot(Blood_Vision,aes(x=Bloodtype))+
  geom_bar()
ggplot(Blood_Vision,aes(x=Bloodtype,y=Vision))+
  geom_bar(stat='identity')

#geom_boxplot(상자 수염 그림)
data('airquality')
ggplot(data=airquality,
       aes(x=Month,y=Temp,group=Month))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3,
               outlier.color='red',outlier.shape=4)

#이상치 숨기기! Y값의 범위가 변하진 않아 말그대로 표시만 안할뿐
#여기서는 month가 numeric이니까 group을 해야함.범주로 그러나 factor로 바꾸면 안해줘도 됨.
ggplot(data=airquality,
       aes(x=Month,y=Temp,group=Month))+
  geom_boxplot(fill='lightpink',color='darkslategrey',width=0.3,
               outlier.shape=NA)

# +의 위치 중요
ggplot(classDF,
       aes(x=bloodType,fill=gender))
+ geom_bar()
ggplot(classDF,
       aes(x=bloodType,fill=gender))+
  geom_bar()

#geom_point 함수를 이용하여 그래프에 점을 추가하기
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+
  geom_col(aes(fill=gender)) +
  geom_point(aes(shape=gender),size=3)

#geom_line 함수를 이용하여 그래프에 점을 추가해보기
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+
  geom_col(aes(fill=gender))+
  geom_point(aes(shape=gender),size=3)+
  geom_line(aes(group=gender,linetype=gender))

#linetype은 0~6까지 있는데 
#변수를 입력하면 변수별로 자동으로 선 종류가 지정된다.


#ggplot 그래프 꾸미기
BloodbarChart+
  theme_classic()+
  theme(plot.title = element_text(family='serif',
                                  face='bold.italic',
                                  hjust=0.5,
                                  size=30,
                                  color='black'),
        plot.subtitle = element_text(family='serif',
                                     face='bold.italic',
                                     hjust = 0.5,
                                     size=13,
                                     color='black'),
        axis.title = element_text(face='bold.italic',size=20,color='black'),
        axis.text.x = element_text(colour='grey20',size=15,hjust = .5,vjust= .5,face='bold.italic'),
        axis.text.y = element_text(colour='grey20',size=15,face='bold.italic'))+
  theme(legend.title = element_text(face='bold',size=15,color='Black'))+
  theme(legend.box.background = element_rect(fill='Black'),legend.box.margin=margin(2,2,2,2))
 
#ggplot의 제목 추가하기
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+
  geom_col(aes(fill=gender))+
  ggtitle('비타민 혈액형 비율',subtitle='(혈액형/성별 기준)')

#x축, y축, 범례 제목 추가
ggplot(classCovDF,
       aes(x=bloodType,y=Freq))+
  geom_col(aes(fill=gender))+
  ggtitle('비타민 혈액형 비율',subtitle='(혈액형/성별 기준)')+
  labs(x='혈액형',
       y='인원수',
       fill='성별')

#테마를 지정하려면 객체를 생성! 객체 생성!
BloodbarChart = ggplot(classCovDF,
                       aes(x=bloodType,y=Freq))+
  geom_col(aes(fill=gender))+
  ggtitle('비타민 혈액형 비율',
          subtitle='(혈액형/성별 기준)')+
  labs(x='혈액형',
       y='인원수',
       fill='성별')

#만든 객체에 테마를 지정해주기!
BloodbarChart + theme_void()
BloodbarChart + theme_dark()
BloodbarChart + theme_minimal()
BloodbarChart + theme_classic()



#################시계열###############
library(ggplot2)
company = c('A','A','A','A','B','B','B','B')
year = c('1980','1990','2000','2010','1980','1990','2000','2010')
sales = c(2750,2800,2830,2840,2760,2765,2775,2790)

coSalesDF=data.frame(company,year,sales)
coSalesDF

ggplot(coSalesDF,aes(x=year,y=sales))+
  geom_line(aes(group=company))

ggplot(coSalesDF,aes(x=year,y=sales))+
  geom_line(size=2, aes(group=company,colour=company))

ggplot(coSalesDF,aes(x=year,y=sales))+
  geom_line(size=2, aes(group=company,colour=company))+
  geom_point(size=2)

#시계열 그래프 그리기 
head(economics)
ggplot(economics, aes(x=date,y=unemploy))+
  geom_line()

ggplot(economics,aes(x=date,y=unemploy)) + 
  geom_line(size=2,color='lightblue')+
  ggtitle('연도별 실업자 수') +
  labs(x='연도',y='실업자 수')

############## 산점도를 통해 상관관계 알아보기 #######
str(cars)
plot(cars$speed,cars$dist,xlab = '속도', ylab='제동거리리')

plot(cars$speed,cars$dist,xlab='속도',ylab='제동거리')
ggplot(data=cars,aes(x=speed,y=dist))+
  geom_point(shape=8,size=2,colour='red')+
  ggtitle("Scatter Plot: Speed vs Dist")

plot(cars$speed,cars$dist,xlab='속도',ylab='제동거리')
lines(lowess(cars$speed,cars$dist))

#lowess 지역가중 다중식 회귀 
# -> 이변량 자료를 각각의 구간으로 나누어 각각의 구간에서 가중 선형 회귀를 하여 곡선을 구한다.

ggplot(data=cars,aes(x=speed, y=dist))+
  geom_point(shape=8, size=2, colour='red') +
  ggtitle("Scatter Plot: Speed vs Dist") +
  stat_smooth(method ='lm', colour='black')
#stat_smooth = 추세선을 그리기! 

str(iris)
plot(iris)
plot(iris$Sepal.Width, iris$Species)
plot(iris$Sepal.Width, iris$Species)

cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)

new_iris = subset(iris,select=-Species)
cor(new_iris)

install.packages("corrplot")
library(corrplot)
cor_iris = cor(new_iris)
corrplot(cor_iris)
corrplot(cor_iris,method=c("number"))

######## 트리맵 ###########
sales_df = read.table(file="clipboard",header = TRUE, sep='\t')
str(sales_df)

install.packages("treemap")
library('treemap')
# 상품 중심으로 그리기
treemap(sales_df,vSize = "saleAmt", index= c('product','region'),title = "A기업 판매 현황")

#지역 중심으로 그리기
treemap(sales_df,vSize="saleAmt",index=c('region','product'),title="A기업 판매현황")

#diamonds 데이터로그리기
d = diamonds
library(dplyr)
dd = diamonds %>%  
  filter(cut == 'ideal')
dd
treemap(d,vSize = "price", index=c("cut","clarity"),title="price")
treemap(d,vSize = "price", index=c("cut","clarity","color"),title="price")
