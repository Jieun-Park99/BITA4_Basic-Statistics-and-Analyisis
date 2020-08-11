#api 확인 해보기 예시 코드 + 전반적인 geocode 출력 보기
library(ggmap)
register_google(key='AIzaSyDglGuNAZE7fzqEr6er5EObrs_vDIUZGb8')
names <- c("용두암","성산일출봉","정방폭포",
           "중문관광단지","한라산1100고지","차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")
gc <- geocode(enc2utf8(addr), ouput="more")

gc


df = data.frame(name = names,
                lon = gc$lon,
                lat = gc$lat)
cen = c(mean(df$lon),mean(df$lat))
map = get_googlemap(center = cen,
                    maptype="roadmap",
                    zoom = 10,
                    size = c(640,640),
                    marker = gc)
ggmap(map)

#tibble  vs. dataframe
library(dplyr)
class(gc)
str(gc)

class(iris)
head(iris)
str(iris)

as.data.frame(gc)
gc

tb<-tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "숫자"
); tb


as_tibble(iris)


#학교 이름 넣어서 output 옵션별로 출력해보기
# geocode(location = '장소이름',output = c('lation','latlona','more','all'), ...)
geocode("SungShin Womans University","latlon")
geocode("Sungshin Womans University","latlona")
geocode("SungShin Womans University","more")
geocode("Sungshin Womans University","all")
#output = 'lation':tibble형식으로 위도와 경도 출력
#output = 'latlona' : tibble 형식으로 위도와 경도, 영어주소를 출력
#output = 'more' : tibble 형식으로 위도와 경도, 주소 출력(방위와 국가범위까지)
#output = 'all' : 행정구역분류, 우편번호 등 세세한 정보까지 확인 가능
# -> 각 정보들이 리스트로 구별되어 있음 


#한국어로 주소 출력하기
#location = en2utf8("지역명&language=ko")
geocode(location=enc2utf8("성신여자대학교&language=ko"),output="latlona")

geocode(location=enc2utf8("성신여자대학교&language=ko"),output="more")
#type : 구글이 분류한 해당 지역(point)의 type(e.g.house(주택),establishment(기관)...)
#loctype : type과 비슷하나 type의 street_address에서 좀 더 세부적인 정보 제공 
# rooftop : 구체적인 geocode와 위치정보를 가진 장소
# range_interpolated : 구체적인 위치정보는 없으나 주변 rooftop들의 정보들로 대체됨 


#[quakes 실습] 
# 지진 규모를 발생 지역에 표시

library(ggmap)
library(ggplot2)

str(quakes)

# quakes information
df <- head(quakes,100)  
df

# map location
cen <- c(mean(df$long), mean(df$lat))
cen

#경도를 넘는 경우 반환
#원래는 360도인데 구글지도에서는 -180~180

# create data frame
gc <- data.frame(lon=df$long, lat=df$lat)

gc$lon <- ifelse(gc$lon>180, -(360-gc$lon), gc$lon) 
gc$lon


get_googlemap(center=cen, # on the map                   
              maptype="roadmap",                     
              zoom=4, marker=gc) %>% ggmap()
#zoom : 3(continent)~21(building)
#디폴트 값: 10(city)

#x,y축 제목, 눈큼, text 제거
map = get_googlemap(center=cen, # on the map                   
                    maptype="roadmap",                     
                    zoom=4,
                    marker=gc)
# x,y axis blank
ggmap(map)+theme(axis.title.x=element_blank(),               
                 axis.text.x=element_blank(),                 
                 axis.ticks.x=element_blank(),                 
                 axis.title.y=element_blank(),                 
                 axis.text.y=element_blank(),                 
                 axis.ticks.y=element_blank())




# mag에 따른 원의 크기 표시
map = get_googlemap(center=cen, # on the map                   
                    maptype="roadmap",                     
                    zoom=5)
ggmap(map)+geom_point(data=df, # 산점도 표현
                      aes(x=long,y=lat,size=mag), # 원의 크기를 mag로 표시
                      alpha=0.5) # 투명도 표시

# depth에 따른 색깔 표시 
ggmap(map) + geom_point(data=quakes, 
                        aes(x=long, y=lat, col=depth), 
                        size=1)


# size, depth에 따른 색깔 표시 
ggmap(map) + geom_point(data=quakes, 
                        aes(x=long, y=lat, size=mag, col=depth), 
                        alpha=0.5)


####### [지하철 실습] #######

data <- read.csv('C:/Users/danan/Desktop/비타민/정규수업_R/7주차/지하철역위치.csv', 
                 header=T)

#필요한 열 뽑기
data2 <- data[,c(2,3,8,9)]
colnames(data2) <- c('전철역명','호선','x좌표','y좌표')
head(data2)
head(data)

# 2호선 추출
s_2 <- data2 %>% filter(호선=='2')

# 상행/하행 모두 표시되서 절반만 추출
N <- seq(1,44,2)
s_21 <- s_2[N,]
s_21

# map에 표현하기
center <- c(mean(s_2$y좌표), mean(s_2$x좌표))
seoul <- get_map(center, zoom=11, maptype='roadmap')

# 투명도 조절
ggmap(seoul) + geom_point(data=s_21, aes(x=y좌표, y=x좌표), size=2.5, alpha=0.7) 



# 3호선 뽑기

s_3 <- data2 %>% filter(호선=='3')
center <- c(mean(s_3$y좌표), mean(s_3$x좌표))
seoul <- get_map(center, zoom=11, maptype='roadmap')

ggmap(seoul,size=c(300,300)) +
  geom_point(data=s_2, aes(x=y좌표, y=x좌표), size=2.5, alpha=0.8, col='black', fill='white', stroke=3 ,shape=21)+ #strok : 테두리 굵기
  geom_point(data=s_3, aes(x=y좌표, y=x좌표), size=2.5, alpha=0.7, col='red')+
  geom_line(data=s_3, aes(x=y좌표, y=x좌표), linetype=1) +
  geom_label(data=s_3, aes(x=y좌표, y=x좌표+0.005, label=전철역명, fontface='bold'), size=2)+
  geom_text(data=s_2, aes(x=y좌표, y=x좌표+0.005, label=전철역명), size=2.8)+
  theme(axis.title.x=element_blank(), # x,y axis blank              
        axis.text.x=element_blank(),                 
        axis.ticks.x=element_blank(),                 
        axis.title.y=element_blank(),                 
        axis.text.y=element_blank(),                 
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill='green'), legend.position='right')


# theme : legend, title, 배경, axis의 line, text 등
#         부가적인 요소 변경




[실제 데이터에서 주소만 알 때]


# 여러 지역의 마커를 표시

names <- c("용두암","성산일출봉","정방폭포",
           "중문관광단지","한라산1100고지","차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")
gc <- geocode(enc2utf8(addr))



df <- data.frame(name=names,
                 lon=gc$lon,
                 lat=gc$lat)

cen <- c(mean(df$lon),mean(df$lat))

get_googlemap(center=cen,
              maptype="roadmap",
              zoom=10,
              size=c(640,640),
              marker=gc) %>% ggmap()



[주요 인구 표시]


# data:다운
#1.
data <- read.csv("poppulation_2014.csv")
head(data)

#2. 열 이름 변경
region <- data$지역명
lon <- data$lon
lat <- data$lat
pop <- data$population

data <- data.frame(region, lon, lat, pop)
head(data)


# 세종 좌표수정(잘못되었음)
data[8,2] <- 127.296620
data[8,3] <- 36.535268


#3. get_map
center <- c(mean(data$lon), mean(data$lat))

map <- get_map(location =center,maptype="watercolor", zoom=7)
m1 <- ggmap(map)


#4. geom_point
m1 + geom_point(data=data, aes(x=lon,y=lat))

#5. pop비율에 맞게 size변경 / label 넣기
m1 + 
  geom_point(data=data, 
             aes(x=lon,y=lat, color=pop, size=pop)) +
  geom_text(data=data, 
            aes(x=lon, y=lat+0.1, label=region),
            size=3)

#6. 범례제목 변경
m1 + 
  geom_point(data=data, 
             aes(x=lon,y=lat, color=pop, size=pop)) +
  geom_text(data=data, 
            aes(x=lon, y=lat+0.1, label=region),
            size=3) +
  scale_color_continuous(name="인구") +
  scale_size_continuous(name="인구")



[scale color/size discrete 예시]


# data:다운
data <- read.csv("C:/Users/palt1/Desktop/R/university.csv")
head(data)
str(data)

# get_map : 지도가져오기
library(ggmap)
map <- get_map("seoul", zoom=18, maptype="watercolor")
ggmap(map)

# data의 위도, 경도 point 찍기
# 학교이름별 색깔변화
map2 <- ggmap(map) + 
  geom_point(data=data,
             aes(x=lon, y=lat, color=factor(학교명)),
             size=3)
map2

# label 넣기 / 범례제목설정
map2 + geom_text(data=data, 
                 aes(x=lon+0.01, y=lat+0.01, label=학교명),
                 size=3) +
  scale_color_discrete(name="학교명") # 범례제목설정