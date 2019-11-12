
#workign directory setting
getwd()
setwd("C:/수업/2019_고급통계학특강/Project/data/주요상권/프랜차이즈/스타벅스")

#library load
library(readxl)
library(dplyr)
library(sqldf)
library(ggmap)  ##google map visualization
library(ggplot2)
#install.packages("RMySQL")
#library(RMySQL)


#basic info loading by excel(hand-made)
df1 <- read_excel('./스타벅스매장주소(20191029).xlsx')
head(df1)
str(df1)

#aggregate groupby count
#시도/시군구별 학원수
st_gu <- df1 %>%
  group_by(시도,시군구) %>%
  summarise(n = n())

head(st_gu,n =20)
st_gu

#column rename
names(st_gu) <- c("시도","시군구" , "매장수")
st_gu <- arrange(st_gu, desc(매장수)) #arrange by number ofr stroe

st_gu
tail(st_gu)
#add rank column : 시도기준 group rank
st_gu %>% mutate(순위= min_rank(desc(매장수))) -> st_gu

# 전체 순위 : 순번 
as.numeric(rownames(st_gu))
rownames(st_gu)
st_gu$순번 <- as.numeric(rownames(st_gu))

head(st_gu)
tail(st_gu)
st_gu


#save csv
#write.csv(st_gu, file="./st_gu.csv",row.names=FALSE) 


#store number lager than 10 
st_gu
head(st_gu)
st_gu %>% filter(매장수 > 10) -> st_gu_top

st_gu_top

#bar graph
ggplot(st_gu_top, aes(x=시군구, y=매장수))+geom_bar(stat='identity')
#학원수대로 정렬해서 처리
#RGB COLOR PICKER : https://www.w3schools.com/colors/colors_picker.asp
ggplot(st_gu_top, aes(x=reorder(시군구,순번), y=매장수) )+
  geom_bar(stat='identity',fill="#d699ff")+
  geom_text(aes(label=매장수),vjust=-0.7) +
  labs( x = "시군구", y = "매장수",
        title ="스타벅스 구별 매장수")



#동별 스타벅스 매장수
df1 %>% group_by(시도,시군구,동) %>% summarise(n = n()) -> st_dong
st_dong

#column rename
names(st_dong) <- c("시도","시군구" ,"동", "매장수")
st_dong <- arrange(st_dong, desc(매장수)) #arrange by number ofr stroe

st_dong
tail(st_dong)
#add rank column : 구기준 group rank
st_dong %>% mutate(순위= min_rank(desc(매장수)))-> st_dong


# 전체 순위 : 순번 
as.numeric(rownames(st_dong))
st_dong$순번 <- as.numeric(rownames(st_dong))

st_dong
st_dong %>% filter(시도=="부산광역시") -> st_busan_dong

st_dong %>% filter(시도=="부산광역시" & 시군구 =="해운대구")

#save csv
#write.csv(st_dong, file="./st_dong.csv",row.names=FALSE) 
#write.csv(st_busan_dong, file="./st_busna_dong.csv",row.names=FALSE) 


#주소정보로 위도,경도 정보 채집하기(ggmap)
df1
register_google(key='AIzaSyByyJbCvs_4wroydh7L5umaqX7PfPgH4PA') # 부여받은 키 등록
addr <- df1$주소

addr <- as.vector(addr)
str(addr)
#주소로 위경도 수집하는 절차이나 건별로 시간이 9분이상 소요됨(작업완료후 주석처리)
#gc <- geocode(enc2utf8(addr))

head(gc)
sum(is.na(gc))
#경도 180도 이상건 체크
gc %>% filter(lon>180)

df2 <- cbind(df1,gc)

df2 %>% mutate(id=row_number()) ->df2

colnames(df2)
df2[1:3,c(9,1:8)]

df2 <- df2[,c(9,1:8)]
head(df2)

#save csv
#write.csv(df2, file="./st_geo.csv",row.names=FALSE) 

head(df2)




#mysql table data insert 
con <- dbConnect(MySQL(), user="stat", password="stat1234", dbname="estat_cl_k", host="164.125.33.112")

#테이블목록확인
dbListTables(con)
#테이블컬럼확인
dbListFields(con, "stbuck_str_i")

dbGetQuery(con, "select count(1) from stbuck_str_i")
dbGetQuery(con, "select * from stbuck_str_i limit 10")

#dataframe to Mysql table  : db 스키마 생성시 utf8 -> euckr euckr_korean_ci 로 encoding 생성
# 작업 : df3  ->insert stbuck_str_i  : column명 일치 시켜 처리 
df3 <- df2
names(df3)
names(df3) <- c("id", "bhf_nm", "addr", "tlphon_no","ctprvn", "signgu", "dong", "lo","la")
dbGetQuery(con, "set names 'euckr'")

df3

#df3 -> stbuck_str_i  일괄 insert 
dbWriteTable(con,"stbuck_str_i", df3, append = TRUE, row.names= FALSE) 

#건수 체크 및 데이터 확인 
dbGetQuery(con, "select count(1) from stbuck_str_i")
dbGetQuery(con, 
           "select id, bhf_nm, ctprvn,signgu,dong,lo, la 
              from stbuck_str_i 
            order by CAST(id AS UNSIGNED)")

dbGetQuery(con,
"select id, bhf_nm, ctprvn,signgu,dong,lo, la 
   from stbuck_str_i 
  where signgu = '해운대구'
")

dbDisconnect(con)

## 지도위에 시각화
register_google(key='AIzaSyByyJbCvs_4wroydh7L5umaqX7PfPgH4PA') # 부여받은 키 등록

head(df3)
map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw')

#전국지도에 점찍기 : https://kuduz.tistory.com/1042
ggmap(map) + geom_point(data=df3, aes(x=lo, y=la))
#2d density
ggmap(map) + stat_density_2d(data=df3, aes(x=lo, y=la))
#polygon
ggmap(map) + stat_density_2d(data=df3, aes(x=lo, y=la, fill=..level.., alpha=..level..), geom='polygon', size=2, bins=30)

p <- ggmap(map) + stat_density_2d(data=df3, aes(x=lo, y=la, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28)
p

p+scale_fill_gradient(low = 'yellow',high = 'red')

p + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)


#You need to install Rtools for  source base package install
#install.packages('ggmap')
#install.packages('ggplot2')
#install.packages("raster") 
#install.packages('rgeos', type='source')
#install.packages('maptools')
#install.packages('rgdal', type='source')
#install.packages("viridis")


library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(readxl)
library(viridis)

#if (!require(gpclib)) install.packages("gpclib", type="source")
#library(gpclib)
#gpclibPermit()

#korea <- getData('GADM', country='kor', level=2)
#ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')
#korea

#shp 파일 불러와서 지도로 표시하기, 구별 매장수 시각화 
korea <- shapefile('TL_SCCO_SIG.shp') 
#좌표변환이 필요한 경우
#korea <- spTransform(korea,CRS("+proj=longlat"))
ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')

df4 <- read_excel('./st_gu_id.xlsx')


head(korea)
#shp파일을 데이터프레임으로 shp파일에 따라서  fortify 에러 발생가능성 있음
#korea <- fortify(korea) 
korea <- fortify(korea,region='SIG_CD') 
head(korea)
head(df4)

colnames(df4)
names(df4) <- c("시도","시군구" , "매장수","순위","순번","id")

st_korea <- merge(korea,df4, by='id')
st_korea <- merge(df4,korea, by='id')
head(st_korea)

#스타벅스 매장 지도위에 시각화 전국
ggplot() + geom_polygon(data=st_korea, aes(x=long, y=lat, group=group, fill=매장수))


#부산만 따로 떼어서 보기 26110 ~26710
head(korea)
korea %>% filter(id %in% c('26110' ,'26140' ,'26170' ,'26200' ,'26230' ,'26260' ,'26290' ,'26320' ,'26350' ,'26380' ,'26410' ,'26440' ,'26470' ,'26500' ,'26530' ,'26710'))
korea %>% filter(id %in% c('26110' ,'26140' ,'26170' ,'26200' ,'26230' ,'26260' ,'26290' ,'26320' ,'26350' ,'26380' ,'26410' ,'26440' ,'26470' ,'26500' ,'26530' ,'26710')) ->busan

ggplot() + geom_polygon(data=busan, aes(x=long, y=lat, group=group), fill='white', color='black')


head(df4)
df4 %>% filter(SIG_CD %in% c('26350','26290'))
df4 %>% filter(SIG_CD %in% c('26110' ,'26140' ,'26170' ,'26200' ,'26230' ,'26260' ,'26290' ,'26320' ,'26350' ,'26380' ,'26410' ,'26440' ,'26470' ,'26500' ,'26530' ,'26710')) -> result

head(busan)
names(result) <- c("시도","시군구" , "매장수","순위","순번","id")
result
st_busan <- merge(result,busan,by='id')
head(st_busan)

#스타벅스 매장 지도위에 시각화 부산 : maptype = c(“roadmap”, “terrain”, “satellite”, “hybrid”)
p <- ggplot() + geom_polygon(data=st_busan, aes(x=long, y=lat, group=group, fill=매장수))
p + scale_fill_gradient(low='white',high='blue')
p + scale_fill_viridis(direction = -1)

map <- get_map(location='busan',zoom=11,maptype='roadmap',color='bw')
ggmap(map)
q <- ggmap(map) +geom_polygon(data=st_busan, aes(x=long, y=lat, group=group, fill=매장수),alpha=0.7) +labs(fill="스타벅스 매장수")
q+scale_fill_gradient(low='green',high='red')

q + scale_fill_viridis(direction = -1)
