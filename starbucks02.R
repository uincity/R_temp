library(readxl)

#basic info loading by excel(hand-made)
df_co <- read_excel('./상관분석데이터.xlsx')
head(df_co)
str(df_co)

#column rename
colnames(df_co)
names(df_co) <- c("gu","starbucks","work","woker","subway","population")

cor(df_co)
#gu column drop

dropc <- c("gu")
df_co2 <- df_co[,!(names(df_co) %in% dropc)]

df_co2

cor(df_co2)

#correlation visualization
library(corrplot) 
mcor <- cor(df_co2) 
round(mcor, 3) 
plot(df_co2)

#method = c("circle", "square", "ellipse", "number", "shade", "color", "pie") 
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)
corrplot(mcor, method='circle')
corrplot(mcor, method='square')
corrplot(mcor, method='ellipse')
corrplot(mcor, method='number')
corrplot(mcor, method='color')
corrplot(mcor, method='pie')


# corrplot
corrplot(mcor, 
         method="shade", # 색 입힌 사각형
         addshade="all", # 상관관계 방향선 제시
         # shade.col=NA, # 상관관계 방향선 미제시
         tl.col="red", # 라벨 색 지정
         tl.srt=30, # 위쪽 라벨 회전 각도
         diag=FALSE, # 대각선 값 미제시
         addCoef.col="black", # 상관계수 숫자 색
         order="FPC" # "FPC": First Principle Component
         # "hclust" : hierarchical clustering
         # "AOE" : Angular Order of Eigenvectors
)


#test
cor.test(df_co2$starbucks,df_co2$woker)

cor.test(df_co2$starbucks,df_co2$population)



###
# Github
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("ggobi/ggally")
#install.packages("GGally")

library("GGally")

# Correlation plot
ggcorr(df_co2, palette = "RdBu", label = TRUE)

ggpairs(df_co2)



#regresson 
library(ggplot2)

r <- ggplot(df_co, aes(x = starbucks, y = woker,label = gu)) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red")

r + geom_text(check_overlap = TRUE)

r2 <- ggplot(df_co, aes(x = starbucks, y = population,label = gu)) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red")

r2 + geom_text(check_overlap = TRUE)


r3 <- ggplot(df_co, aes(x = starbucks, y = subway,label = gu)) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red")

r3 + geom_text(check_overlap = TRUE)
