
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

games <- read.csv("D:/Rstudy/Video_Games.csv")


#데이터 파악
dim(games)
names(games)

str(games)

games[ , c("Platform", "Year_of_Release", "Genre")] <- lapply(games[ , c("Platform", "Year_of_Release", "Genre")], as.factor)

str(games)

games[games == "N/A"]<-NA
colSums(is.na(games))

colSums(is.na(games))/nrow(games)

games <- games %>% 
  drop_na(Year_of_Release)
anyNA(games)

summary(games)





#년도 정수형 변환

games$Year_of_Release<- as.integer(as.character(games$Year_of_Release))

is.integer(games$Year_of_Release)


#null / na 값 확인 및 제거

is.null(games)
is.na(games)
table(is.na(games))




gamess <- melt(games,
                    id.vars = c("Genre","Year_of_Release","Global_Sales"),
                    measure.vars = "Global_Sales", na.rm = TRUE)
Genre <- gamess %>%
  group_by (Genre) %>%
  summarize(Global_Sales=n()) %>%
  arrange(desc(Global_Sales))
ggplot((data = Genre), aes(x = reorder(Genre, - Global_Sales), y = Global_Sales)) + geom_col (fill = "Dark Green") + labs(title = "가장 인기있는 장르", x = "Genre" , y='Global_Sales') + theme(axis.text.x= element_text(size =10, angle = 90))




#연도에 따른 게임 장르별 판매량 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

wrap <- gamess %>%
  filter (Year_of_Release >=1997) %>%
  filter (Year_of_Release < 2017) %>%
  group_by(Genre, Year_of_Release) %>%
  summarize(Global_Sales = n())
#시각화2
ggplot(data = wrap, aes(x = Global_Sales, y = Genre)) + geom_col(fill = "Dark Green", color = "Black") + facet_wrap(~Year_of_Release) + theme_classic() + labs(title = "연도에 따른 게임 장르별 판매량", x='Global_Sales')


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#연도에 따른 게임 장르별 판매량2
gamess %>%
  filter(Year_of_Release < 2017) %>%
  group_by (Genre, Year_of_Release) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  #시각화3
  ggplot(aes(x = Year_of_Release, y = total_sales, fill = Genre)) + geom_col() + scale_fill_brewer(palette = "Set3") + theme_classic() + labs(title = "연도에 따른 게임 장르별 판매량2", y = "Global Sales (Millions)")
