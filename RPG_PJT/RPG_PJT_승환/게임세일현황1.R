library( stringr )
library( readr )
library( magrittr )
library( dplyr )
library( tidyr )
library( ggplot2 )
library( reshape2 )
library(RColorBrewer)

#파일 불러오기
games <- read.csv("D:/Rstudy/Video_Games.csv")


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

View(Global_sales_platform)


#가장 많이 판매된 플랫폼
Global_sales_platform <- aggregate.data.frame(x = list(Global_Sales = games$Global_Sales),
                                             by = list(Platform = games$Platform),
                                             FUN = sum)

Global_sales_platform <- Global_sales_platform[order(Global_sales_platform$Global_Sales, decreasing = T), ]
head(Global_sales_platform, 10)




top_sales_platform <- head(Global_sales_platform, 10)

ggplot(data = top_sales_platform, mapping = aes(x = reorder(Platform, -Global_Sales), y = Global_Sales)) +
  geom_col(mapping = aes(fill = Platform)) +
  theme_classic() +
  theme(legend.position = "None") +
  labs(title =  "가장 많이 판매된 플랫폼 top 10",
       x = NULL,
       y = "Global Sales") +
  scale_fill_brewer(palette = "Spectral")







#연도별 판매된 게임 장르 그래프

Global_sales_year <- aggregate.data.frame(x = list(Global_Sales = games$Global_Sales),
                                         by = list(Year = games$Year),
                                         FUN = sum)

Global_sales_year <- Global_sales_year[order(Global_sales_year$Global_Sales, decreasing = T), ]
head(Global_sales_year, 10)
  # 2008, 2009, 2007, 2010, 2006 년도에 가장 많이 팔림

View(top_year)

top_year <- games %>%
  group_by(Year_of_Release, Genre) %>%
  summarize(Global_Sales = sum(Global_Sales)) %>% 
  filter(Year_of_Release %in% c("2008", "2009", "2007", "2010", "2006"))



ggplot(data = top_year, aes(x = reorder(Year_of_Release, Global_Sales), y = Global_Sales))+
  geom_col(aes(fill = Genre), position = "dodge")+
  labs(title = "Top 5 Years Game Release by Genre",
       subtitle = "Video Games Sales Data",
       x = NULL,
       y = "Global Sales",
       fill = NULL)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Spectral")

View(games)