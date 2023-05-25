#데이터 불러오기
getwd()
video_games <- read.csv("전공프로젝트/data/Video_Games.csv")
video_games
# View(video_games)

# 결측치 확인하기
table(is.na(video_games))
table(is.na(video_games$Name))
table(is.na(video_games$Year_of_Release))
table(is.na(video_games$Platform))
table(is.na(video_games$Genre))
table(is.na(video_games$Publisher))
table(is.na(video_games$NA_Sales))
table(is.na(video_games$EU_Sales))
table(is.na(video_games$JP_Sales))
table(is.na(video_games$Other_Sales))
table(is.na(video_games$Global_Sales))
table(is.na(video_games$Critic_Score)) # 결측치 존재
table(is.na(video_games$Critic_Count)) # 결측치 존재
table(is.na(video_games$User_Score))
table(is.na(video_games$User_Count)) # 결측치 존재
table(is.na(video_games$Developer))
table(is.na(video_games$Rating))

# 공백값(아무값도 없는 칸)를 NA로 만들기
video_games$Critic_Score <- ifelse(video_games$Critic_Score==' ',NA,video_games$Critic_Score)
video_games$Critic_Count <- ifelse(video_games$Critic_Score==' ',NA,video_games$Critic_Count)
video_games$User_Score <- ifelse(video_games$Critic_Score==' ',NA,video_games$User_Score)
video_games$User_Count <- ifelse(video_games$Critic_Score==' ',NA,video_games$User_Count)
video_games$Developer <- ifelse(video_games$Critic_Score==' ',NA,video_games$Developer)
video_games$Rating <- ifelse(video_games$Critic_Score==' ',NA,video_games$Rating)

# Year_of_Release : N/A 값 최저값(1980)로 바꾸기
library(dplyr)
# video_games$Year_of_Release <- ifelse(video_games$Year_of_Release == 'N/A', NA, video_games$Year_of_Release)
#video_games$Year_of_Release <- as.integer(video_games$Year_of_Release)
# str(video_games)
# summary(video_games$Year_of_Release)
video_games$Year_of_Release <- ifelse(video_games$Year_of_Release == 'N/A', 1980, video_games$Year_of_Release)
video_games$Year_of_Release <- as.integer(video_games$Year_of_Release)
str(video_games)
# View(video_games)
# video_games <- video_games %>% filter(!is.na(video_games$Year_of_Release))

# Year_of_Release :2017년부터는 제거(2016년 이후의 데이터가 거의 없어서)
video_games$Year_of_Release <- ifelse(video_games$Year_of_Release>2016, NA, video_games$Year_of_Release) 
video_games <- video_games %>% filter(!is.na(Year_of_Release))
table(video_games$Year_of_Release)

# 장르 : ''값 NA로 바꾸고 제거
video_games$Genre <- ifelse(video_games$Genre == '', NA, video_games$Genre)
video_games <- video_games %>% filter(!is.na(video_games$Genre))
table(!is.na(video_games$Genre))


# 데이터 확인
# View(video_games)
video_games %>% filter(!is.na(Critic_Score) & !is.na(Critic_Count)) %>% summary() 
str(video_games)
# 데이터 실험1
ex <- video_games %>% filter(!is.na(Critic_Score) & !is.na(Critic_Count) & !is.na(User_Score) & !is.na(User_Count))
table(is.na(ex))
str(ex)
video_games %>% arrange(desc(NA_Sales)) %>% select(NA_Sales) %>% head(10)
# 데이터 실험2
table(video_games$Year_of_Release)
year <- ifelse(video_games$Year_of_Release == 'N/A', NA, video_games$Year_of_Release)
table(year)
GG <- ifelse(video_games$Genre == '', NA, video_games$Genre)
table(!is.na(GG))

######### 현황 분석 - 시각화
library(ggplot2)
library(scales)
# 1. 대룩별 게임 판매 현황 시각화
sum(video_games$NA_Sales)
sum(video_games$EU_Sales)
sum(video_games$JP_Sales)
sum(video_games$Other_Sales)

total_sales <- data.frame(대륙=c('북미','유럽','일본','그외'),
                          판매량=c(sum(video_games$NA_Sales),sum(video_games$EU_Sales),
                                sum(video_games$JP_Sales),sum(video_games$Other_Sales)))

total_sales
# 그래프1
ggplot(data=total_sales, aes(x=대륙,y=판매량))+
  geom_col()
# 정렬 그래프
ggplot(data=total_sales, aes(x=reorder(대륙,-판매량),y=판매량,fill=대륙))+
  geom_col()
# 완성 그래프
ggplot(data=total_sales, aes(x=reorder(대륙,-판매량),y=판매량,fill=대륙))+
  geom_col() +
  geom_text(aes(label=판매량),position=position_stack(vjust=0.5)) +
  scale_fill_brewer(palette='Spectral') +
  ggtitle('대륙별 게임 판매량') +
  xlab('') +
  theme(axis.text.x = element_text(face='bold', size=15),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=13),
        plot.title = element_text(face='bold',
                                  size=25,
                                  hjust=0.5,
                                  color='black'),
        legend.position = 'none')
  
# 북미,유럽,일본에서 비디오게임 시장이 활발하다.
# 특히 북미에서 어마어마한 판매액을 보이고 있는것으로 보아, 북미시장을 타켓하면 좋을 것 같다.

# 2. 어느 기업이 강세인가
# 북미 : Nintendo -> Electronic Arts -> Activision -> Sony -> Ubisoft
str(video_games)
Pu_NA <- video_games %>% group_by(Publisher) %>% 
  summarise(NA_total=sum(NA_Sales),
         EU_total=sum(EU_Sales),
         JP_total=sum(JP_Sales),
         Other_total=sum(Other_Sales)) %>% 
  select(Publisher,NA_total) %>% 
  arrange(desc(NA_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Publisher,-NA_total),y=NA_total,fill=Publisher)) +
  geom_col() + 
  labs(x='북미 기업별 판매량') + theme(legend.position='None')

# 유럽 : Nintendo -> Electronic Arts -> Activision -> Sony -> Ubisoft
Pu_EU <- video_games %>% group_by(Publisher) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Publisher,EU_total) %>% 
  arrange(desc(EU_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Publisher,-EU_total),y=EU_total,fill=Publisher)) +
  geom_col() + 
  labs(x='유럽 기업별 판매량') + theme(legend.position='None')

# 일본 : Nintendo(압도적) -> Namco -> Konami -> Sony -> Capcom
Pu_JP <- video_games %>% group_by(Publisher) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Publisher,JP_total) %>% 
  arrange(desc(JP_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Publisher,-JP_total),y=JP_total,fill=Publisher)) +
  geom_col() + 
  labs(x='기업별 판매량(일본)') + theme(legend.position='None')

# 그 외 국가 : Electronic Arts -> Nintendo -> Sony -> Activision -> Take
Pu_Other <- video_games %>% group_by(Publisher) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Publisher,Other_total) %>% 
  arrange(desc(Other_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Publisher,-Other_total),y=Other_total,fill=Publisher)) +
  geom_col() + 
  labs(x='기업별 판매량(그 외 국가)') + theme(legend.position='None')

# Electronic Arts는 왜 인기일까? -> 피파 시리즈가 인기있다.
video_games %>% filter(Publisher == 'Electronic Arts') %>% 
  select(Name,Publisher,Genre,Year_of_Release,Other_Sales) %>% 
  arrange(desc(Other_Sales)) %>% head(5)
  

library(gridExtra)
grid.arrange(Pu_NA,Pu_EU,Pu_JP,Pu_Other, ncol=2,nrow=2)
# 북미,유럽, 일본 모두 닌텐도 기업이 1위.



# 전체 자료를 통한 기업 순위 보기
str(video_games)
video_games %>% group_by(Publisher) %>% 
  summarise(Global_total=sum(Global_Sales)) %>% 
  select(Publisher,Global_total) %>% 
  arrange(desc(Global_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Publisher,-Global_total),y=Global_total,fill=Publisher)) +
  geom_col() + 
  labs(x='기업별 판매량(순위)') + theme(legend.position='None')
# 닌텐도 기업은 전세계적으로 인기가 있는 것으로 보인다.
# Electronic Arts / Activision 기업이 그 뒤를 따르고 있다.

# 3. 대륙별 장르 선호도
# 북미 : Action - Sports - Shooter - Platform - Misc
str(video_games)
Genre_NA <- video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Genre,NA_total) %>% 
  arrange(desc(NA_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Genre,-NA_total),y=NA_total,fill=Genre)) +
  geom_col() + 
  labs(x='장르별 판매량(북미)') + theme(legend.position='none')

# 유럽 : Action - Sports - Shooter - Racing - Misc
Genre_EU <- video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Genre,EU_total) %>% 
  arrange(desc(EU_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Genre,-EU_total),y=EU_total,fill=Genre)) +
  geom_col() + 
  labs(x='장르별 판매량(유럽)') + theme(legend.position='none')

# 일본 : Role-Playing - Action - Sports - Platform - Misc
Genre_JP <- video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Genre,JP_total) %>% 
  arrange(desc(JP_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Genre,-JP_total),y=JP_total,fill=Genre)) +
  geom_col() + 
  labs(x='장르별 판매량(일본)') + theme(legend.position='none')

# 그외 : Action - Sports - Shooter - Racing - Misc
Genre_Other <- video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  select(Genre,Other_total) %>% 
  arrange(desc(Other_total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(Genre,-Other_total),y=Other_total,fill=Genre)) +
  geom_col() + 
  labs(x='장르별 판매량(그 외)') + theme(legend.position='none')

# install.packages('gridExtra')
library(gridExtra)
grid.arrange(Genre_NA,Genre_EU,Genre_JP,Genre_Other, ncol=2,nrow=2)
# 북미,유럽,그외는 모두 Action,Sports,Shooter 순이지만
# 일본에서는 Role-Playing 장르가 압도적으로 1위이다.

# 4. 지역별 인기 게임
# 북미 인기게임 :  Wii Sports - Super Mario Bros. -  Duck Hunt - Tetris - Mario Kart Wii
popular_NA <- video_games %>% arrange(desc(NA_Sales)) %>% 
  select(Name,NA_Sales) %>% head(5) %>% 
  ggplot(aes(x=reorder(Name,-NA_Sales),y=NA_Sales,fill=Name)) +
  geom_col() + 
  labs(x=' 북미 인기게임') + theme(legend.position='none')

# 유럽 인기게임 :  Wii Sports - Mario Kart Wii - Nintendogs - Wii Sports Resort - Brain Age: Train Your Brain in Minutes a Day
popular_EU <- video_games %>% arrange(desc(EU_Sales)) %>% 
  select(Name,EU_Sales) %>% head(5) %>% 
  ggplot(aes(x=reorder(Name,-EU_Sales),y=EU_Sales,fill=Name)) +
  geom_col() + 
  labs(x=' 유럽 인기게임') + theme(legend.position='none')

# 일본 인기게임 :  Pokemon Red/Pokemon Blue  - Pokemon Gold/Pokemon Silver  - Super Mario Bros. - New Super Mario Bros - Pokemon Diamond/Pokemon Pearl 
popular_JP <- video_games %>% arrange(desc(JP_Sales)) %>% 
  select(Name,JP_Sales) %>% head(5) %>% 
  ggplot(aes(x=reorder(Name,-JP_Sales),y=JP_Sales,fill=Name)) +
  geom_col() + 
  labs(x=' 일본 인기게임') + theme(legend.position='none')

# 그외 대륙 인기게임 :  Grand Theft Auto: San Andreas - Wii Sports  - Gran Turismo 4 - Grand Theft Auto V - Mario Kart Wii 
popular_Other <- video_games %>% arrange(desc(Other_Sales)) %>% 
  select(Name,Other_Sales) %>% head(5) %>% 
  ggplot(aes(x=reorder(Name,-Other_Sales),y=Other_Sales,fill=Name)) +
  geom_col() + 
  labs(x=' 그외대륙 인기게임') + theme(legend.position='none')

grid.arrange(popular_NA,popular_EU,popular_JP,popular_Other, ncol=2,nrow=2)
# 북미와 유럽에서는 Wii Sports, 일본에서는 포켓몬 Red/Blue, 그 외 대륙에서는 Grand Theft가 가장 인기있다.
# 대륙별로 인기있는 게임이 다르다는 것을 알 수 있다. 

# 전세계 인기있는 게임 순위 - 원그래프
video_games %>% arrange(desc(Global_Sales)) %>% head(5) %>%  
  mutate(Global_Sales_percent=(Global_Sales/sum(Global_Sales)*100)) %>% 
  select(Name,Global_Sales_percent) %>% 
  ggplot(aes(x="",y=Global_Sales_percent,fill=Name)) +
  geom_bar(width = 1,stat='identity') +coord_polar("y", start=0)

# 전세계 인기있는 게임 순위 - 원그래프2
video_games %>% mutate(Global_Sales_percent=round(Global_Sales/sum(Global_Sales)*100,2)) %>%
  arrange(desc(Global_Sales)) %>% 
  select(Name,Global_Sales_percent) %>% head(5) %>% 
  ggplot(aes(x="",y=Global_Sales_percent,fill=Name)) +
  geom_bar(width = 1,stat='identity') +coord_polar("y", start=0) +
  geom_text(aes(label=Global_Sales_percent),position=position_stack(vjust=0.5))
# Wii Sports가 전세계 비디오게임 중 0.93%로 가장 매출이 높다.
# 그 뒤로 Super Mario, Mario Kart, Pockemon Red/Blue가 뒤를 따르고 있다.

####################
# 연도별로 출시된 장르 수 시각화
video_games %>% group_by(Genre,Year_of_Release) %>%
  summarise(count=n()) %>% arrange(desc(count)) %>% 
  ggplot(aes(x=Year_of_Release,y=count,colour=Genre)) +
  geom_line() +
  ggtitle('연도별로 출시된 장르 수 시각화') +
  theme(axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5,
                                  color='black'))
#   Genre  Year_of_Release     n
# 1 Action            2009   272
# 2 Action            2012   265
# 3 Action            2015   253
# 4 Action            2011   238
# 5 Action            2010   226
# 6 Action            2008   221
# 7 Misc              2008   212
# 8 Action            2007   210
# 9 Misc              2009   206
# 10 Misc              2010   201

# 출시된 장르 수 확인(어떤 장르가 출시가 많이됐나)
video_games %>% group_by(Genre) %>%
  summarise('출시된 수'=n()) %>% arrange(desc('출시된 수'))
#   Genre            n
# 1 Action        3369
# 2 Sports        2348
# 3 Misc          1750
# 4 Role-Playing  1498
# 5 Shooter       1323
# 6 Adventure     1303
# 7 Racing        1249
# 8 Platform       888
# 9 Simulation     873
# 10 Fighting       849
# 11 Strategy       683
# 12 Puzzle         580

# 많이 출시된 장르 top5만 그래프 그려보기
video_games %>% filter(Genre == c('Action','Sports','Misc','Role-Playing','Shooter'))%>% 
  group_by(Genre,Year_of_Release) %>%
  summarise(count=n()) %>% arrange(desc(count)) %>% 
  ggplot(aes(x=Year_of_Release,y=count,colour=Genre)) +
  geom_line() +
  ggtitle('많이 출시된 장르 top5 그래프') +
  theme(axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=15,
                                  hjust=0.5,
                                  color='black'))

###########
# 5. 연도별 출시게임 장르 현황 시각화
### 북미
str(video_games)
# 북미 판매량 높은 5개의 장르 : Action Sports Shooter Platform Misc
video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  arrange(desc(NA_total)) %>% 
  head(5) # Action Sports Shooter Platform Misc

#연도별 장르 시각화(북미)
video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Platform','Misc')) %>% 
  select(Name,Year_of_Release,Genre,NA_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=NA_Sales,colour=Genre, group=Genre)) +
  geom_line() +
  ggtitle('연도별 장르 시각화(북미)') +
  theme(axis.text.x = element_text(face='bold', size=12),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5,
                                  color='black'))

video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Platform','Misc')) %>% 
  select(Name,Year_of_Release,Genre,NA_Sales) %>% arrange(desc(NA_Sales)) %>% 
  filter(Year_of_Release %in% c(1983,1984,1985,2005,2006)) %>% head(5)
#                   Name Year_of_Release    Genre NA_Sales
# 1            Wii Sports            2006   Sports    41.36
# 2     Super Mario Bros.            1985 Platform    29.08
# 3             Duck Hunt            1984  Shooter    26.93 

video_games %>% filter(Genre %in% c('Action','Sports')) %>% 
  select(Name,Year_of_Release,Genre,NA_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=NA_Sales,color=Genre, group=Genre)) +
  geom_line()

video_games %>% filter(Genre %in% c('Action','Sports','Shooter')) %>% 
  select(Name,Year_of_Release,Genre,NA_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=NA_Sales,color=Genre, group=Genre)) +
  geom_line()
# sports,shooter는 초대박 흥행작이 있는 반면 action은 초대박 흥행작이 없다.
# 초대박 흥행작 없이 action이 가장 많은 수익을 낸 것을 보면
# action은 꾸준히 사랑받고 있음을 알수있다.


### 유럽
# 유럽 판매량 높은 5개의 장르 : Action Sports Shooter Racing Misc
video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  arrange(desc(EU_total)) %>% 
  head(5) # Action Sports Shooter Racing Misc

#연도별 장르 시각화(유럽)
video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Racing','Misc')) %>% 
  select(Name,Year_of_Release,Genre,EU_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=EU_Sales,colour=Genre, group=Genre)) +
  geom_line() +
  ggtitle('연도별 장르 시각화(유럽)') +
  theme(axis.text.x = element_text(face='bold', size=12),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5,
                                  color='black'))

video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Racing','Misc')) %>% 
  select(Name,Year_of_Release,Genre,EU_Sales) %>% arrange(desc(EU_Sales)) %>% 
  head(10)
#                                            Name Year_of_Release    Genre EU_Sales
# 1                                    Wii Sports            2006 Sports    28.96
# 2                                Mario Kart Wii            2008 Racing    12.76
# 3                             Wii Sports Resort            2009 Sports    10.93
# 4  Brain Age: Train Your Brain in Minutes a Day            2005   Misc     9.20
# 5                                      Wii Play            2006   Misc     9.18
# 6                            Grand Theft Auto V            2013 Action     9.09
# 7                                  Wii Fit Plus            2009 Sports     8.49
# 8                                       Wii Fit            2007 Sports     8.03
# 9                                 Mario Kart DS            2005 Racing     7.47
# 10                           Grand Theft Auto V            2014 Action     6.31

video_games %>% filter(Genre %in% c('Action','Sports')) %>% 
  select(Name,Year_of_Release,Genre,EU_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=EU_Sales,color=Genre, group=Genre)) +
  geom_line()

video_games %>% filter(Genre %in% c('Action','Sports','Shooter')) %>% 
  select(Name,Year_of_Release,Genre,EU_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=EU_Sales,color=Genre, group=Genre)) +
  geom_line()
# sports만 초대박 흥행작이 있는걸 봐서, shooter(Duck Hunt)는 북미에서만 인기있었다.
# 북미와 동일하게 action은 꾸준히 사랑받고 있음을 알수있다.


### 일본
# 일본 판매량 높은 5개의 장르 : Role-Playing Action Sports Platform Misc
video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  arrange(desc(JP_total)) %>% 
  head(5) # Role-Playing Action Sports Platform Misc

#연도별 장르 시각화(일본)
video_games %>% filter(Genre %in% c('Role-Playing', 'Action', 'Sports', 'Platform', 'Misc')) %>% 
  select(Name,Year_of_Release,Genre,JP_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=JP_Sales,colour=Genre, group=Genre)) +
  geom_line() +
  ggtitle('연도별 장르 시각화(일본)') +
  theme(axis.text.x = element_text(face='bold', size=12),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5,
                                  color='black'))

video_games %>% filter(Genre %in% c('Role-Playing', 'Action', 'Sports', 'Platform', 'Misc')) %>% 
  select(Name,Year_of_Release,Genre,JP_Sales) %>% arrange(desc(JP_Sales)) %>% 
  head(10)
#                            Name Year_of_Release        Genre JP_Sales
# 1       Pokemon Red/Pokemon Blue            1996 Role-Playing    10.22
# 2    Pokemon Gold/Pokemon Silver            1999 Role-Playing     7.20
# 3              Super Mario Bros.            1985     Platform     6.81
# 4          New Super Mario Bros.            2006     Platform     6.50
# 5  Pokemon Diamond/Pokemon Pearl            2006 Role-Playing     6.04
# 6    Pokemon Black/Pokemon White            2010 Role-Playing     5.65
# 7  Pokemon Ruby/Pokemon Sapphire            2002 Role-Playing     5.38
# 8       Monster Hunter Freedom 3            2010 Role-Playing     4.87
# 9      New Super Mario Bros. Wii            2009     Platform     4.70
# 10           Pokemon X/Pokemon Y            2013 Role-Playing     4.35

# 일본에서 포켓몬 시리즈는 초대박 흥행작으로 상당히 인기있는 게임 시리즈임을 알 수 있다.
# 포켓몬 외에는 슈퍼마리오가 인기가 있다.

video_games %>% filter(Genre %in% c('Role-Playing', 'Action', 'Sports')) %>% 
  select(Name,Year_of_Release,Genre,JP_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=JP_Sales,color=Genre, group=Genre)) +
  geom_line()
# 일본에서는 Role-Playing(포켓몬시리즈)가 꾸준히 엄청난 사랑을 받고있다.


### 그 외 대륙
# 그 외 대륙 판매량 높은 5개의 장르 : Action Sports Shooter Racing Misc
video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales)) %>% 
  arrange(desc(Other_total)) %>% 
  head(5) # Action Sports Shooter Racing Misc

#연도별 장르 시각화(그 외 대륙)
video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Racing','Misc')) %>% 
  select(Name,Year_of_Release,Genre,Other_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Other_Sales,colour=Genre, group=Genre)) +
  geom_line() +
  ggtitle('연도별 장르 시각화(그 외)') +
  theme(axis.text.x = element_text(face='bold', size=12),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.x = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5,
                                  color='black'))

video_games %>% filter(Genre %in% c('Action','Sports','Shooter','Racing','Misc')) %>% 
  select(Name,Year_of_Release,Genre,Other_Sales) %>% arrange(desc(Other_Sales)) %>% 
  head(10)
#                             Name Year_of_Release    Genre Other_Sales
# 1  Grand Theft Auto: San Andreas            2004  Action       10.57
# 2                     Wii Sports            2006  Sports        8.45
# 3                 Gran Turismo 4            2004  Racing        7.53
# 4             Grand Theft Auto V            2013  Action        3.96
# 5                 Mario Kart Wii            2008  Racing        3.29
# 6              Wii Sports Resort            2009  Sports        2.95
# 7      Pro Evolution Soccer 2008            2007  Sports        2.93
# 8                       Wii Play            2006    Misc        2.84
# 9                 FIFA Soccer 08            2007  Sports        2.46
# 10    Call of Duty: Black Ops II            2012 Shooter        2.42

# 그 외 대륙에서도 Wii는 흥행하였다. 전세계적인 게임임이 확실하다.
# 그 외 대륙에서는 Action(Grand Theft Auto: San Andreas), Racing(Gran Turismo 4)이 인기가 있다.

video_games %>% filter(Genre %in% c('Action','Sports','Shooter')) %>% 
  select(Name,Year_of_Release,Genre,Other_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Other_Sales,color=Genre, group=Genre)) +
  geom_line()

#####
#

video_games %>% group_by(Genre) %>% 
  ggplot(aes(x=Genre,y=NA_Sales, group=Genre)) +
  geom_line()

#####
# 출시연도별 장르 흐름 시각화 _ 누적막대그래프 
str(video_games)

video_games %>% select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,fill=Genre, group=Genre)) +
  geom_bar(stat='identity')

video_games %>% select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,colour=Genre, group=Genre)) +
  geom_point()

video_games %>% select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,colour=Genre, group=Genre)) +
  geom_line()


video_games %>% select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,fill=Genre, group=Genre)) +
  geom_bar(stat='identity',position='dodge')

# 글로벌세일즈 top5 중 장르별 판매량
video_games %>% group_by(Year_of_Release) %>% 
  summarise(Global_total=sum(Global_Sales)) %>% 
  arrange(desc(Global_total)) %>% head(5) # 2008 2009 2007 2010 2006

video_games %>% filter(Year_of_Release %in% c(2008,2009,2007,2010,2006)) %>%
  select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,fill=Genre, group=Genre)) +
  geom_bar(stat='identity',position='dodge')


###########################
# 통계적 검증

# 글로벌 세일즈 확인용
video_games %>% group_by(Genre) %>% 
  summarise(NA_total=sum(NA_Sales),
            EU_total=sum(EU_Sales),
            JP_total=sum(JP_Sales),
            Other_total=sum(Other_Sales),
            Global_total=sum(Global_Sales)) %>% 
  arrange(desc(Global_total)) %>% 
  head(5) # Action Sports Shooter Racing Misc

video_games %>% filter(Genre %in% c('Action','Sports','Shooter')) %>% 
  select(Name,Year_of_Release,Genre,Global_Sales) %>% group_by(Genre) %>% 
  ggplot(aes(x=Year_of_Release,y=Global_Sales,color=Genre, group=Genre)) +
  geom_line()

# 통계적 검증
trend <- video_games %>%
  mutate(Year =ifelse(Year_of_Release<1990, 1980,
                       ifelse(Year_of_Release<2000, 1990,
                              ifelse(Year_of_Release<2010, 2000, 2010)))) %>% 
  select(Global_Sales,Year,Genre) %>% 
  group_by(Genre,Year) %>% summarise(Global_total=sum(Global_Sales))

trend
# trend 데이터를 피봇테이블 형식으로 변환
library(reshape2)
trend_dcast = dcast(trend, Year ~ Genre, sum , value.var = 'Global_total')
# 카이제곱 검정
chisq.test(trend_dcast, correct = TRUE) #  p-value < 2.2e-16 
# 귀무가설 (H0): 연대와 장르사이에 관련이 없다.(비디오 게임 장르의 트랜드는 변하지 않는다)
# 대립가설 (H1): 연대와 장르사이에 관련이 있다.(비디오 게임 장르의 트랜드는 변한다)

# 카이제곱 검정을 시행한 결과 p-value값이 < 2.2e-16이 나왔다.
# 비디오 게임장르에서 트랜드가 없다는 귀무가설을 기각한다.
# 즉, 비디오게임 시장에서 게임장르의 트랜드는 계속해서 변화하는 것으로 판단할 수 있다