# 데이터셋 가져오기 ------------------------------------------------------------
getwd()
setwd('RPG_PJT')
# game <- read.csv('Video_Games(정제).csv')
# View(game)
# summary(game)
str(Game)

game <- read.csv('Video_Games(연평균).csv')
game[game == ""] <- NA
game[game == "tbd"] <- NA
game$U_Score <- as.numeric(game$U_Score) # 형변환
# U_Score 열의 NA 값을 최저값으로 변경
game$U_Score[is.na(game$U_Score)] <- min(game$U_Score, na.rm = TRUE)
Game <- game

# Name, U_Score, Rating 칼럼을 제거하고 새로운 df생성해서 사용
Game <- game[, !(names(game) %in% c("Name", "U_Score", "Rating"))]
summary(Game)



# 기술통계 ####################################################################

# 1. 전에 결측치 확인하기 ---------------------------------------
missing <- sapply(Game, function(x) sum(is.na(x)))
table(missing)
# missing
#  0 
# 10 # 컬럼수임


# 2. 각 컬럼들에 대한 기초 통계량 확인하기 -----------------------------
stat <- sapply(Game, summary)
stat

# 최소값, 1사분위, 중앙, 평균, 3사분위, 최대값

# $Platform
# Length     Class      Mode 
# 16716 character character 
# 
# $Year
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 1977    2003    2007    2006    2010    2020 
# 
# $Genre
# Length     Class      Mode 
# 16716 character character 
# 
# $Publisher
# Length     Class      Mode 
# 16716 character character 
# 
# $NA_
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.000   0.000   0.500   1.659   1.480 243.290 
# 
# $EU_
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.000   0.000   0.140   1.004   0.690 170.350 
# 
# $JP_
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.4543  0.2500 43.5000 
# 
# $Other_
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0000  0.0000  0.0600  0.3353  0.2200 55.6300 
# 
# $Global_
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.030   0.390   1.065   3.454   2.940 485.470 
# 
# $C_Score
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 13.00   33.00   64.00   55.06   75.00  100.00 


# 3. 범주형변수들 간단히 시각화하기-----------------------------------------

par(mfrow = c(2,2))
par(mar = c(4,4,4,4))
# (1) 플랫폼
platform_freq <- table(game$Platform)
# 막대그래프
barplot(platform_freq, main = "Frequency of Platforms", 
        xlab = "Platform", ylab = "Freq", las = 2)
# DS,PS2 > PS3,Wii,X360,PSP ..



# (2) 장르
genre_freq <- table(game$Genre)
# 막대그래프 
barplot(genre_freq, main = "Frequency of Genres", 
        xlab = "Genre", ylab = "Freq", las = 2)
# Action > Sports > Misc ..




# (3) publisher
publisher_freq <- table(game$Publisher)
# 막대그래프 
barplot(publisher_freq, main = "Frequency of Publishers", 
        xlab = "Publisher", ylab = "Freq", las = 2)
# Electronic Arts Victor > Activision > MTO ..

par(mfrow = c(1,1))





# 4. 수치형변수들 간단히 시각화 --------------------------------------------

# (1)출시연도
hist(Game$Year, col = "skyblue", main = "Histogram of Year")
# 90년 중반부터 성장세, 2010년 쯤 탑 찍고 급격한 하락세

# (2)매출 - 북미,유렵,일본,그외,총매출
boxplot(Game$NA_, Game$EU_, Game$JP_, Game$Other_, Game$Global_, 
        col = c("skyblue", "lightgreen", "lightyellow", "lightpink", "lightgray"),
        main = "Boxplot of Sales",
        names = c("NA", "EU", "JP", "Other", "Global"))
# 매출액이 고루 분포되어있지않고 대부분 낮은데 특정 인기게임만 압도적인 매출
# 일본시장이 전반적으로 낮지만 비슷한 분포도를 가지고 있는 듯 하다.(?)


# (3)전문가평점
densityPlot <- density(Game$C_Score)
plot(densityPlot, main = "Density Plot of C_Score", 
     xlab = "C_Score", ylab = "Density")
# 75점이 평균적으로 주는 점수인 듯 하며, 
# 아래 13점구간에 압도적으로 밀집한 선형을 띄는 이유는 
# 원본 데이터셋이 약 16700행 중 8000개 결측, 4000개를 추가보완하고도 
# 부족한 값은 최저점수인 13으로 대체했기 때문으로 추정됨.


# (4) 북미와, 글로벌 누적판매 산점도
plot(Game$NA_, Game$Global_, col = "skyblue", pch = 16,
     main = "Scatter Plot of Sales", xlab = "NA Sales", ylab = "Global Sales")
# 북미와 글로벌 누적판매량은 양의 상관관계를 갖는 듯 하다. 





# 5. 상관관계 분석해보기----------------------------------------------------------
# 우선 기준값으로 설정해둔 글로벌판매량과 다른 변수간의 상관관계 간단히 분석해보기

str(Game)

# (1) 피어슨 상관계수 - 
# Global_ 변수를 종속 변수(Y값)로 설정하여 다른 변수들 간의 상관관계 계산
correlation <- cor(Game[, c("NA_", "EU_", "JP_", "Other_", "C_Score", "U_Score")], 
                   Game$Global_)

# 계산된 상관계수 출력
print(correlation)


# (2) 산점도 그려서 확인

par(mfrow = c(3,2))
plot(Game$Global_, Game$NA_, main = "NA_", 
     xlab = "NA_", ylab = "Global_")
plot(Game$Global_, Game$EU_, main = "EU_", 
     xlab = "EU_", ylab = "Global_")
plot(Game$Global_, Game$JP_, main = "JP_", 
     xlab = "JP_", ylab = "Global_")
plot(Game$Global_, Game$Other_, main = "Other_", 
     xlab = "Other_", ylab = "Global_")
plot(Game$Global_, Game$C_Score, main = "C_Score", 
     xlab = "C_Score", ylab = "Global_")
plot(Game$Global_, Game$C_Score, main = "C_Score", 
     xlab = "U_Score", ylab = "Global_")






# 5_2. 범주(장르,플랫폼,publisher)-수치(글로벌판매)형 변수 간  ------------------------
# 상관관계 분석하기(main주제임)
# 더미변수로 변환(원핫 인코딩) - matrix함수 사용


# (1) 장르
dummy_genre <- model.matrix(~ Genre - 1, data = Game)
# 상관계수 계산
correlation <- cor(Game$Global_, dummy_genre)
mean(correlation)
#       GenreAction GenreAdventure GenreFighting   GenreMisc GenrePlatform GenrePuzzle
# [1,]  0.01126355    -0.06626163  -0.008869262 -0.01361534    0.03247212  -0.0260145

# GenreRacing Genre  Role-Playing Genre  Shooter   GenreSimulation GenreSports GenreStrategy
# [1,] -0.002033382         0.0217427   0.07153019     -0.01538952 0.009545931   -0.04059031

# Shooter 양의 상관관계를 보임 -  0.07153019


# (2) 플랫폼
dummy_flatform <- model.matrix(~ Platform - 1, data = Game)
correlation_f <- cor(Game$Global_, dummy_flatform)
mean(correlation_f)
#  correlation_f
#       Platform2600 Platform3DO Platform3DS  PlatformDC  PlatformDS PlatformGB PlatformGBA
# [1,]  -0.01514504 -0.00445112  0.02825695 -0.01195419 -0.03818607 0.04537331 -0.03427059
# PlatformGC   PlatformGEN   PlatformGG  PlatformN64 PlatformNES   PlatformNG
# [1,] -0.03036782 -0.0001268009 -0.002559436 -0.009536446  0.02738705 -0.008104245
#       PlatformPC PlatformPCFX  PlatformPS PlatformPS2 PlatformPS3 PlatformPS4 PlatformPSP
# [1,] -0.03845364 -0.002574835 -0.02760008 -0.01488506  0.06751544  0.09789476 -0.05106132
#       PlatformPSV PlatformSAT PlatformSCD PlatformSNES PlatformTG16 PlatformWii PlatformWiiU
# [1,] -0.03479722 -0.02785878 -0.00454437 -0.007780378 -0.003450922  0.03886018    0.0244158
#         PlatformWS PlatformX360  PlatformXB PlatformXOne
# [1,] -0.004578951    0.0738912 -0.04193095      0.05253
max(correlation_f)
# PS4 0.09789476


# (3) publisher
dummy_publisher <- model.matrix(~ Publisher - 1, data = Game)
correlation_p <- cor(Game$Global_, dummy_publisher )
mean(correlation_p)

max(correlation_p)
# 0.2272958 가장 강한 양의 상관관계
 # Nintendo 
                  
head(Game)


#####################################################################################
# 가설1 :  연대별로 비디오 게임의 장르 트랜드가 변화한다 
# 귀무가설 (H0): 연대와 장르사이에 관련이 없다.(비디오 게임 장르의 트랜드는 변하지 않는다.)
# 대립가설 (H1): 연대와 장르사이에 관련이 있다.(비디오 게임 장르의 트랜드는 변한다.)

# p값으로 파악 

#  ⇒ p < 0.05 - 귀무가설을 기각하고 대립가설을 채택한다

# p > 0.05 - 귀무가설 기각 없이 판단을 보류한다.


# 카이제곱으로 검정 (출시년도/장르)-----------------------------------------------------
chisq_table <- table(Game$Year, Game$Genre)
result <- chisq.test(chisq_table)
result
# Pearson's Chi-squared test
# 
# data:  contingency_table
# X-squared = 2867.8 : 통계량 값이 클수록 변수들사이 강한 관련성 O
# df = 451 : 자유도, 범주형변수 카테고리 수 기반
# p-value < 2.2e-16 : 매우 작은값으로 극단적인 결과를 얻을 확률이 매우 낮음

# 출시연도와 장르사이에 관련성이 있다!!!!! 
# 따라서 대부분의 유의수준에서 귀무가설을 기각하고 대립가설을 채택할 수 있다.









# 년도,장르가 글로벌매출에 유의미한 영향 주는지 -------------------


# 이원분산분석(ANOVA)으로 검정
AOV <- aov(Global_ ~ Year + Genre, data = Game)
result_aov <- summary(AOV)
result_aov
# Df           Sum    Sq   Mean Sq Fvalue  Pr(>F)    
# Year            1   0.67  0.6747   68.02 <2e-16 ***
# Genre          11   2.40  0.2179   21.97 <2e-16 ***
# Residuals   16699 165.63  0.0099                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Pr(>F)이 매우 작은 값이다 ***!! 연도,장르 변수에 따라 매출값이 유의미하게 변화한다. 










# 게임의 장르에 따라 평균 평점이 달라지는지 -----------------------------

# 가설에 필요한 변수 선택
subset_data <- game[, c("Genre", "C_Score")]
# 장르별로 평균 평점 계산
genre_means <- aggregate(C_Score ~ Genre, data = subset_data, FUN = mean)
# 평균 평점 비교를 위한 박스플롯
boxplot(C_Score ~ Genre, 
        data = subset_data, 
        col = "skyblue", 
        main = "평균 평점의 장르별 비교")

# 통계적 검정을 통한 가설 검정
# 예를 들어, 분산분석(ANOVA)을 사용하여 장르별 평균 평점의 차이를 검정할 수 있습니다.
result <- aov(C_Score ~ Genre, data = subset_data)
summary(result)

#                     제곱합      평균제곱     F값       유의확률
#                 Df   Sum Sq Mean Sq   F value   Pr(>F)    
# Genre          11   477835   43440    64.64   <2e-16 ***
# Residuals   16700 11223535     672               

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# F값은 분산분석에서 사용되는 통계량으로 그룹간 분산과 그룹 내 분산의 비율 나타냄,
# 클수록 평균의 차이가 있는 것을 의미
# 유의확률은 가설 검정에서 사용되는 값으로 통계적으로 유의미한 차이가 있는지 판단,
# 작을수록 (일반적으로 0.05보다 작을 때) 평균의 차이가 유의미하다고 판단

# Pr값 매우 작음. 장르 변수에 따른 평균 평점의 차이는 통계적으로
# 유의미하다고 판단할 수 있다 -> 장르에 따라 평균 평점에 차이가 있다.


str(game)

game <- read.csv('Video_Games(정제).csv')
game[game == ""] <- NA
game[game == "tbd"] <- NA
game$U_Score <- as.numeric(game$U_Score)
game$U_Score[is.na(game$U_Score)] <- min(game$U_Score, na.rm = TRUE)



# 각 장르별 연별 출시흐름
subset_data <- game[, c("Year", "Genre")]

genre_counts <- table(subset_data$Year, subset_data$Genre)
barplot(genre_counts, beside = TRUE, 
        legend.text = TRUE, 
        xlab = "Year", 
        ylab = "Count", 
        main = "Number of Games Released by Genre")



# 각 장르별 평가점수
library(dplyr)
genre_scores <- game %>%
  group_by(Genre) %>%
  summarize(Avg_Score = mean(C_Score, na.rm = TRUE))

barplot(genre_scores$Avg_Score, 
        names.arg = genre_scores$Genre, 
        xlab = "Genre", 
        ylab = "Average Score", 
        main = "Average Scores by Genre")



# 시대별로 장르의 트렌드가 변화하는지 카이제곱으로 가설검정

library(ggplot2)
library(dplyr)

# 연도별 장르 빈도수 간단 시각화
genre_counts <- Game %>%
  group_by(Year, Genre) %>%
  summarize(count = n()) %>%
  ungroup()
ggplot(genre_counts, aes(x = Year, y = count, fill = Genre)) +
  geom_area() +
  labs(x = "Year", y = "Count", fill = "Genre") +
  theme_minimal()

# 카이제곱검정
genre_table <- table(Game$Year, Game$Genre)
chisq.test(genre_table)
# Pearson's Chi-squared test
# 
# data:  genre_table
# X-squared = 2836, df = 429, p-value < 2.2e-16


