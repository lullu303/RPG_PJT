getwd()
setwd('RPG_PJT')
list.files()

# 데이터셋 가져오기
game <- read.csv('Video_Games(정제).csv')
head(game)
str(game)

# 빈칸 및 이상치 NA로 변환
game[game == ""] <- NA
game[game == "tbd"] <- NA
dim(game)
# [1] 16716    13



# 결측치 파악
table(is.na(game))
#  FALSE   TRUE 
# 201416  15892 


# 시각화에 사용할 열 결측치 파악
table(is.na(game$Year)); table(is.na(game$Genre)) ; table(is.na(game$Global_))
# FALSE 
# 16716 
# FALSE 
# 16716 
# FALSE 
# 16716

# 유저평점 변수 형변환
game$U_Score <- as.numeric(game$U_Score)



# 각 연도별로 가장 많이 출시된 장르에 대한 막대그래프 그리기 --------------------------

# 출시년도, 장르만 담은 df생성
year_genre <- game[, c("Year", "Genre")]

library(reshape2)
# melt함수 이용해 장르 기준으로 연도 재구성하기
melted <- melt(year_genre, id.vars = "Genre", measure.vars = "Year")
head(melted)
dim(melted)

# aggregate함수로 장르와 연도를 기준으로 그룹화하여 빈도수 계산하고 df에 저장
genre_year_counts <- dcast(melted, Genre ~ value, fun.aggregate = length)
head(genre_year_counts)
dim(genre_year_counts)
# [1] 12 43


# 연도, 빈도수, 최대값만 넣은 df 따로 생성하기

# 1.빈 데이터프레임(칼럼 지정) 생성
max_genre_df <- data.frame(Year = integer(),
                           Genre = character(),
                           Max_Frequency = integer(),
                           stringsAsFactors = FALSE)

# 2. 연도, 빈도수 최대값, 장르 for문으로 추출
# 컬럼의 첫번째(장르)빼고
for (i in colnames(genre_year_counts)[-1]){
  year <- as.integer(i)  # year에 연도 정수형으로,
  max_frequency <- max(genre_year_counts[[i]])  # max_frequency에 최대값
  # max_genre에 최대값있는 장르 인덱싱
  max_genre <- genre_year_counts$Genre[which.max(genre_year_counts[[i]])]
  # 만들어뒀던 빈 df에 rbind
  max_genre_df <- rbind(max_genre_df, data.frame(Year = year,
                                                 Genre = max_genre,
                                                 Max_Frequency = max_frequency))
}
head(max_genre_df)
tail(max_genre_df)

# 패키지
library(ggplot2)
library(RColorBrewer)

# 연도별, 가장 많이 출시된 장르에 대한 막대그래프 그리기
ggplot(max_genre_df, aes(x = Year, y = Max_Frequency, fill = Genre)) +
  geom_bar(stat = "identity", lwd = 3) +
  scale_fill_manual(values = brewer.pal(n = length(unique(max_genre_df$Genre)), 
                                        name = "Set2")) +
  labs(title = "연도별, 가장 많이 출시된 장르(1977-2020)",
       x = "출시년도", y = "출시된 수") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(min(max_genre_df$Year), max(max_genre_df$Year), by = 1)) +
  scale_y_continuous(breaks = seq(0, 280, by = 10))
# 2017 이후 너무 급격하게 출시빈도가 줄은 것이 상식적이지 않음
# 수집한 데이터셋 자체에서 문제가 있는 것으로 간주하고
# 정확도를 위해 팀원들과 회의 후 2016년까지만 사용하기로


tail(max_genre_df)
# 뒤 41,42행 삭제 
max_genre_df <- max_genre_df[1:(nrow(max_genre_df) - 2), ]
# 다시 그래프 그리기
ggplot(max_genre_df, aes(x = Year, 
                         y = Max_Frequency, 
                         fill = Genre)) +
  
  geom_bar(stat = "identity", lwd = 3) +
  
  scale_fill_manual(values = brewer.pal(n = length(unique(max_genre_df$Genre)), 
                                        name = "Set2")) +
  
  labs(title = "연도별, 가장 많이 출시된 장르(1977-2016)",
       x = "출시년도", 
       y = "출시된 수") +
  
  theme_minimal() +
  
  theme(legend.position = "right",
        panel.grid.major = element_line(color = "gray", 
                                        linetype = "dashed", 
                                        size = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(min(max_genre_df$Year), 
                                  max(max_genre_df$Year), 
                                  by = 1)) +
  scale_y_continuous(breaks = seq(0, 280, by = 10))


# 연도별, 게임출시빈도 선 그래프 그리기---------------------------------------------------------------

# 연도별 출시빈도 df 만들기
release_frequency <- table(game$Year)
release_df <- data.frame(Year = as.numeric(names(release_frequency)),
                         Frequency = as.numeric(release_frequency))
str(release_df)
# 'data.frame':	42 obs. of  2 variables:
#   $ Year     : num  1977 1978 1979 1980 1981 ...
# $ Frequency: num  3 6 2 13 46 37 17 14 13 21 ...
head(release_df)
tail(release_df)
# 2017,2020행 삭제
release_df <- release_df[1:(nrow(release_df) - 2), ]

# 선 그래프
ggplot(release_df, aes(x = Year, 
                       y = Frequency, 
                       group = 1, 
                       color = "게임출시빈도")) +
  geom_path(size = 1.5) +
  labs(title = "연도별, 비디오게임 출시(1977-2016)", 
       x = "출시년도", 
       y = "출시된 비디오게임 수") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(release_df$Year), 
                                  max(release_df$Year), 
                                  by = 5)) +
  scale_y_continuous(breaks = seq(0, max(release_df$Frequency), 
                                  by = 100)) +
  scale_color_manual(values = brewer.pal(n = 1, name = "Set2"), guide = guide_legend(title = NULL)) +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5)) +
  theme(legend.position = "right")



# 출시연도별, 누적매출이 가장 높은 장르 및 해당장르의 매출합계 막대그래프-----------------------------------

# 패키지
library(dplyr)
library(ggplot2)
head(game)

# year_sale_dx에 출시년도/장르 그룹화 후, 그룹별 매출합계를 구해서 넣기 
year_sale_dx <- game %>% 
  group_by(Year, Genre) %>% 
  summarize(Global_ = sum(Global_), .groups = "drop") 
View(year_sale_dx)

# year_sale에 출시년도별로 매출가장 높은 장르만 추출해서 넣기
year_sale <- year_sale_dx %>% 
  group_by(Year) %>% 
  mutate(year_max = max(Global_),
         genre = Genre[Global_ == year_max]) %>% 
  ungroup() %>% 
  filter(Global_ == year_max) %>% 
  select(-year_max)
View(year_sale)
summary(year_sale)
# # 뒤 41,42행 삭제 (2017,2020년 행)
# year_sale <- year_sale[1:(nrow(year_sale) - 2), ]
# tail(year_sale)

# text 포함 막대그래프
ggplot(year_sale, aes(x = Year, y = Global_, fill = genre)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  geom_text(aes(label = paste(genre, "--", round(Global_, 2))),
            vjust = 0.1, hjust = -0.1, size = 3.8, color = "black", angle = 90) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "출시연도별, 누적매출이 가장 높은 장르 및 해당장르의 매출합계(1977-2016)",
       x = "출시년도", y = "매출합계(장르별)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "right",
        panel.grid.major.y = element_line(color = "gray", 
                                          linetype = "dashed", 
                                          size = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(year_sale$Global_), by = 10))+
  scale_x_continuous(breaks = seq(0, max(year_sale$Year), by = 1))





#---------------------------------------------------------------------------------------------
# 출시연도, 장르, 전세계총판매량 변수를 이용해서 나무지도로 시각화해보기
library(treemap)

# 연도/장르별 전체 글로벌 판매량 계산한 데이터 만들기
sales_data <- aggregate(Global_ ~ Year + Genre, data = game, sum)
View(sales_data)
# 연도-장르 계층으로 구성된 나무지도 그리기
tree_map <- treemap(
  sales_data,
  index = c("Year", "Genre"),
  vSize = "Global_",# 타일크기 = 판매량
  type = "value",
  vColor = "Global_",# 타일색상 = 판매량
  palette = "RdYlBu",
  title = "Global Sales",
  fontsize.labels = 10
)

print(tree_map)

summary(sales_data)

#-------------------------------------------------------------------------------
# 다중산점도로 변수간의 상관계수 확인하기
str(game)

game <- read.csv('Video_Games(정제).csv')
game[game == ""] <- NA
game[game == "tbd"] <- NA
game$U_Score <- as.numeric(game$U_Score)
# U_Score 열의 NA 값을 최저값으로 변경
game$U_Score[is.na(game$U_Score)] <- min(game$U_Score, na.rm = TRUE)

# 수치형변수들 다중산점도#####################################################
variables <- game[c("NA_", "EU_", "JP_", "Other_", "Global_", "C_Score","U_Score")]

# 피어슨상관계수 매트릭스 생성
cor_matrix <- cor(variables, method = "pearson")

# 다중산점도 그리기
pairs(variables, 
      upper.panel = panel.smooth, 
      lower.panel = NULL,  
      diag.panel = NULL,  
      labels = colnames(cor_matrix),  
      main = ""
)




# pair_data에 상관관계 알아볼 변수들 넣고 팩터 변환하기
pair_data <- game[c("Platform","Genre","Publisher","Year","Global_","C_Score","U_Score")]
# 팩터변환
pair_data$Platform <- as.factor(pair_data$Platform)
pair_data$Genre <- as.factor(pair_data$Genre)
pair_data$Publisher <- as.factor(pair_data$Publisher)




# pair_data 변수들 간의 산점도 행렬 그리기######################################## 
# install.packages('psych')
library(psych)

pairs.panels(pair_data,
             method = "pearson",  # 피어슨상관계수로 플롯생성
             hist.col = "lightblue",  # 변수의 분포 히스토그램으로 표시
             density = TRUE,  # 밀도 플롯(대각선요소에서 변수 밀도 추정치 표시)
             ellipses = TRUE  # 상삼각요소에서 변수들 간 이원 정규 분포의 등고선 표시
)


#############################################################
# 수치형변수만 선택 후 다중산점도 그리기
numeric_vars <- game[c("Year", "Global_", "C_Score", "U_Score")]

# 다중산점도
pairs(numeric_vars, 
      upper.panel = panel.smooth,  
      lower.panel = NULL, 
      diag.panel = NULL,  
      labels = colnames(numeric_vars),  
      main = "출시년도,매출,전문가평점,유저평점 다중산점도"
)






###############################################################################################
# 기술통계
game <- read.csv('Video_Games(연평균).csv')
game[game == ""] <- NA
game[game == "tbd"] <- NA
game$U_Score <- as.numeric(game$U_Score) # 형변환
# U_Score 열의 NA 값을 최저값으로 변경
game$U_Score[is.na(game$U_Score)] <- min(game$U_Score, na.rm = TRUE)


# 숫자형 변수의 기술통계
variables <- c("Year", "NA_", "EU_", "JP_", "Other_", "Global_", "C_Score", "U_Score")

for (variable in variables) {
  cat("Variable:", variable, "\n")
  cat("최소값:", min(game[[variable]]), "\n")
  cat("1사분위수:", quantile(game[[variable]], 0.25), "\n")
  cat("중앙값:", median(game[[variable]]), "\n")
  cat("3사분위수:", quantile(game[[variable]], 0.75), "\n")
  cat("최대값:", max(game[[variable]]), "\n")
  cat("평균값:", mean(game[[variable]], na.rm = TRUE), "\n")
  cat("최빈값:", names(table(game[[variable]]))[table(game[[variable]]) == max(table(game[[variable]]))], "\n")
  cat("분산:", var(game[[variable]], na.rm = TRUE), "\n")
  cat("표준편차:", sd(game[[variable]], na.rm = TRUE), "\n\n")
}



# 범주형 변수의 기술통계 - 빈도수 (publisher 너무 많아서 제외시킴)
categorical_variables <- c("Platform", "Genre")

for (variable in categorical_variables) {
  cat("Variable:", variable, "\n")
  cat("빈도수:", "\n")
  freq_table <- table(game[[variable]])
  print(freq_table)
  cat("\n")
}
# 범주개수
categorical_variables <- c("Platform", "Genre", "Publisher")

for (variable in categorical_variables) {
  cat("Variable:", variable, "\n")
  unique_values <- unique(game[[variable]])
  num_categories <- length(unique_values)
  cat("범주 개수:", num_categories, "\n\n")
}


# 왜도,첨도,신뢰구간 (모든변수)

# 왜도와 첨도 계산을 위해 moments 패키지 설치
# install.packages("moments")

# psych 패키지 로드
library(psych)
# moments 패키지 로드
library(moments)

# 수치형변수 따로담기
game_variables <- c("Year", "NA_", "EU_", "JP_", "Other_", "Global_", "C_Score", "U_Score")

# 변수별 왜도와 첨도 계산
for (variable in game_variables) {
  cat("Variable:", variable, "\n")
  skew <- skewness(game[[variable]])
  kurt <- kurtosis(game[[variable]])
  cat("Skewness:", skew, "\n")
  cat("Kurtosis:", kurt, "\n\n")
}

# 변수별 기술통계량과 신뢰구간 계산
describe(game[game_variables])







# 두 번째 정제 이전에 썼던 코드-------------------------------------------------------------------------------
# # 출시연도,장르컬럼 NA값 있는 행 삭제(원본데이터에 적용했음)
# game <- game[complete.cases(game[, c("Year", "Genre")]), ]
# table(is.na(game$Year_of_Release)); table(is.na(game$Genre))
# # FALSE 
# # 16448 
# # FALSE 
# # 16448 
# 
# dim(game)
# # 16448    11
# colnames(game)
# # [1] "Name"            "Platform"        "Year_of_Release" "Genre"           "Publisher"       "NA_Sales"       
# # [7] "EU_Sales"        "JP_Sales"        "Other_Sales"     "Global_Sales"    "User_Score"
# str(game)
# 
# View(game)

# 연도별 출시된 게임 수 막대그래프
# # 라이브러리 로드
# library(ggplot2)
# 
# # Year_of_Release 열의 빈도 계산
# year_freq <- table(game$Year_of_Release)
# 
# # 빈도 막대 그래프 생성
# ggplot(data = data.frame(Year_of_Release = names(year_freq), Freq = as.numeric(year_freq)),
#        aes(x = Year_of_Release, y = Freq)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   xlab("연도") +
#   ylab("게임 수") +
#   ggtitle("연도별 출시된 게임 수") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# # 비디오게임의 단순 출시빈도에 대한 그래프를 확인해보았을 때, 
# # 93년도부터 가파른 성장세를 보이기 시작하면서 08년 정점을 찍은 후 출시하지 않는 추세이다.


# -------------------------------------------------------------------------------------------------------------


# # 장르와 연도 추출하여 새로운 데이터프레임 생성
# year_genre <- game[, c("Year_of_Release", "Genre")]
# 
# 
# library(reshape2)
# # melt함수 이용해 장르 기준으로 연도 재구성하기
# melted <- melt(year_genre, id.vars = "Genre", measure.vars = "Year_of_Release")
# head(melted)
# # aggregate함수로 장르와 연도를 기준으로 그룹화하여 빈도수 계산하고 df에 저장 
# genre_year_counts <- dcast(melted, Genre ~ value, fun.aggregate = length)
# 
# View(genre_year_counts)
# 
# str(genre_year_counts)
# 
# # 장르총합, 연도총합 칼럼 및 행 생성 ------------삭제했음######################
# # 장르빈도 총합 칼럼 생성
# genre_year_counts$Total_genre <- rowSums(genre_year_counts[, -1])
# 
# 
# 
# # 연도별 출시빈도 총합 행 생성
# # 1980~2020년도까지의 칼럼 범위 지정
# year_columns <- colnames(genre_year_counts)[2:(ncol(genre_year_counts))]
# year_columns
# 
# # 1980~2020년까지의 칼럼들의 합 구하기
# 
# year_sums <- colSums(genre_year_counts[, year_columns])
# year_sums
# 
# 
# # 칼럼의 합을 맨 아래에 추가
# genre_year_counts <- rbind(genre_year_counts, "Column_Total" = c("Total_year", year_sums, NA))
# View(genre_year_counts)




# ------------------------------------------------------------------------------------------------------
# # 컬러파레트 지정
# library(RColorBrewer)
# num_genres <- length(unique(max_genre))
# genre_colors <- brewer.pal(num_genres, "Set2")
# 
# # 여백설정
# par(mar = c(5, 5, 4, 1))
# 
# # 그래프 분할
# par(mfrow = c(2, 1))
# 
# 
# # 선그래프 ---------------------------------------------------------------
# 
# # 연도별 출시빈도 카운트해서 변수에 저장
# release_frequency <- table(game$Year_of_Release)
# release_frequency
# # 선그래프 그리기
# plot(release_frequency, type = "o", main = "비디오게임 연도별 출시 빈도(1980-2020)",
#      col = genre_colors, pch = 16, ylim = c(0, 1450), 
#      xlab = "year", ylab = "Frequency", axes = FALSE)
# axis(1, at = seq(1980, 2020, 5), las = 2)
# axis(2, las = 1)
# box(lwd = 2)
# grid(lwd = 0.5)
# # 범례추가
# legend(x = "topleft", legend = c("total Frequency"), fill = genre_colors, border = NA, 
#        x.intersp = 0.4, y.intersp = 0.4, xjust = 1, yjust = 1, text.font = 1)
# 
# 
# 
# 
# # 막대그래프 -----------------------------------------------------------
# # 각 연도별로 최대값이 있는 행의 인덱스를 찾아서 저장
# 
# # y리밋계산
# y_max <- ceiling(max(max_frequency) / 10) * 10
# 
# # x리밋계산
# x_max <- ncol(genre_counts)
# 
# # 막대그래프 그리기
# barplot(max_frequency, xlab = "year", ylab = "Frequency", col = genre_colors, 
#         ylim = c(0, y_max), axes = FALSE)
# axis(1, at = seq(1, x_max, 5), labels = seq(1980, 2020, 5), las = 2)
# axis(2, at = seq(0, y_max, 10), las = 1)
# box(lwd = 2)
# grid(lwd = 0.5)
# 
# # 범례
# legend(x = "topleft", legend = c(unique(max_genre)), fill = c(genre_colors), border = NA, 
#        x.intersp = 0.7, y.intersp = 0.7, xjust = 0, yjust = 1, text.font = 1)
# 
# 
# # 분할 원상태로 리셋
# par(mfrow = c(1, 1))
# 
# 
# 
# 
# 
# 
# # 연도별로 가장 많이 출시된 장르가 막대그래프를 보니 생각보다 
# # 한 장르가 독점하기 보단 다양하게 나온거같아서 
# # 연도별로 1등-출시-장르 파이그래프로 시각화해보기-----------------
# str(max_genre)
# 
# # 
# genre_frequency <- table(max_genre)
# 
# # 여백설정
# par(mar = c(5, 5, 4, 6))
# # Plot the pie chart with genre colors and frequency labels
# pie(genre_frequency, col = genre_colors, main = "Genre Distribution",
#     labels = paste0(names(genre_frequency), "\n", genre_frequency))
# 
# # Add a legend
# legend("topright", legend = names(genre_frequency), fill = genre_colors, border = NA, 
#        x.intersp = 1, y.intersp = 1, xjust = 1, yjust = 1)
# 
# 
# 
#
#### ㅋㅋㅋ위에 개뻘짓함 아ㅠㅠㅋㅋㅋㅋㅋㅋ

### barplot이나 plot사용않고 ggplot으로 그냥 그리기..------------------------------------------------------











# ### 시각화 더 다양하게 시도 --------------
# 
# 
# year_sale$Year_of_Release <- as.numeric(year_sale$Year_of_Release)
# 
# # 누적영역 
# ggplot(year_sale, aes(x = Year_of_Release, y = Global_Sales, fill = genre)) +
#   geom_area(color = "black") +
#   labs(title = "Global Sales of Genres Over the Years",
#        x = "Year", y = "Global Sales") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal() +
#   theme(legend.position = "right",
#         panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(breaks = seq(min(year_sale$Year_of_Release), max(year_sale$Year_of_Release), by = 5)) +
#   scale_y_continuous(breaks = seq(0, max(year_sale$Global_Sales), by = 10))
# 
# 
# 
# 
# # 여러 선 그래프
# ggplot(year_sale, aes(x = Year_of_Release, y = Global_Sales, color = genre)) +
#   geom_line(size = 1) +
#   labs(title = "Global Sales of Genres Over the Years",
#        x = "Year", y = "Global Sales") +
#   scale_color_brewer(palette = "Set2") +
#   theme_minimal() +
#   theme(legend.position = "right",
#         panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(breaks = seq(min(year_sale$Year_of_Release), max(year_sale$Year_of_Release), by = 5)) +
#   scale_y_continuous(breaks = seq(0, max(year_sale$Global_Sales), by = 10))
# 
# 
# 
# 
# 
# # 히트맵---------------수정해야함. 너무 졸려서 못하겠음..
# library(tidyverse)
# 
# # 칼럼추출
# comp_genre <- game[, c("Genre", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")]
# 
# # long형으로 변환
# comp_map <- comp_genre %>%
#   pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales),
#                names_to = "Region",
#                values_to = "Sales")
# 
# 
# # 히트맵
# ggplot(comp_map, aes(x = Genre, y = Region, fill = Sales)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = sprintf("%.1f", Sales)), color = "black") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue",
#                       limits = c(0, max(comp_map$Sales))) +
#   labs(title = "Sales Comparison by Genre",
#        x = "Genre",
#        y = "Region",
#        fill = "Sales") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
#         axis.text.y = element_text(size = 12),
#         plot.title = element_text(size = 16, hjust = 0.5),
#         legend.position = "right",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##############제일 처음에 그렸던 그래프#####################################
# # 출시년도 및 장르만 담은 DF 생성
# game_YG <- game[,c("Year_of_Release","Genre")]
# View(game_YG)
# dim(game_YG)
# # 결측치 파악
# table(is.na(game_YG))
# # FALSE  TRUE 
# # 33436     2
# 
# # N/A, NA가 있어서 수정
# game_YG[game_YG == "N/A"] <- NA
# # 결측치 파악
# table(is.na(game_YG))
# # FALSE  TRUE 
# # 33167   271 
# table(game_YG)
# 
# 
# game_y_freq <- data.frame(table(game_YG))
# View(game_y_freq)
# dim(game_y_freq)
# str(game_y_freq)
# 
# # tidyverse 패키지 불러오기
# library(tidyverse)
# 
# # wide format으로 변환
# game_y_wide <- game_y_freq %>%
#   pivot_wider(names_from = Genre, values_from = Freq, values_fill = 0)
# 
# View(game_y_wide)
# str(game_y_wide)
# 
# 
# # ggplot2 패키지 불러오기
# library(ggplot2)
# library(tidyr)
# library(RColorBrewer)
# 
# game_y_long <- game_y_wide %>%
#   gather(key = "Genre", value = "Frequency", -Year_of_Release)
# 
# # Set3 팔레트사용
# color_palette <- brewer.pal(n = length(unique(game_y_long$Genre)), name = "Set3")
# 
# # 연도별 게임 흐름 누적막대그래프
# ggplot(game_y_long, aes(x = Year_of_Release, y = Frequency, fill = Genre)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = color_palette) +
#   labs(title = "Yearly Genre Flow", x = "Year", y = "Frequency") +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 






