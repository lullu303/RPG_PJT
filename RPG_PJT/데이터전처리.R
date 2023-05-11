# 데이터 불러오기
game <- read.csv("data/Video_Games.csv")
game

head(game,10)

# 데이터 세트의 전체 행/전체 열 수를 출력 
dim(game)
# [1] 16719    16

# 사본 만들기
df_game <- data.frame(game)

df_game

# 컬럼명 대소문자 섞여 있음 - 소문자로 변경
names(df_game) <- tolower(names(df_game))
head(df_game)
View(df_game)

# library(dplyr)

# Global_Sales 이 높은 순으로 정렬 
View(df_game %>% arrange(desc(Global_Sales)))

View(group_by(df_game,Publisher))

game_group <- df_game %>% 
  group_by(Publisher) %>% 
  arrange(desc(Global_Sales))

View(game_group)

# 결측치 파악
is.na(df_game)
table(is.na(df_game))
# FALSE   TRUE 
# 241211  26293 

# library(reshape2)
melt_DfGame <- melt(df_game,
                   id.vars = c("Name", "Genre"),
                   measure.vars = "Global_Sales", na.rm = TRUE) # 결측치 제거
View(melt_DfGame)


# library(ggplot2)
# library(scales)

head(melt_DfGame)


