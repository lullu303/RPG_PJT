getwd()
setwd('RPG_PJT')
setwd('현황')
list.files()
setwd('../')
getwd()



# txt파일 내용 다 병합하기 ---------------------------------------------
# 폴더경로설정(for문으로 txt파일 내용 다 긁어올거임)
folder_path <- file.path("C:/RStudy/RPG_PJT/현황")

# 파일리스트 가져오기
file_names <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
file_names



# 빈벡터공간 세팅
merged_text <- character()
# 각 파일 내용 다 가져오기
for (file_path in file_names) {
  file_text <- readLines(file_path, encoding = "UTF-8")
  merged_text <- c(merged_text, file_text)
}
# 확인
cat(merged_text, sep='\n')
class(merged_text)
str(merged_text)

# df로 저장
df <- data.frame(merged_text)


# 파일경로 설정하고 사용하기
output_file <- "merged_text.csv"
# csv파일로 저장해놓기
write.csv(df, file = output_file, row.names = FALSE)


library(KoNLP)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(wordcloud)
useSejongDic()

status <- merged_text

class(status)
str(status)


install.packages("tm")
library(tm)
# 텍스트 전처리
status <- gsub("[[:punct:]]", "", status) # 문장부호 제거
#status <- gsub("[[:digit:]]", "", status) # 숫자 제거
status <- tolower(status) # 모든 문자를 소문자로 변환
status <- removeWords(status, stopwords("english")) # 불용어 제거
status <- removeWords(status, c("미디어","등","출시","자본시장","capital","2022년","실적","및","한국","중국","조선비즈","모바일","공략","이유","byline","국내","1분기","글로벌","비디오","매출","게임업계","2023","2023년","2022","신작","콘솔","콘솔로","pc콘솔","게임시장","게임사","인기","콘솔","시장","콘솔게임","비디오게임","게임","게임산업", "업무","위해", "노력하겠다", "기회를", "만들자", "개막", "경기도n", "경기도nhn", "경기도와", "경기도의", "경기도에서", "경기도는", "경기도에서는", "경기도의", "경기도는", "경기도에는","핵심을", "한","게임문화", "조성을", "위한", "미래산업의","산업","필요", "의원", "보유", "직전", "대형","분위기", "바뀌나","웹", "해외","지원","과제","성적표", "만", "웃었다","거듭", "이해충돌", "논란", "가상", "자산", "공개", "논의", "본격화","미르m","이용자","실시"))
status <- stripWhitespace(status)

wordcloud(status, scale=c(3,.3), min.freq=6, random.order=TRUE, 
          rot.per=.12, colors=brewer.pal(8, "Dark2"))


getwd()



### 전망도 위와 똑같이 진행해보기 ########################################################
folder_path2 <- "C:/RStudy/RPG_PJT/전망"


# txt내용병합 후 df저장
file_names2 <- list.files(folder_path2, pattern = "\\.txt$", full.names = TRUE)
merged_text2 <- character()
for (file_path2 in file_names2) {
  file_text2 <- readLines(file_path2, encoding = "UTF-8")
  merged_text2 <- append(merged_text2, file_text2)
}
df <- data.frame(merged_text2)


# csv 저장
output_file2 <- "전망 헤드라인.csv"
write.csv(df, file = output_file2, row.names = FALSE, encoding = 'UTF-8')


cat(merged_text2, sep='\n')
status2 <- merged_text2
str(status2)



#install.packages("tm")
library(tm)


# 텍스트 전처리
status <- gsub("[[:punct:]]", "", status) # 문장부호 제거
status <- tolower(status) # 모든 문자를 소문자로 변환
status <- removeWords(status, stopwords("english")) # 불용어 제거
status <- removeWords(status, c("미디어","등","출시","자본시장","capital","2022년","실적","및","한국","중국","조선비즈","모바일","공략","이유","byline","국내","1분기","글로벌","비디오","매출","게임업계","2023","2023년","2022","신작","콘솔","콘솔로","pc콘솔","게임시장","게임사","인기","콘솔","시장","콘솔게임","비디오게임","게임","게임산업", "업무","위해", "노력하겠다", "기회를", "만들자", "개막", "경기도n", "경기도nhn", "경기도와", "경기도의", "경기도에서", "경기도는", "경기도에서는", "경기도의", "경기도는", "경기도에는","핵심을", "한","게임문화", "조성을", "위한", "미래산업의","산업","필요", "의원", "보유", "직전", "대형","분위기", "바뀌나","웹", "해외","지원","과제","성적표", "만", "웃었다","거듭", "이해충돌", "논란", "가상", "자산", "공개", "논의", "본격화","미르m","이용자","실시"))
status <- removeWords(status, c("억","위메이드","플레이엑스포","조","올해","분기","성장"))
status <- removeNumbers(status)
status <- stripWhitespace(status)

wordcloud(status, scale=c(3,0.8), min.freq=7, 
          random.order=TRUE, rot.per=.12, 
          colors=brewer.pal(8, "Dark2"))

#...흠....마음에 안 드는군..
