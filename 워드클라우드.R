
url1 <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
keyword <- "콘솔게임"          


url2 <- "%EC%BD%98%EC%86%94%EA%B2%8C%EC%9E%84&sort=0&photo=0&field=0&pd=13&"
start_date <- "ds=2023.02.10&"     #기사가 올라온 날짜 범위
end_date <- "de=2023.05.11&"

url3 <- "cluster_rank=65&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:3m,a:all&start=1"
PAGE <- seq(from=1, by=10, length.out=50)     # 기사 페이지 범위


#크롤링
library(rvest)
library(httr)

result <- list()
n=1
for (i in PAGE) {
  url = paste0(url1, keyword, url2, start_date, end_date, url3, i)
  get_url = GET(url)
  
  title <- get_url %>%
    read_html(encoding="UTF-8") %>%
    html_nodes('.news_area') %>%
    html_nodes('a') %>%
    html_attr('title')
  
  title <- title[complete.cases(title)]
  
  result[[n]] <- title
  n=n+1
}

result <- unlist(result)
head(result)



#텍스트마이닝
library(tidytext)
library(tidyverse)
library(KoNLP)

result <- as_tibble(result)
result

result_tb <- result %>%
  unnest_tokens(input=value, output = word, token=SimplePos09) %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word,"/.*$"))%>%
  filter(str_length(word)>=2)
  
result_tb


#안쓸 단어 삭제
st_word <- tibble(word=c('비트코','삼국블레이드','1분기','롯데월드','체험존','어트랙션','삼성전자'))

result_tb <- result_tb %>%
  anti_join(st_word, by=c("word"="word")) %>%
  filter(!grepl(pattern="//d+", word))

result_tb <- result_tb %>%
  count(word, sort = TRUE)
resut_tb


#워드클라우드
library(wordcloud2)
 wordcloud2(result_tb, color = "random-light", fontFamily = "나눔바른고딕")
