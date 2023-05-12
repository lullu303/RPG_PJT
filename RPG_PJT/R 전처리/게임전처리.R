#install packages for text mining
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("extrafont")
# install.packages("tidytext")

library('tidyverse')
library('reshape2')
library('wordcloud')
library('rJava') 
library('KoNLP')
library("dplyr")
library("stringr")
library("RColorBrewer")
library("tidytext")
# useSejongDic()

getwd()
setwd("D:/RPG_PJT")

#open file
NEWS <- read.csv("data/NaverNews_인기 콘솔게임.csv", header=TRUE, sep=",", encoding="UTF-8")
View(NEWS)
dim(NEWS) # [1] 359   3

text <- sapply(NEWS,extractNoun,USE.NAMES = F)

data <- unlist(text)

data <- Filter(function(x){nchar(x)>=2},data)


txt <- str_replace_all(data, "\t\t\t\t\t\t\t\t\t\t", " ")

nouns <- extractNoun(txt)

df_word <- as.data.frame(wordcount, stringAsFactors=F)

View(df_word)




