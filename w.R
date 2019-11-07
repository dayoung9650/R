# ************************************************
# -- 감성 분석(단어의 긍정/부정 분석) 
#    시각화 : 파랑/빨강/녹색 
#    input: 저장 파일 경로
#           저장 파일명
# ************************************************
# ------------------------------
# -- 1) 데이터 가져오기("C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R") 
# ------------------------------
setwd("C:/1_RPA/WikhanREFrameWorkQueueEnhanced/Data/R")
data = readLines("input/조국.txt", encoding="UTF-8")
head(data)
# localeToCharset()
# Encoding(data)
# data<-iconv(data,'UTF-8')
# Encoding(data)
data<- gsub('[[:punct:]]', '', data) # 문장부호 제거
data <- gsub('[[:cntrl:]]', '', data) # 특수문자 제거

head(data)

# -- (1) 문자열 처리를 위한 패키지 로딩 
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
useSejongDic()
# install.packages("KoNLP")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("slam")
# ------------------------------
# -- 2) 대상 텍스트 분석
# ------------------------------
# -- (1) 명사 추출
extractNoun(data)
nouns = sapply(data, extractNoun, USE.NAMES=F)
# typeof(nouns)
# head(unlist(nouns),20)
nouns = unlist(nouns)
nouns = Filter(function(x){nchar(x) >= 2}, nouns) #1글자 이하인 명사 삭제
# head(unlist(nouns),20)
unlist(nouns)
write(unlist(nouns), 'wc.txt') 
wc_data = read.table('wc.txt')
# typeof(wc_data)
# nrow(wc_data)

# -- (2) wordcount 데이터 준비
wordcount = table(wc_data)
write(unlist(wordcount), 'wordcount.txt')
head(sort(wordcount, decreasing=F), 100)

# -- (3) wordcount 색상 정의
palette = brewer.pal(8, "Set2")

# ------------------------------
# -- 3) 대상 경로에 이미지 저장
# ------------------------------
png(filename="Output/wordcloud.png"
, width=800, height=600)
wordcloud(names(wordcount), freq = wordcount, scale=c(15,2), rot.per = 0.25, random.order=F, colors=palette, random.color=T)
dev.off()