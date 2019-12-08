#영화 '재심'에 대한 리뷰 분석

library(wordcloud)
library(KoNLP)

useSejongDic()

#데이터 불러오기
fname='C:\\Users\\Desktop\\RBasic-master\\R_Data\\30_AgainCourt.txt'
road1=readLines(fname)

##아래 데이터는 직접 긍정단어, 부정단어를 찾아 나눈 것이다
pos.words = scan("C:\\Users\\Desktop\\RBasic-master\\R_Data\\pos_kor_words.txt", what="character", comment.char=";")
neg.words = scan("C:\\Users\\202-006\\Desktop\\RBasic-master\\R_Data\\neg_kor_words.txt", what="character", comment.char=";")


#### 의미 없는 데이터 제거, 단어 발생 빈도 등을 기반으로 감성 분석 값을 반환.
require(plyr)

sentence <- road1

# 구두점 문자, ! " # $ % & ’ ( ) * + , - . / : ; < = > ? @ [  ] ^ _ ` { | } ~. 제거
sentence <- gsub('[[:punct:]]', "", sentence)  
# \n, \r 같은 제어문자 등 제거
sentence <- gsub('[[:cntrl:]]', "", sentence)
# 숫자 제거
sentence <- gsub('\\d+', "", sentence)
head(sentence)

length(sentence)
# ,autoSpacing = TRUE
#### 명사 추출
wordlist <- sapply(sentence, extractNoun, USE.NAMES=F)
words <- unlist(wordlist)   # 단어를 하나의 벡터로 
head(words)

##감성 분석을 위한 점수 확인
pos.matches <- match(words, pos.words)  ## 긍정단어, 부정단어 확인
neg.matches <- match(words, neg.words)  ## 단어 존재(사전에서 위치), 없으면 NA
pos.matches <- !is.na(pos.matches)  # NA가 아닌것 가져오기(문장에 단어 있음)
neg.matches <- !is.na(neg.matches)

sum(pos.matches)  # 점수 합(긍정단어)
sum(neg.matches)  # 점수 합(부정단어) 

score <- sum(pos.matches) - sum(neg.matches)
score 

#단어 확인 및 빈도에 따른 정렬
pos_word <- words[pos.matches ]
neg_word <- words[neg.matches ]

pos_cnt <- table(pos_word)
neg_cnt <- table(neg_word)

pos_cnt_sort <- sort(pos_cnt, decreasing = T)
neg_cnt_sort <- sort(neg_cnt, decreasing = T)
pos_cnt_sort
neg_cnt_sort


## TOP 5 단어 그래프 보기
barplot(pos_cnt_sort[5:0], main='긍정 단어 TOP 5', horiz=T, col=c("green", "blue", "yellow"))


install.packages("wordcloud2")
library(wordcloud2)

library(RColorBrewer)

palette=brewer.pal(8,"Set2")

#부정어 워드클라우드
base_word1<-as.data.frame(neg_cnt,stringsAsFactors = F)
wordcloud2(data=base_word1,fontFamily = "나눔바른고딕")

#긍정어 워드클라우드
base_word2<-as.data.frame(pos_cnt,stringsAsFactors = F)
wordcloud2(data=base_word2,fontFamily = "나눔바른고딕")
