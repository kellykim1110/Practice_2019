#현재 작업 디렉토리 확인
getwd()
setwd()

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_221")


install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")


library(rJava)
library(memoise)
library(KoNLP)
search()

##사전설정
useNIADic()

#readLines()는 데이터를 행으로 받아진다
#R은 기본적으로 ANSI를 읽을 수 있으므로 저장시 uft-8을 ANSI로 바꿔 저장
txt<-readLines("C:\\R\\Rproject2019\\\\SpiderMan.txt")
head(txt)
length(txt)
is(txt)

##문자열처리
install.packages("stringr")
library(stringr)

##특수문자 제거
#str_replace_all(df,정규표현식 표시 조사 포함 "\\W",'대체할 값')
#str_replace_all(txt,"\\영화"," ")
txt

##명사추출
extractNoun("오늘은 즐거운 날이다. 당신은 소중한 사람입니다.")
nouns<-extractNoun(txt)
nouns
is(nouns) #각 자료당 데이터 개수가 다르니까 
#한줄이 하나의 vector값으로 한 단어가 하나의 데이터값으로 각각 들어감

unlist(nouns)#그러므로 unlist()를 통해 모두 하나의 vector안에 각각의 데이터값으로 만들어줌
wordCount<-table(unlist(nouns))
wordCount

df_word<-as.data.frame(wordCount,stringsAsFactors = F) #전환시킬때 as 사용

nrow(df_word)
ncol(df_word)


##변수명 수정
library(dplyr)  #selct, filter, dply, mutate,rename이 팩캐지 dplyr 에 포함
df_word<-rename(df_word,word=Var1,freq=Freq)
df_word

##두글자 이상 단어 추출
df_word<-filter(df_word,nchar(word)>=2)
df_word


###워드 클라우드
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer) 

#색상 추출
pal<-brewer.pal(8,"Dark2")
set.seed(1004)  #난수1004로 고정시키기 #프로그램을 돌릴 때 마다 같은 것으로 고정시키기 위해

###제작
wordcloud(word=df_word$word,#단어
          freq=df_word$freq,#빈도
          colors = pal)     #색상 목록

wordcloud(word=df_word$word,#단어
          freq=df_word$freq,#빈도
          max.words = 100,  ##최대 표현 단어 수
          random.order=F,   ##고빈도 단어 중앙배치유무
          rot.per = 0.4,    ##회전 단어 비율 지정
          colors = pal)     #색상 목록


wordcloud(word=df_word$word,#단어
          freq=df_word$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
