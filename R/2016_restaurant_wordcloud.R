#데이터 가져오기
##공공데이터 포털에서 ‘국내 음식점 정보’를 검색 후, 데이터 셋을 다운로드
##https://www.data.go.kr/dataset/15003419/fileData.do
df<-read.csv("C:\\Users\\Downloads\\2016_restaurant.csv")
str(df)
table(df$카테고리3)
table(df$지역명)
table(df$시군구명)

library(dplyr)
#특이한 카테고리명을 알아보기 위해 데이터 추출
df1<-df%>%filter(카테고리3=="이색음식점")
df2<-df%>%filter(카테고리3=="채식전문점")

df1$제목
df2$제목

#시각화
library(ggplot2)
ggplot(data=df1,aes(x=지역명))+geom_bar() #서울 경기도 경상북도 인천 전라북도 제주도 충남
ggplot(data=df2,aes(x=지역명))+geom_bar()  #서울 경기도 경상북도 전라북도

#상위 4개의 지역명 데이터 추출
df3<-df1%>%filter(지역명=="서울")
df4<-df1%>%filter(지역명=="경기도")
df5<-df1%>%filter(지역명=="제주도")
df6<-df1%>%filter(지역명=="인천")

ggplot(data=df3,aes(x=시군구명))+geom_bar() #서울 중구
#ggplot(data=df4,aes(x=시군구명))+geom_bar() 
ggplot(data=df5,aes(x=시군구명))+geom_bar() #제주시
ggplot(data=df6,aes(x=시군구명))+geom_bar() #인천 중구



df7<-df2%>%filter(지역명=="서울")
df8<-df2%>%filter(지역명=="경기도")
df9<-df2%>%filter(지역명=="경상북도")
df0<-df2%>%filter(지역명=="전라북도")


ggplot(data=df7,aes(x=시군구명))+geom_bar() #서울 종로구
ggplot(data=df8,aes(x=시군구명))+geom_bar() #성남
ggplot(data=df9,aes(x=시군구명))+geom_bar() #경주
ggplot(data=df0,aes(x=시군구명))+geom_bar() #전주


#Wordcloud
df<-readLines("C:\\Users\\Downloads\\2016_restaurant.csv")

library(rJava)
library(memoise)
library(KoNLP)
search()

##사전설정
useNIADic()

#readLines()는 데이터를 행으로 받아진다
#R은 기본적으로 ANSI를 읽을 수 있으므로 저장시 uft-8을 ANSI로 바꿔 저장

##문자열처리
#install.packages("stringr")
library(stringr)

##특수문자 제거
txt1<-str_replace_all(df3$개요,"\\W",' ')
txt2<-str_replace_all(df5$개요,"\\W",' ')
txt3<-str_replace_all(df6$개요,"\\W",' ')
txt4<-str_replace_all(df7$개요,"\\W",' ')
txt5<-str_replace_all(df8$개요,"\\W",' ')
txt6<-str_replace_all(df9$개요,"\\W",' ')
txt7<-str_replace_all(df0$개요,"\\W",' ')
#str_replace_all(txt,"\\영화"," ")



##명사추출
#extractNoun("오늘은 즐거운 날이다. 당신은 소중한 사람입니다.")
nouns1<-extractNoun(txt1)
nouns1
nouns2<-extractNoun(txt2)
nouns2
nouns3<-extractNoun(txt3)
nouns3
nouns4<-extractNoun(txt4)
nouns4
nouns5<-extractNoun(txt5)
nouns5
nouns6<-extractNoun(txt6)
nouns6
nouns7<-extractNoun(txt7)
nouns7

 #각 자료당 데이터 개수가 다르니까 
#한줄이 하나의 vector값으로 한 단어가 하나의 데이터값으로 각각 들어감
is(nouns1)
is(nouns2)
is(nouns3)
is(nouns4)
is(nouns5)
is(nouns6)
is(nouns7)

unlist(nouns1)#그러므로 unlist()를 통해 모두 하나의 vector안에 각각의 데이터값으로 만들어줌
unlist(nouns2)
unlist(nouns3)
unlist(nouns4)
unlist(nouns5)
unlist(nouns6)
unlist(nouns7)

wordCount1<-table(unlist(nouns1))
wordCount2<-table(unlist(nouns2))
wordCount3<-table(unlist(nouns3))
wordCount4<-table(unlist(nouns4))
wordCount5<-table(unlist(nouns5))
wordCount6<-table(unlist(nouns6))
wordCount7<-table(unlist(nouns7))
#wordCount

df_word1<-as.data.frame(wordCount1,stringsAsFactors = F) #전환시킬때 as 사용
df_word2<-as.data.frame(wordCount2,stringsAsFactors = F) #전환시킬때 as 사용
df_word3<-as.data.frame(wordCount3,stringsAsFactors = F) #전환시킬때 as 사용
df_word4<-as.data.frame(wordCount4,stringsAsFactors = F) #전환시킬때 as 사용
df_word5<-as.data.frame(wordCount5,stringsAsFactors = F) #전환시킬때 as 사용
df_word6<-as.data.frame(wordCount6,stringsAsFactors = F) #전환시킬때 as 사용
df_word7<-as.data.frame(wordCount7,stringsAsFactors = F) #전환시킬때 as 사용


##변수명 수정
library(dplyr)  #selct, filter, dply, mutate,rename이 팩캐지 dplyr 에 포함
df_word1<-rename(df_word1,word=Var1,freq=Freq)
df_word2<-rename(df_word2,word=Var1,freq=Freq)
df_word3<-rename(df_word3,word=Var1,freq=Freq)
df_word4<-rename(df_word4,word=Var1,freq=Freq)
df_word5<-rename(df_word5,word=Var1,freq=Freq)
df_word6<-rename(df_word6,word=Var1,freq=Freq)
df_word7<-rename(df_word7,word=Var1,freq=Freq)



##두글자 이상 단어 추출
df_word1<-filter(df_word1,nchar(word)>=2)
df_word2<-filter(df_word2,nchar(word)>=2)
df_word3<-filter(df_word3,nchar(word)>=2)
df_word4<-filter(df_word4,nchar(word)>=2)
df_word5<-filter(df_word5,nchar(word)>=2)
df_word6<-filter(df_word6,nchar(word)>=2)
df_word7<-filter(df_word7,nchar(word)>=2)

###워드 클라우드
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer) 

#색상 추출
pal<-brewer.pal(8,"Dark2")
set.seed(1004)  #난수1004로 고정시키기 #프로그램을 돌릴 때 마다 같은 것으로 고정시키기 위해

###제작

#이색음식점에서 상위 3위에 있는 지역중 가장 많은 식당을 가진 시군구

#서울 중구
wordcloud(word=df_word1$word,#단어
          freq=df_word1$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
#제주 제주시
wordcloud(word=df_word2$word,#단어
          freq=df_word2$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
#인천 중구
wordcloud(word=df_word3$word,#단어
          freq=df_word3$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록






#채식음식점에 있는 지역중 가장 많은 식당을 가진 시군구

#서울 종로구
wordcloud(word=df_word4$word,#단어
          freq=df_word4$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
#경기도 성남시
wordcloud(word=df_word5$word,#단어
          freq=df_word5$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
#경북 경주
wordcloud(word=df_word6$word,#단어
          freq=df_word6$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록
#전북 전주
wordcloud(word=df_word7$word,#단어
          freq=df_word7$freq,#빈도
          min.freq=2,       ###최소 단어 빈도
          max.words = 100,  #최대 표현 단어 수
          random.order=F,   #고빈도 단어 중앙배치유무
          rot.per = 0.1,    #회전 단어 비율 지정
          scale = c(5,0.2), ###단어 크기 범위
          colors = pal)     #색상 목록