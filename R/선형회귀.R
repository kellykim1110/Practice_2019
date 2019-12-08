#비정형을 정형으로 변경
#텍스트를 자연어 처리로 정형으로 변경

#단순선형회귀 : 종속변수가 한개  
#다중선형회귀 : 종속변수가 여러개

#머신러닝은 학습(입력)데이터로 학습후 새로운(출력) 데이터로 예측
#딥러닝은 머신러닝 중 하나
#딥러닝은 이 때 주로 이미지나 텍스트를 데이터로 사용

k<-c(50,70,80,90,100)
e<-c(77,88,99,100,70)
m<-c(55,75,85,95,105)


all<-data.frame(k,e,m)
all

#lm(패턴,데이터)
#lm(출력(답)~입력(문제),data=이름)
model1<-lm(m~k,data=all)  #국어점수x로 수학점수y를 구하기
model1 #intercept는 y절편, k는 가중치

model1<-lm(m~k+e,data=all)  
model1
summary(model1)  #*의 개수가 더 유의함을 나타내줌 ##국어점수가 더 유의함 


###회귀 - 수치형변수###########################################################


dat<-data.frame(k=c(80,85))
pre<-predict(model1,newdata=dat)
pre   #수치형을 예측

k<-c(50,70,80,90,100)
e<-c(70,80,90,100,70)
h<-c(170,220,250,280,270)

all<-data.frame(k,e,h)
all
model2<-lm(h~k+e,data=all)
model2

dat<-data.frame(k=c(80,85),e=c(50,60))
pred<-predict(model2,newdata = dat)
pred
