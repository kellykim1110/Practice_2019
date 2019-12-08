#시각화
library(ggplot2)

mpg
names(mpg)
ggplot(data=mpg)
#배경, 축 설정
ggplot(data=mpg,aes(x=cyl,y=hwy))
#그래프 그리기
ggplot(data=mpg,aes(x=cyl,y=hwy))+
  geom_col() #값표시
ggplot(data=mpg,aes(x=cyl))+
  geom_bar()  #빈도수 표시


ggplot(data=mpg,aes(x=cyl,y=hwy))+
  geom_col()+ylim(0,3000)+xlim(4,8)


midwest
names(midwest)

ggplot(data=midwest,aes(x=poptotal,y=popwhite))+geom_point()+xlim(0,500000)+
  ylim(0,80000)+ggtitle("전체인구와 백인인구 사이의 관계")

ggplot(data=midwest,aes(x=poptotal,y=popasian))+geom_point()+xlim(0,100000)+
  ylim(0,80000)+ggtitle("전체인구와 아시아인구 사이의 관계")

  
#데이터 불러오기
titanic<-read.csv("C:\\Users\\202-006\\Desktop\\RBasic-master\\R_Data\\tr_mod.csv")
head(titanic,10)
names(titanic)
#시각화
ggplot(titanic,aes(x=SibSp,y=Survived,colour=Sex))+geom_col()
ggplot(titanic,aes(x=Age,y=Survived,colour=Sex))+geom_col()
ggplot(titanic,aes(x=SibSp,y=Sex,colour=Age))+geom_col()

ggplot(titanic,aes(x=SibSp,y=Sex,fill=Embarked))+geom_col() #색 설정
ggplot(titanic,aes(x=SibSp,y=Sex,col=Embarked))+geom_col() #테두리 색설정



##파생변수
#mutate(변수명=식...) #mutate를 사용시 library(dplyr)을 먼저 해줘
titanic%>%mutate(family=SibSp+Parch)%>%head
new_dat<-titanic%>%mutate(family=SibSp+Parch)
new_dat

new_dat<-titanic%>%mutate(EM_C=ifelse(Embarked=="C",1,0),
                          EM_S=ifelse(Embarked=="S",1,0),EM_Q=ifelse(Embarked=="Q",1,0))
new_dat%>%select(EM_C,EM_S,EM_Q)
new_dat<-titanic%>%mutate(Sex_M=ifelse(Sex=="male",1,0),
                          Sex_W=ifelse(Sex=="female",1,0))
new_dat%>%select(Sex,Sex_M,Sex_W)
p1=ggplot(new_dat,aes(x=Sex))

new_dat<-titanic%>%mutate(Age_class=ifelse(Age<10,0,
                                           ifelse(Age<20,1,
                                           ifelse(Age<30,2,
                          ifelse(Age<40,3,
                         ifelse(Age<50,4,
                          ifelse(Age<60,5,6)))))))
p2=ggplot(new_dat,aes(x=Age_class,y=Survived,fill=Sex))+geom_col()
table(new_dat$Age_class)                          
table(new_dat$Age_class,new_dat$Survived)



p1 = ggplot(data = titanic, aes(x = Age, y = Survived, fill = Sex)) + geom_col() ## 14 : ggplot을 이용한 나이(Age)별 생존수(Survived)그래프 
p2 = ggplot(data = titanic, aes(x = Sex, y = Survived, fill = Sex)) + geom_col() ## 15 : ggplot을 이용한 성별(Sex)별 생존수(Survived) 그래프 
p3 = ggplot(data = titanic, aes(x = Pclass, y = Survived, fill = Sex)) + geom_col() ## 16 : ggplot을 이용한 Pclass 별 생존수(Survived) 그래프
p4 = ggplot(data = titanic, aes(x = Embarked, y = Survived, fill = Sex)) + geom_col() ## 17 : ggplot을 이용한 Embarked 별 생존수(Survived) 그래프 

p1
p2
p3
p4

#묶어서 보기
library(grid)
grid.add(p1, p2, p3, p4,ncol = 2, nrow = 2, top = "Titanic _ Survived") 

