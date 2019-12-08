vec1<-c(1,2,3,4)
vec1
seq(1,10,2)


is.vector(vec1)  #vec1은 벡터인가 아닌가
class(vec1) #vec1은 벡터인가 아닌가

sd(vec1) #표준편차
var(vec1) #분산
median(vec1) #평균
IQR #제 3분위수-제1분위수
quantile(mtcars$mpg) #summary와 비슷#분위수로 나누어줌



install.packages("ggplot2")
library(ggplot2)
search()

x<-c('a','b','c','c','b')
qplot(x)



#데이터 탐색
mpg
str(mpg) #속성출력#구조확인
dim(mpg)  #행렬 확인

names(mpg)
colnames(mpg)

head(mpg) #앞에서 6행
tail(mpg) #뒤에서 6행 

summary(mpg) #요약값


qplot(data=mpg, x=hwy)


##데이터 읽어오기
#read.csv()
#read_excel()
#read.table("file.txt",sep=',')
#double=좀더 넓은 범위를 나타내고 싶을때


##데이터 만들기
#write.csv()

#RData파일  (.rda)
#save()
#load()



#행추출  filter()
#열추출  select()
#정렬  arrange()
#변수추가  mutate()
#집단별나누기 group_by()
#통계치 산출  summarise()
#열 데이터 합치기  merge(), left_join()




kor<-c(80,80,90)
eng<-c(100,100,100)
math<-c(95,100,70)




#가장 많이 사용하는 자료형
#데이터 프레임->행과 열이 같아야 한다
df<-data.frame(kor,eng,math)
df


install.packages("dplyr")
library(dplyr)
search()

mtcars
?mtcars #설명
dt_new<-mtcars
dim(dt_new)
head(dt_new)
tail(dt_new)


dt<-data.frame(var1=c(1,3,5),var2=c(2,4,6))
dt
#새로운 컬럼 만들기1
dt$total<-dt$var1+dt$var2
dt

dt_new
dt_new$total<-dt_new$disp+dt_new$hp+dt_new$drat



#새로운 컬럼 만들기2
#ifelse(조건문,참,거짓)
#ifelse(조건문,참,
#                 ifelse(조건문,참,거짓))

library(ggplot2)
dat<-mpg
dat$total<-(dat$cty+dat$hwy)/2
summary(dat$total)

dat$passfail<-ifelse(dat$total>=20,"pass","fail")
head(dat$passfail,20)

qplot(dat$passfail)
dat$fuel_class<-ifelse(dat$total>=23.5,"A",
                       ifelse(dat$total>=15.5,"B","C"))
head(dat$fuel_class,20)
qplot(dat$fuel_class)
table(dat$fuel_class) #빈도수 


 #data[-5] data의 5번째 값 삭제



new_val<-c(1,2,3,5,6)
length(new_val)
new_val[3]<-30
new_val

new_val[TRUE]
new_val[new_val%%2==0]
new_val[new_val%%2==0]=200
new_val


val<-c(seq(2,100,2))
val
val[val%%5==0]=1000
val


#ls(변수 혹은 객체명)  #상세히 설명
#rm(#변수 또는 객체명)

rm(val)
str() #내가 만든 객체 자세히

#installed.packages()  #내가 설치한 패캐지

#로컬에서 설치
#install.packages("devtools")
#library(devtools)
#install("c:/명칭")

#read.spss : spss 파일 읽기


library(dplyr)
#데이터 불러오기
titanic<-read.csv("C:\\Users\\202-006\\Desktop\\RBasic-master\\R_Data\\tr_mod.csv")
head(titanic,10)
names(titanic)

##등급별 인원 구하기
table(titanic$Pclass)
#filter(행추출)
#filter(조건문)
p1<-titanic %>% filter(Pclass==1)#titanic안에서 Pclass가 1인 것만 p1으로 명칭해주기 #filter가 행을 가져오는 것
p1
head(p1)


P1<-titanic%>%filter(Embarked=="C"&Survived==1)
dim(P1)
P2<-titanic%>%filter(Embarked=="Q"&Survived==1)
dim(P2)
P3<-titanic%>%filter(Embarked=="S"&Survived==1)
dim(P3)

##등급별 살아남은 인원 구하기
Q<-titanic%>%filter(Survived==1)
P<-Q%>%select(Embarked)
P
qplot(P$Embarked)

Q1<-titanic%>%filter(Survived==0)
P1<-Q1%>%select(Embarked)
P1
qplot(P1$Embarked)


dat<-titanic%>%select(Pclass,Name,Sex) #titanic에서 Pclass,Name,Sex인 열만 뽑기
head(dat)


#Survived만 제외한 컬럼을 만들어보자.
Dat<-titanic%>%select(-Survived)
Dat
Dat<-titanic%>%select(-Survived,-Embarked)
Dat

##2,3등급 사람 뽑기
d<-titanic%>%filter(Pclass%in%c(2,3))
d

#arrange(기준 컬럼)
A<-titanic%>%arrange(Age)%>%head(10) #나이가 내림차순
A


install.packages("dplyr")
library(dplyr)
titanic<-read.csv("C:\\Users\\202-006\\Desktop\\RBasic-master\\R_Data\\tr_mod.csv")

Dat<-titanic%>%select(-Name,-Parch,-Ticket,-Fare,-Cabin,-PassengerId)
names(Dat)
table(Dat)

w<-Dat%>%filter(Sex=='female')
dim(w)
m<-Dat%>%filter(Sex=='male')
dim(m)
W<-w%>%select(Survived)
M<-m%>%select(Survived)
table(W)
table(M)

w<-w%>%filter(Sex=='female',Survived==1)
dim(w)
m<-m%>%filter(Sex=='male',Survived==1)
dim(m)

W<-w%>%select(Pclass)
M<-m%>%select(Pclass)
table(W)
table(M)

W1<-w%>%filter(Pclass==1)
M3<-m%>%filter(Pclass==3)
W<-W1%>%select(SibSp)
M<-M3%>%select(SibSp)
table(W)
table(M)


W1<-M3%>%filter(SibSp==0)
W<-W1%>%select(Embarked)
table(W)
W1<-W1%>%filter(Embarked=="S")
W<-ifelse(W1$Age<20,"1",
          ifelse(W1$Age<30,"2",
                 ifelse(W1$Age<40,"3","4")))
table(W)
