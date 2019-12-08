#데이터 불러오기
library(MASS)
head(UScrime)

###Q1. 남부와 그외지역이 같은 투옥 확률 존재하는가?
#Prob 범죄확률
#tbl_So : 지역 남부 인지 아닌지
tbl_So<-table(UScrime$So)
tbl_So
#시각화
barplot(tbl_So)
library(ggplot2)
ggplot(UScrime,aes(x=So))+geom_bar()


#가설검정 :귀무 대립 가설
#t.test: 독립성 정규성 등분산성


###정규성
#30이상 정규성만족
#10이상 30미만 Kolmogorov-Smirov test, Shapiro-Wilk test
#10미만 정규성 불만족

###등분산성 
#var.test(v1,v2)
#p-value가 0.05보다 작다 = 유의하다.

var.test(UScrime$So,UScrime$Prob)  #남부와 그외지역이 같은 투옥 확률 존재 무

t.test(Prob~So,data=UScrime)

###Q2.젊은 남성과 장년 남성의 실업률은 서로 다른가?
#U1: 14~24세의 남성 실업률
#U2: 35~39세의 남성 실업률
var.test(UScrime$U1,UScrime$U2)
t.test(UScrime$U1,UScrime$U2, paired=T)  #장년층과 젊은 남성의 실업률은 다르다



#######################################################################################################

#단순선형회귀 : 종속변수가 한개  
#다중선형회귀 : 종속변수가 여러개



##단순선형회귀
k<-c(50,70,80,90,100)
e<-c(77,88,99,100,70)
m<-c(55,75,85,95,105)


all<-data.frame(k,e,m)
all

#lm(패턴,데이터)
#lm(출력(답)~입력(문제),data=이름)
model1<-lm(m~k,data=all)  #국어점수x로 수학점수y를 구하기
model1 #intercept는 y절편, k는 가중치
summary(model1)

#다중선형회귀 
model2<-lm(m~k+e,data=all)  
model2
summary(model2)
##잔차 = 예측값



k<-c(50,70,80,90,100)
e<-c(70,80,90,100,70)
h<-c(170,220,250,280,270)

all<-data.frame(k,e,h)
all

model3<-lm(h~k+e,data=all)
model3
summary(model3)

# 두개의 변수로 이용한 예측
dat <- data.frame(k=c(80,85), e=c(50,60))
pred1 <- predict(model3, newdata=dat)
pred1

############################################################
#kaggle의 자전거 수요예측 데이터
bike<-read.csv("C:\\Users\\RBasic-master\\R_Data\\train.csv")
names(bike)

bike%>%head()
model4<-lm(count~season+weather+workingday+holiday,data=bike)  #holiday workingday 는 유의하지 않아
summary(model4)

model5<-lm(count~season+weather+temp,data=bike)
summary(model5)

model6<-lm(count~season+weather+atemp+temp+humidity,data=bike)
summary(model6)


model7<-lm(count~humidity+windspeed,data=bike)
summary(model7)  #windspeed 유의하지 않아

#datetime 은 문자열
##############################################################
#데이터 불러오기
mtcars

plot(mtcars$disp, mtcars$mpg, 
     xlab="자동차 배기량", 
     ylab="자동차의 연비", 
     pch = 15, main="자동차배기량과 연비의 관계")

model <- lm(mpg~disp, data=mtcars)
summary(model)
abline(model)  #예측값 그려보기




names(mtcars)
model1 <- lm(mpg~disp+cyl+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
summary(model1)

model2<-lm(mpg~wt,data=mtcars)
summary(model2)

plot(mtcars$wt, mtcars$mpg, 
     xlab="자동차 배기량", 
     ylab="자동차의 연비", 
     pch = 15, main="자동차배기량과 연비의 관계")
abline(model2)

#-----------------------------------------------------------------

model1 <- lm(mpg~disp+cyl+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
summary(model1) #wt,qsec,am의 Pr(>|t|)(p_value)값이 유독작아

#R-squared값은 클수록 좋아

model3<-lm(mpg~wt+qsec+am,data=mtcars)
summary(model3)

#########################################################################

library(MASS)
data("Boston")
head(Boston)

set.seed(0)

#7:3일 때 mae_value는 3.262974
i<-sample(1:nrow(Boston),
          size=nrow(Boston)*0.7,
          replace=F)
i 

Boston_tr<-Boston[i,]
Boston_test<-Boston[-i,]

#8:2일 때 mae_value는  2.753242
j<-sample(1:nrow(Boston),
          size=nrow(Boston)*0.8,
          replace=F)

Boston_tr<-Boston[j,]
Boston_test<-Boston[-j,]


#9:1일 때 mae_value는 2.368251 mse_value는 10.2528
j<-sample(1:nrow(Boston),
          size=nrow(Boston)*0.9,
          replace=F)

Boston_tr<-Boston[j,]
Boston_test<-Boston[-j,]



#5:5일 때 mae_value는 3.431026
j<-sample(1:nrow(Boston),
          size=nrow(Boston)*0.5,
          replace=F)

Boston_tr<-Boston[j,]
Boston_test<-Boston[-j,]




#6:4일 때 mae_value는 3.243208
j<-sample(1:nrow(Boston),
          size=nrow(Boston)*0.6,
          replace=F)

Boston_tr<-Boston[j,]
Boston_test<-Boston[-j,]



#모델생성
lm.model<-lm(medv~.,data=Boston)
summary(lm.model)


#모델예측
pred_value<-predict(lm.model,Boston_test)
pred_value

##MAE = |Y-e(Y)|/n
dim(Boston_test) ;
n1=length(pred_value)
diff_val=abs(Boston_test$medv-pred_value)
mae_value=sum(diff_val)/n1
mae_value

##MSE = (Y-e(Y))^2
dim(Boston_test) ;
n1=length(pred_value)
diff_val=abs(Boston_test$medv-pred_value)
mse_value=sum((diff_val)^2)/n1
mse_value

############################################################################

install.packages("faraway")
install.packages("pscl")


library(faraway)
library(pscl)
library(caret) #정확도 확인
library(ROCR)
#왜 정확도 대신 ROC커브를 이용할까?
### 데이터의 불균형
search()

data(pima)
names(pima)



library(MASS)

set.seed(0)
#5:5일 때 mae_value는 0.3270682, 
j<-sample(NROW(pima)*0.5)

pima_tr<-pima[j,]
pima_test<-pima[-j,]

#모델생성1
lm.model<-lm(test~glucose+pregnant+bmi,data=pima)
summary(lm.model)


#모델예측1
pred_value<-predict(lm.model,pima_test)
pred_value

##MAE = |Y-e(Y)|/n
dim(pima_test) ;
n1=length(pred_value)
diff_val=abs(pima_test$test-pred_value)
mae_value=sum(diff_val)/n1
mae_value

##MSE = (Y-e(Y))^2
dim(pima_test) ;
n1=length(pred_value)
diff_val=abs(pima_test$medv-pred_value)
mse_value=sum((diff_val)^2)/n1
mse_value


#모델생성2
glm.model<-glm(test~glucose+pregnant+bmi,family=binomial,data=pima)
summary(glm.model)

#모델예측2
pred_value<-predict(glm.model,newdata=pima_test,type="response")
pred_value

pred_value<-as.integer(pred_value>0.5)
pred_value

actual<-pima_test[,"test"]

#분할표 이용한 정확도 확인 
xt=xtabs(~pred_value+actual)
xt
prop.table(xt) #정확도 0.6093750+0.1901042=0.794792


#caret팩캐지 이용한 정확도 확인 Accuracy : 0.7995 
actual<-pima_test[,"test"]
actual<-as.factor(actual)
pred_value<-as.factor(pred_value)
confusionMatrix(pred_value,actual)
