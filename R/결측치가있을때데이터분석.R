#결측치가 있을 때 데이터 분석
##데이터 선택
mtcars
#각 데이터셋의 10개 데이터 보기 
mtcars[seq(1,10,1)]

mc_part<-mtcars
#컬럼명
names(mc_part)

mc_part[c(4,8),'drat']<-NA   #drat셋의 4번째,8번째에 NA를 넣기
mc_part

library(dplyr)
mc_part%>%summarise(mean_drat=mean(drat,na.rm=T),
                    sum_drat=sum(drat,na.rm=T),
                    med_drat=median(drat,na.rm=T))

mc_part[c(4,8),'qsec']<-NA  #qsec셋의 4번째,8번째에 NA를 넣기
mc_part
mc_part%>%summarise(mean_qsec=mean(qsec,na.rm=T),
                    sum_qsec=sum(qsec,na.rm=T),
                    med_qsec=median(qsec,na.rm=T),
                    var_qsec=var(qsec,na.rm=T),
                    sd_qsec=sd(qsec,na.rm = T))
