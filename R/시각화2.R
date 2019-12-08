#연속형 변수는boxplot,histogram
#barplot()은  table()로 만들어서 정제   
#ggplot()과의차이점

library(ggplot2)
search()

data(mtcars)
qplot(mtcars$wt,mtcars$mpg)

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

colnames(pressure)
ggplot(pressure,aes(x=temperature,y=pressure))+geom_point()+geom_line()


ggplot(mtcars,aes(x=cyl))+geom_bar(col=c("red","green","blue"))  #외각선 색 넣기
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_col()

install.packages("gcookbook")
library(gcookbook)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",
                                               fill="lightblue",
                                               colour="red",
                                               position="dodge")
