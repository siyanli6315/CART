library(rpart)
library(ggplot2)
library(showtext)
showtext.auto(enable = T)

biao1=read.csv("表1.csv",header = T)
biao2=read.csv("表2.csv",header = T)
biao3=read.csv("表3.csv",header = T)
data("iris")

#第一题

fun=function(x,y){
  y/10*((x/y)*log2(x/y)+((y-x)/y)*log2((y-x)/y))
}

fun2=function(x1,y1,x2,y2){
  m1=fun(x1,y1)
  m2=fun(x2,y2)
  m1+m2
}

(x0=fun(3,10))
(x11=fun2(1,3,6,7))
(x12=fun2(3,4,2,6))
(x13=fun(3,7))
(x21=fun(3,4))
(x22=fun(3,6))
(x23=fun(3,8))
(x3=fun2(2,5,4,5))
x0-x11
x0-x12
x0-x13
x0-x21
x0-x22
x0-x23
x0-x3

#第二题：用表1数据建立分类树，然后用表3数据看预测效果

mod=rpart(PLAY~.,data=biao1,method="class",
          control=rpart.control(minbucket=1,cp=0.01))
biao3
play_hat=predict(mod,newdata=biao3)
play_hat

#第三题：iris数据。用R中的rpart包，用二折交叉验证估计CART的误差。

mycv=function(formula=Species~.,data=iris,minbucket=c(1:20),k=2){
  n=nrow(data)
  m=n%/%k
  data=data[sample(1:n,n),]
  result=matrix(NA,nrow=length(minbucket),ncol=k)
  for(i in 1:length(minbucket)){
    for(j in 1:k){
      index=c(((j-1)*m+1):(j*m))
      valid=data[index,]
      train=data[-index,]
      mod=rpart(formula,data=train,method="class",
                control=rpart.control(minbucket=minbucket[i]))
      y_hat=predict(mod,newdata = valid)
      y_hat=apply(y_hat,1,function(x)names(which.max(x)))
      acc=sum(y_hat==valid$Species)/m
      result[i,j]=acc
    }
  }
  result=rowMeans(result)
  return(list(minbucket=minbucket,acc=result))
}

set.seed(100)
cv=mycv()
png("交叉验证.png",width = 700,height = 500)
ggplot(data=data.frame(minbucket=cv$minbucket,Accuracy=cv$acc),
       aes(x=minbucket,y=Accuracy))+
  geom_point(size=5,shape=15)+
  geom_line(size=1)+
  theme_bw()+
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15))
dev.off()

set.seed(100)
data=iris[sample(1:nrow(iris),nrow(iris)),]
train=sample(1:nrow(iris),nrow(iris)%/%4*3)
mod=rpart(Species~.,data=data,method="class",subset = train,
          control=rpart.control(minbucket=1))
y_hat=predict(mod,newdata = data[-train,])
y_hat=apply(y_hat,1,function(x)names(which.max(x)))
sum(y_hat==data$Species[-train])/length(data$Species[-train])
table(y_hat,data$Species[-train])
