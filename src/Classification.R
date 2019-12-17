setwd("/Users/aradhyakasat/Documents/Acads/DataAnalytics/Final project/")
library(corrplot)
library(glmnet)
library(car)

train_class=read.csv("finaldata.csv")
for(i in 2:nrow(train_class))
{
  if(train_class$Close.Price[i]>train_class$Close.Price[i-1])
  {
    train_class$trend[i]=1
  }
  else
  {
    train_class$trend[i]=0
  }
}
train_class$trend[1]=0
std=train_class[,3:24]
#std$Close.Price=NULL
std <- scale(std)

std=cbind(std,train_class$trend)
std=data.frame(std)

set.seed(100)
smp_size <- floor(0.8 * nrow(std))
train_ind <- sample(seq_len(nrow(std)), size = smp_size)
colnames(std)[23]='trend'
train_class <- std[train_ind, ]
test_class <- std[-train_ind, ]


#train=subset(train,select = -c(High.Price,Low.Price,Volume))

#model=lm(train$Close.Price~.,train)
#summary(model)

std2=std
for(i in 1:(nrow(std)-1))
{
  std2$Close.Price[i]=std$Close.Price[i+1]
}

train_class <- std2[train_ind, ]
test_class <- std2[-train_ind, ]


train_class=subset(train_class,select = -c(Low.Price,Volume))
test_class=subset(test_class,select = -c(Low.Price,Volume))

x=as.matrix(train_class[,1:20])
y=as.matrix(train_class[,21])
z=as.matrix(test_class[,1:20])

# AUC
library(ROCR)
perf=performance(pred1,"tpr","fpr")
plot(perf)


# Base Model
prr=glm(y ~ . ,family=binomial(link='logit'),data=as.data.frame(x))
yy=predict(prr,test_class[,1:20],type="response")
test_class$prob=yy
test_class$logistic=ifelse(test_class$prob>0.5,1,0)
table(test_class$trend,test_class$logistic)

#Modified Logistic
prr=cv.glmnet(x,y,family="binomial",type.measure="auc")
yy=predict(prr,z, s="lambda.min",type="response")
test_class$prob=yy
test_class$logistic_modi=ifelse(test_class$prob>0.5,1,0)
table(test_class$trend,test_class$logistic_modi)

#nnet classification
library(mlbench)
require(nnet)
prr=nnet( y ~.,data=as.data.frame(x), size=2,linout=TRUE)
yy=predict(prr,test_class[,1:20],type="raw")
test_class$prob=yy
test_class$nnet=ifelse(test_class$prob>0.5,1,0)
table(test_class$trend,test_class$nnet)

#random forest classification
library(randomForest)
prr=randomForest( y ~.,data=as.data.frame(x), ntree=20)
yy=predict(prr,test_class[,1:20])
test_class$prob=yy
test_class$rf=ifelse(test_class$prob>0.5,1,0)
table(test_class$trend,test_class$rf)
varImpPlot(prr, 
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

#knn classification
library(class)
library(dplyr)
library(lubridate)
prr=knn( x,z,y, k=15)
test_class$knn=as.integer(prr)-1
table(test_class$trend,test_class$knn)