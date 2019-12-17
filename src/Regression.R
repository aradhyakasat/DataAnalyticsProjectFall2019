setwd("/Users/aradhyakasat/Documents/Acads/DataAnalytics/Final project/")
library(corrplot)
library(glmnet)
library(car)
library(ggplot2)
library(dplyr)

train=read.csv("finaldata.csv")

##Insert visualization code here
train_viz = train
train_viz$id = 1:nrow(train_viz)
for(i in 1:(nrow(train_viz)-1))
{
  train_viz$Close.Price[i]=train_viz$Close.Price[i+1]
}
ggplot(train_viz,aes(x=id,y=Close.Price))+geom_line()
ggplot(train_viz,aes(x=train_viz$News.Publication.Count))+geom_density()
ggplot(train_viz,aes(x=train_viz$News.Publication...Daily.Number.of.Stories))+geom_density()

ggplot(train_viz,aes(x=train_viz$Close.Price))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Publication.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Publication.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Positive.Sentiment))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Negative.Sentiment.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Neutral.Sentiment.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Maximum))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Minimum))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Average))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Heat...Daily.Average.Story.Flow))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Sentiment...Daily.Average))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Publication...Daily.Number.of.Stories))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Heat...Daily.Max.Readership))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Neutral.Sentiment.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Negative.Sentiment.Count))+geom_histogram()
ggplot(train_viz,aes(x=train_viz$News.Positive.Sentiment.Count))+geom_histogram()


ggplot(train_viz,aes(x=train_viz$Close.Price,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Publication.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Publication.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Positive.Sentiment,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Negative.Sentiment.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Neutral.Sentiment.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Maximum,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Minimum,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$Twitter.Sentiment.Daily.Average,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Heat...Daily.Average.Story.Flow,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Sentiment...Daily.Average,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Publication...Daily.Number.of.Stories,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Heat...Daily.Max.Readership,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Neutral.Sentiment.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Negative.Sentiment.Count,y=train_viz$Close.Price))+geom_point()
ggplot(train_viz,aes(x=train_viz$News.Positive.Sentiment.Count,y=train_viz$Close.Price))+geom_point()









#############
std=train[,3:24]
std$Close.Price=NULL
std <- scale(std)
std=cbind(std,train$Close.Price)
std=data.frame(std)

ggplot(std,aes(x=std$News.Positive.Sentiment.Count))+geom_histogram()

set.seed(100)
smp_size <- floor(0.8 * nrow(std))
train_ind <- sample(seq_len(nrow(std)), size = smp_size)
colnames(std)[22]='Close.Price'
train <- std[train_ind, ]
test <- std[-train_ind, ]


#train=subset(train,select = -c(High.Price,Low.Price,Volume))

#model=lm(train$Close.Price~.,train)
#summary(model)

std2=std
for(i in 1:(nrow(std)-1))
{
  std2$Close.Price[i]=std$Close.Price[i+1]
}

train <- std2[train_ind, ]
test <- std2[-train_ind, ]


train=subset(train,select = -c(Date,Open.Price,Close.Price))

#base regression
model=lm(train$Close.Price~.,train)
test_lm=test[,1:19]
base_model=predict(model,test)
test$base=base_model
summary(model)

#cor plot

corrmat=data.frame(cor(train))
write.csv(corrmat,"corrmat.csv")


corrplot(cor(train),tl.cex = 0.5)

# backward model
bmodel <- step(model, direction = "backward", trace=TRUE )
summary(bmodel)
test$reg_mod=predict(bmodel,test)
test$pred=NULL

x=model.matrix(Close.Price~.,data=train)[,-22]
y=train$Close.Price

enet=glmnet(x,y)

ggplot() +
  geom_line(data = test, aes(x = id, y = Close.Price), color = "red") +
  geom_line(data = test, aes(x = id, y = predicted), color = "blue") +
  xlab('data_date') +
  ylab('price')+scale_colour_manual("", values = c("Close.Price" = "red","predicted" = "blue"))



ggplot(data = test, aes(x = id)) +
  geom_line(aes(y = Close.Price, colour = "Close.Price")) +
  geom_line(aes(y = nnet, colour = "nnetPredicted.Price")) +
  geom_line(aes(y = knn, colour = "knnPredicted.Price"))
scale_colour_manual("",
                    breaks = c("Close.Price", "nnetPredicted.Price","knnPredicted.Price"),
                    values = c("red", "blue","black"))+
  xlab("Date")+ylab("Price")


x=as.matrix(train[,1:19])
y=as.matrix(train[,20])
fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
# summarize the fit
summary(fit)

#NNet Regression
library(mlbench)
require(nnet)
nnet.fit <- nnet( train$Close.Price/10000 ~.,train, size=2,linout=TRUE)
nnet.predict <- predict(nnet.fit,test)*10000
test$nnet=nnet.predict

#RandomForest Regression
library(randomForest)
rf <- randomForest(train$Close.Price ~ ., data=train[,1:19], ntree=20)
rf.predict=predict(rf, test[,1:21])
test$rf=rf.predict

#KNN Regression
library('FNN')
test_new=test
test_new$Low.Price=NULL
test_new$nnet=NULL
test_new$rf.predict=NULL
test_new$Volume=NULL
test_new[,21:23]=NULL

pred_001 = FNN::knn.reg(train = train, test = test_new, y=train$Close.Price, k = 50)
test$knn=pred_001$pred
mean((pred_001$pred - test$Close.Price)^2)