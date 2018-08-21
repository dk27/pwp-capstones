library(RODBC)
library(statmod)
library(data.table)
library(caret)
library(boot)
library(MASS)
library(censReg)
library(AER)
library(sandwich)
library(lmtest)
library(nnet)
library(betareg)
library(plyr)
library(glmnet)
library(rattle)
library(h2o)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(qdap)
library(dplyr)
library(HDtweedie)
library(neuralnet)
setwd("C:/Users/daria/Desktop/H2O Open Tour")

data<-read.csv(file="Train.csv", header=T, sep=',')
test<-read.csv(file="Test.csv", header=T, sep=",")
plot(data$TARGETVAR)
hist(data$TARGETVAR)
range(data$TARGETVAR)
table(data$ZONEID)


data$ZONEID<-as.factor(data$ZONEID)
data$month<-substr(data$TIMESTAMP, 5, 6)
data$season<-as.factor(ifelse(data$month %in% c('12', '01', '02'), "winter",
                              ifelse(data$month %in% c('03', '04', '05'), "spring",
                                     ifelse(data$month %in% c("06", "07", "08"), "summer", "fall"))))
table(is.na(data$season))
data$time<-substr(data$TIMESTAMP, 10, 14)
data$time1<-as.numeric(gsub("[[:punct:]]", "", substr(data$time, 1,2)))
data$timeofday<-as.factor(ifelse(data$time1 %in% c(5:10), "5am-10am",
                                 ifelse(data$time1 %in% c(11:16), "11am-4pm",
                                        ifelse(data$time1 %in% c(17:20), "5pm-8pm",
                                               ifelse(data$time1 %in% c(21:23), "9pm-11pm", "12am-4am")))))
table(data$timeofday, as.factor(data$time1))
test$ZONEID<-as.factor(test$ZONEID)
test$month<-substr(test$TIMESTAMP, 5, 6)
test$season<-as.factor(ifelse(test$month %in% c('12', '01', '02'), "winter",
                              ifelse(test$month %in% c('03', '04', '05'), "spring",
                                     ifelse(test$month %in% c("06", "07", "08"), "summer", "fall"))))
test$time<-substr(test$TIMESTAMP, 10, 14)
plot( test$season, test$TARGETVAR)
test$time1<-as.numeric(gsub("[[:punct:]]", "", substr(test$time, 1,2)))
test$timeofday<-as.factor(ifelse(test$time1 %in% c(5:10), "5am-10am",
                                 ifelse(test$time1 %in% c(11:16), "11am-4pm",
                                        ifelse(test$time1 %in% c(17:20), "5pm-8pm",
                                               ifelse(test$time1 %in% c(21:23), "9pm-11pm", "12am-4am")))))
table(data$timeofday, as.factor(data$time1))


library(h2o)

## Connect to H2O cluster
h2o.init(nthreads = -1)

df<-as.h2o(data)

y<-"TARGETVAR"
x<-c("ZONEID", "U10", "V10", "U100", "V100", "season", "timeofday", "interaction")
dftest<-as.h2o(test)
split = h2o.splitFrame(data = df[, c(y, x)], destination_frames = c("train_hex", "valid_hex"))
train = split[[1]]
valid = split[[2]]

gbm1 = h2o.gbm(x = x,
               y = y,
               training_frame = train, 
               validation_frame = valid, 
               distribution = "AUTO",
               ntrees=100)


prediction=h2o.predict(gbm1, newdata=dftest)
pred=as.data.frame(prediction)
test<-cbind(test, pred)


hist(test$predict)
range(data$TARGETVAR)
range(test$predict)

data$TARGETVAR1<-ifelse(data$TARGETVAR==0, 0.001,
                        ifelse(data$TARGETVAR==1, 0.999,
                               data$TARGETVAR))
gy <- betareg(TARGETVAR1 ~ U10+V10+U100+V100+ZONEID+timeofday+season | U10+V10+U100+V100+ZONEID+timeofday+season, 
              data = data)
print(summary(gy))
test$TARGETVAR <- predict(gy,newdata=test)
