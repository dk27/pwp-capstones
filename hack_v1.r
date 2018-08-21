library(RODBC)
library(statmod)
library(data.table)
library(tweedie)
library(caret)
library(cplm)
library(glmnet)
library(HDtweedie)
library(boot)
library(h2o)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(dplyr)
library(lubridate)

setwd("C:/Users/daria/Desktop/Hackathon/speed_hackers_data_set")

train<-read.csv(file="training_data.csv", header=T, sep=",")
test<-read.csv(file="test_data.csv", header=T, sep=",")

table(is.na(train$Age))
range(train$Age)
summary(train$Age)

train$Start<-strptime(train$CallStart, format="%H:%M:%S")
train$End<-strptime(train$CallEnd, format="%H:%M:%S")

train$duration_call<-as.numeric(difftime(train$End, train$Start, 
                             units =  "secs"))
train$Days_passed<-ifelse(train$DaysPassed==-1, "Z",
                   ifelse(train$DaysPassed<=90, "0-90",
                   ifelse(train$DaysPassed<=120, "90-120",
                   ifelse(train$DaysPassed<=270, "120-270",
                   ifelse(train$DaysPassed<=360, "270-360", "360+")))))
train$target<-as.factor(ifelse(train$CarInsurance==1, "1", "0"))


test$Start<-strptime(test$CallStart, format="%H:%M:%S")
test$End<-strptime(test$CallEnd, format="%H:%M:%S")
test$duration_call<-as.numeric(difftime(test$End, test$Start, units =  "secs"))
test$Days_passed<-ifelse(test$DaysPassed==-1, "Z",
                  ifelse(test$DaysPassed<=90, "0-90",
                  ifelse(test$DaysPassed<=120, "90-120",
                  ifelse(test$DaysPassed<=270, "120-270",
                  ifelse(test$DaysPassed<=360, "270-360", "360+")))))
train$target<-as.factor(ifelse(train$CarInsurance==1, "1", "0"))
                                                        

table(train$target)

h2o.init(nthreads=-1)
df<-as.h2o(train)
test<-as.h2o(test)
y<-"target"
x<-c("Age", "Job", "Marital", "Education", "Default", "Balance", "HHInsurance",
     "CarLoan", "Communication", "LastContactDay", "LastContactMonth", "NoOfContacts",
     "PrevAttempts", "Outcome", "duration_call")
df[[y]] <- as.factor(df[[y]])  
df.split <- h2o.splitFrame(data=df, ratios=0.7)

# Create a training set from the 1st dataset in the split
df.train <- df.split[[1]]

# Create a testing set from the 2nd dataset in the split
df.test <- df.split[[2]]
gmb<-h2o.gbm(y=y,
             x=x,
             training_frame = df.train,
             validation_frame = df.test,
             ntrees=500)

predict<-h2o.predict(object=gmb, test)
test.df<-h2o.cbind(test, predict)
a<-as.data.frame(predict)
write.csv(data.frame(a), file="output.csv")
?h2o.randomForest
rf<-h2o.randomForest(y=y,
                     x=x,
                     training_frame = df.train,
                     validation_frame = df.test,
                     nfolds=5,
                     stopping_metric="logloss",
                     stopping_tolerance=0.001,
                     ntrees=500)
