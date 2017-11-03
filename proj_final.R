library(MASS)
library(e1071)
library(nnet)
library(rattle)
library(rpart)
library(xlsx)
library(slidify)
library(caret)

author("demo")
##setwd("C:/Users/Narasimman/Documents/demo")
medical<-read.csv(file.choose())
str(medical)
summary(medical)
attach(medical)


##No.show <- factor(make.names(No.show))
##To check whether we have a column with N/A
#3
##numIndex<- sapply(medical, is.numeric)
##summary(medical[,numIndex])
##medical$No.show<-as.factor(medical$No.show)
##medical$Gender<-as.factor(medical$Gender)
##medical$Scholarship<-as.factor(medical$Scholarship)

Train <- createDataPartition(medical$SMS_received, p=0.7, list=FALSE)
training <- medical[ Train, ]
testing <- medical[ -Train, ]

                               
model1<- train(No.show ~ Gender+Age+SMS_received, data = training,method = "glm",family="binomial") 
 
model1               

#Age+SMS_received+Gender
##No.show vs age-0.773
##No.show vs age+scholar+sms_recieved-0.75049
##No.show vs age+scholar+sms_recieved+gender - 0.74
##No.show vs age+sms+gender -0.742,this is the best amongst all
##No

model1


varImp(model1)##sms_received is having the highest score

pred = predict(model1, newdata=testing)
confusionMatrix(data=pred, testing$No.show)

#######
model2<-rpart(formula = No.show ~ Age +  Alcoholism + 
        Scholarship +  SMS_received, data = training, method = "class", 
      maxdepth = 4, minsplit = 2, minbucket = 1, cp = -1)
model2


rpart.plot::rpart.plot(model2, type = 2, fallen.leaves = FALSE, extra = 4)
rattle::fancyRpartPlot(model2, sub="", main = "Recursive Partioning of Attendance")


testPred <- predict(model2, newdata = testing)
predictability <- sum(testPred == testing$No.show)/ length(testing$No.show)*100
predictability


##this gives o accuracy random tree is insignificant



rdfit<-train(No.show ~ SMS_received+Age+Handcap+Alcoholism,data = training,method = "lda",family = "binomial")
rdfit
rdaClasses <- predict(rdfit, newdata = testing)
 confusionMatrix(rdaClasses, testing$No.show)
 
 
knn_model<-train(No.show ~ SMS_received+Age+Handcap+Alcoholism,data = training,method = "knn")
 knn_model
 knnpred <- predict(knn_model, newdata = testing)
 confusionMatrix(knnpred, testing$No.show)
 

 