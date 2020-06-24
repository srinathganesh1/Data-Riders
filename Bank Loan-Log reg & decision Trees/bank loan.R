#bank loan 
#we are concerned in defaulter to predict crctly
##table(train$default)
#


d <- read.csv('D:/Data Science/Dr Vinod online classes/Class 8 -Logistic Regression/Bankloan1.csv')
head(d)
dim(d)

set.seed(123)
ind <- sample(2,nrow(d),replace=T,prob=c(0.7,0.3))
train <- d[ind==1,]
test <- d[ind==2,]

dim(train)
dim(test)

#t.test(d$age,d$default,var.equal = T)

library(dplyr)
library(ggplot2)

#EDA----

summary(train)
describe(train)
str(train)

train$ed <- as.factor(train$ed)
train$default <- as.factor(train$default)
test$ed <- as.factor(test$ed)
test$default <- as.factor(test$default)

default <- test[,9]
test <- test[,-9]


#Modelling----
names(train)
model1 <- glm(default~., data=train,family='binomial')
summary(model1)

pred1 <- predict(model1,type='response')
f1 <- table(train$default,pred1>=0.3)
acc1=sum(diag(f1))/sum(f1)
acc1


model2 <- glm(default~ed+employ+creddebt+address, data=train,family='binomial')
summary(model2)

pred2 <- predict(model2,type='response')
f2 <- table(train$default,pred2>=0.5)
acc2=sum(diag(f2))/sum(f2)
acc2

model3 <- glm(default~address+creddebt+employ, data=train,family='binomial')
 summary(model3)
 
pred3 <- predict(model3,type='response')
f3 <- table(train$default,pred3>=0.5)
acc3=sum(diag(f3))/sum(f3)
acc3


model4 <- glm(default~employ+creddebt+address+debtinc,train,family='binomial')

summary(model4)

pred4 <- predict(model4,train,type='response')
f4 <- table(train$default,pred4>=0.3)
acc4=sum(diag(f4))/sum(f4)
acc4

# model44 <- glm(default~employ+debtinc+creddebt+address, data=train,family='binomial')
# 
# summary(model44)
# 
# pred44 <- predict(model44,data=train,type='response')
# f44 <- table(train$default,pred44>=0.4)
# acc44=sum(diag(f44))/sum(f44)
# 
# acc44


table(train$default)
# #roc curve----
# library(pROC)
# roc1 <- plot.roc(train$default, pred4, main="ROC comparison", percent=TRUE, col= "red")
# 
library(ROCR)
p <- prediction(pred4,train$default)
perf <- performance(p,"tpr","fpr")
plot(perf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#0.3 or 0.25 is good , so that it will predict default that is 1s correctly


# 
# model5 <- glm(default~employ+creddebt+income, data=d,family='binomial')
# summary(model5)
# 
# pred5 <- predict(model5,type='response')
# f5 <- table(d$default,pred5>=0.5)
# acc5=sum(diag(f5))/sum(f5)
# acc5


# acc5
# acc4
# acc3
acc2
acc1

#test----
# default <- test[,9]
# test <- test[,-9]
# dim(test)
# dim(train)
# names(test)


pr <- predict(model4,test, type='response')
str(pr)
pr


t <- table(default,pr>=0.3)
acc44=sum(diag(t))/sum(t)
acc44 #test acc

acc4  #train acc

#Decision trees----
library(party)

tree1 <- ctree(default~., train)
plot(tree1)
plot(tree1,type='simple')

pred1 <- predict(tree1,train)
tab1 <- table(train$default,pred1)
tab1
acc1 <- sum(diag(tab1))/sum(tab1)
acc1



tree2 <- ctree(default~employ+creddebt+address+debtinc, train)
plot(tree2)
plot(tree2,type='simple')

pred2 <- predict(tree2,train)
tab2 <- table(train$default,pred2)
tab2
acc2 <- sum(diag(tab2))/sum(tab2)
acc2





dim(test)
pred <- predict(tree2,test)
tab <- table(default,pred)
tab
acc <- sum(diag(tab))/sum(tab)
acc












