#bank loan (defining positive class is important)
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

# #findings
# str(d)
# d$ed <- as.factor(d$ed)
# d$default <- as.factor(d$default)
# 
# #count
# 
# count_def <- d %>% count(default) 
# count_def
# ggplot(count_def,aes(x=reorder(default,n),y=n))+geom_bar(stat='identity',fill='blue')+
#   geom_text(aes(label=n,vjust=-0.5))
# 
# #education vs defaulters/Non def
# ggplot(d, aes(x=ed,fill=default))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
#   ylab("Count")
# 
# #age dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = age, color = default), binwidth = 2.5) +
#   ylab("Frequency")
# 
# 
# #income dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = income, color = default), binwidth = 2.5) +
#   ylab("Frequency")
# 
# 
# #address dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = address, color = default), binwidth = 2.5) +
#   ylab("Frequency")
# 
# #debtinc dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = debtinc, color = default), binwidth = 2.5) +
#   ylab("Frequency")
# 
# 
# 
# #creddebt dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = creddebt, color = default), binwidth = 2.5) +
#   ylab("Frequency")
# 
# 
# #employ dist of def/Non def
# ggplot(d) + geom_freqpoly(mapping = aes(x = employ, color = default), binwidth = 2.5) +
#   ylab("Frequency")





#Modelling----
names(train)

#model0----
model0 <- glm(default~employ+debtinc+creddebt , data=train,family='binomial')
summary(model0)

pred0 <- predict(model0,type='response')
f0 <- table(train$default,pred1>=0.1)
acc0=sum(diag(f0))/sum(f0)
acc0

library(caret)
conf_matrix_m0_train <- confusionMatrix(as.factor(ifelse(predict(model0, train, type="response") >= 0.1, 1, 0)), train$default,positive = '1')
conf_matrix_m0_train


table(train$default)
# #roc curve-
# library(pROC)
# roc1 <- plot.roc(train$default, pred4, main="ROC comparison", percent=TRUE, col= "red")
# 
library(ROCR)
p0 <- prediction(pred0,train$default)
perf0 <- performance(p0,"tpr","fpr")
plot(perf0,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


auc_m0_train <- performance(p0, measure = "auc")
auc_m0_train<- auc_m0_train@y.values[[1]]
auc_m0_train



pr_0 <- predict(model0,test, type='response')
pr_0
t0 <- table(default,pr_0>=0.1)
t0
acc0_log_test=sum(diag(t0))/sum(t0)
acc0_log_test #test acc
library(caret)
conf_matrix_m0_test <- confusionMatrix(as.factor(ifelse(predict(model0, test, type="response") >= 0.1, 1, 0)), default,positive = '1')
conf_matrix_m0_test

library(ROCR)
p_0 <- prediction(pr_0,default)
perf_0 <- performance(p_0,"tpr","fpr")
plot(perf_0,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


auc_m0_test <- performance(p_0, measure = "auc")
auc_m0_test <- auc_m0_test@y.values[[1]]
auc_m0_test

#model1 summary
conf_matrix_m0_train
conf_matrix_m0_test
auc_m0_train
auc_m0_test







#model1----
model1 <- glm(default~., data=train,family='binomial')
summary(model1)

pred1 <- predict(model1,type='response')
f1 <- table(train$default,pred1>=0.1)
acc1=sum(diag(f1))/sum(f1)
acc1

library(caret)
conf_matrix_m1_train <- confusionMatrix(as.factor(ifelse(predict(model1, train, type="response") >= 0.1, 1, 0)), train$default,positive = '1')
conf_matrix_m1_train


table(train$default)
# #roc curve-
# library(pROC)
# roc1 <- plot.roc(train$default, pred4, main="ROC comparison", percent=TRUE, col= "red")
# 
library(ROCR)
p1 <- prediction(pred1,train$default)
perf1 <- performance(p1,"tpr","fpr")
plot(perf1,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


auc_m1_train <- performance(p1, measure = "auc")
auc_m1_train<- auc_m1_train@y.values[[1]]
auc_m1_train



pr_1 <- predict(model1,test, type='response')
pr_1
t <- table(default,pr_1>=0.1)
t
acc1_log_test=sum(diag(t))/sum(t)
acc1_log_test #test acc
library(caret)
conf_matrix_m1_test <- confusionMatrix(as.factor(ifelse(predict(model1, test, type="response") >= 0.1, 1, 0)), default,positive = '1')
conf_matrix_m1_test

library(ROCR)
p_1 <- prediction(pr_1,default)
perf_1 <- performance(p_1,"tpr","fpr")
plot(perf_1,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


auc_m1_test <- performance(p_1, measure = "auc")
auc_m1_test <- auc_m1_test@y.values[[1]]
auc_m1_test

#model1 summary
conf_matrix_m1_train
conf_matrix_m1_test
auc_m1_train
auc_m1_test


# model2 <- glm(default~ed+employ+creddebt+address, data=train,family='binomial')
# summary(model2)
# 
# pred2 <- predict(model2,type='response')
# f2 <- table(train$default,pred2>=0.5)
# acc2=sum(diag(f2))/sum(f2)
# acc2

# model3 <- glm(default~address+creddebt+employ, data=train,family='binomial')
#  summary(model3)
#  
# pred3 <- predict(model3,type='response')
# f3 <- table(train$default,pred3>=0.3)
# acc3=sum(diag(f3))/sum(f3)
# acc3


model4 <- glm(default~employ+creddebt+address+debtinc,train,family='binomial')

summary(model4)

pred4 <- predict(model4,train,type='response')
f4 <- table(train$default,pred4>=0.1)
acc4_log_train=sum(diag(f4))/sum(f4)
acc4_log_train
table(train$default)

p4 <- prediction(pred4,train$default)
perf4 <- performance(p4,"tpr","fpr")
plot(perf4,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

library(caret)
conf_matrix_m4_train <- confusionMatrix(as.factor(ifelse(predict(model4, train, type="response") >= 0.1, 1, 0)), train$default,positive = '1')
conf_matrix_m4_train



# library(caret)
# p4 <- pred4>=0.3
# p4 <- as.factor(p4)
# class(p4)
# class(train$default)
# str(p4)
# str(train$default)
# f4 <- table(train$default,p4)
# confusionMatrix(f4)



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
# #roc curve
#test----
# default <- test[,9]
# test <- test[,-9]
# dim(test)
# dim(train)
# names(test)


pr <- predict(model4,test, type='response')
str(pr)
pr


t <- table(default,pr>=0.1)
t
acc44_log_test=sum(diag(t))/sum(t)
acc44_log_test #test acc
library(caret)
conf_matrix_m4_test <- confusionMatrix(as.factor(ifelse(predict(model4, test, type="response") >= 0.1, 1, 0)), default,positive = '1')
conf_matrix_m4_test


p1 <- prediction(pr,default)
perf1 <- performance(p1,"tpr","fpr")
plot(perf1,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


auc_m4_test <- performance(p1, measure = "auc")
auc_m4_test <- auc_m4_test@y.values[[1]]
auc_m4_test


# acc4_log_train  #train acc
# table(train$default)
baseacc=363/(363+126)
baseacc
conf_matrix_m4_train
conf_matrix_m4_test
auc_m4_train
auc_m4_test

#model4 summary
conf_matrix_m4_train
conf_matrix_m4_test
auc_m4_train
auc_m4_test

#anova test on models

anova(model1, test="Chisq")
anova(model4, test="Chisq")


#Decision trees----
library(party)

# tree1 <- ctree(default~., train)
# plot(tree1)
# plot(tree1,type='simple')
# 
# pred1 <- predict(tree1,train)
# tab1 <- table(train$default,pred1)
# tab1
# acc1 <- sum(diag(tab1))/sum(tab1)
# acc1
# 
# 
# 
# tree2 <- ctree(default~employ+creddebt+address+debtinc, train)
# plot(tree2)
# plot(tree2,type='simple')
# 
# pred2 <- predict(tree2,train)
# tab2 <- table(train$default,pred2)
# tab2
# acc2 <- sum(diag(tab2))/sum(tab2)
# acc2
# 
# 
# 
# dim(test)
# pred <- predict(tree2,test)
# tab <- table(default,pred)
# tab
# acc <- sum(diag(tab))/sum(tab)
# acc
head(train)
head(test)
library(tree)

#train and test accuracy without pruning
tree_train <- tree(default~., data=train)
summary(tree_train) #accuracy near to 88 percent...23 terminal nodes

pred <- predict(tree_train,test,type='class')
tab <- table(pred,default)
acc_tree <- sum(diag(tab))/sum(tab)
acc_tree #accuracy 72


#train and test accuray with pruning

#pruning params
set.seed(123)
cv <- cv.tree(tree_train,FUN = prune.misclass) #cv tree with function of misclassification error
names(cv)
cv
#plotting
dev.off()
par(mfrow=c(1,2))
plot(cv$size,cv$dev,type='b',col='red',lwd=2)
plot(cv$k,cv$dev,type='b',col='red',lwd=2)

#prune tree
pruned.tree_train <- prune.misclass(tree_train,best=9) #9 terminal nodes is optimal
plot(pruned.tree_train)
text(pruned.tree_train,pretty = 0)

#testing pruned tree

pred1=predict(pruned.tree_train,test,type = 'class')
tab1 <- table(pred1,default)
acc1_P_tree <- sum(diag(tab1))/sum(tab1)
acc1_P_tree #accuracy 72.5..with pruning .9 terminal nodes
acc_tree #accuracy  ..withour pruning. 23 terminal nodes



#random forest----

library(randomForest)

set.seed(1)
rf <- randomForest(default~., data=train,importance=T)
importance(rf)
varImpPlot(rf,col='red')
rf #OOB error is 18.4%


#testing RF
pred2=predict(rf,test,type = 'class')
tab2 <- table(pred2,default)
tab2
acc2_rf <- sum(diag(tab2))/sum(tab2)
acc2_rf #random forest accuracy is 75.35



#summary

acc2_rf #random forest test accuracy is 75.35
rf  #train accurcy is 81.6


acc1_P_tree #accuracy 72.5..with pruning ,9 terminal nodes
summary(pruned.tree_train) #DT pruned train accuracy is 86%


acc_tree # DT test accuracy 72% ..withour pruning. 23 terminal nodes
summary(tree_train) # DT train accuracy is 87.9%


acc44_log_test #log reg test acc is 76.3
acc4_log_train  #log reg train acc is 80.7%



#KNN Classifier----
dim(train)
train$default
dim(test)
default

str(train)
str(test)

#normalization

normalize <- function(x){
  
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(10,20,30,40,50))

#new normalized data
head(train[,-c(2,9)])
head(test[,-2])
train_norm <- as.data.frame(lapply(train[,-c(2,9)], normalize))
test_norm <- as.data.frame(lapply(test[,-2], normalize))
train_labels <- train$default
test_labels <- default

head(train_norm)
head(test_norm)
dim(train_norm)
dim(test_norm)



#prediciton
library(class)

#trainig and testing done at a time and k is sqrt of num of obs.
test_pred <- knn(train_norm,test_norm,cl=train_labels,k=23)
summary(test_pred)
table(test_pred)

#evaluate model

# library(gmodels)
 # CrossTable(x=test_labels, y=test_pred, chisq = F)

conf_matrix_KNN_test <- confusionMatrix(test_pred, test_labels,positive = '1')
conf_matrix_KNN_test

#testing diff k values
i=1
acc=1
for (i in 1:30){
   test_pred <- knn(train_norm,test_norm,cl=train_labels,k=i)
   acc[i] <- sum(test_labels == test_pred)/NROW(test_labels) *100
   j=i
   cat(j,'=',acc[i],'')
   }

max(acc) #k=26 is best for acc



#Accuracy plot
plot(acc)


#Loop evaluation
acc <- c()
spec <- c()
sens <- c()
for (i in 1:30) {
  test_pred <- knn(train_norm,test_norm,cl=train_labels,k=i)
  acc <- c(acc,length(which(test_labels==test_pred)==TRUE)/length(test_labels))
  spec <- c(spec,length(which((test_labels==test_pred) & (test_labels==0))) / length(which(test_labels==0)))
  sens <- c(sens,length(which((test_labels==test_pred) & (test_labels==1))) / length(which(test_labels==1)))
}
evaldf <- data.frame(k=1:30,Accuracy=acc,Sensitivity=sens,Specificity=spec)
evaldf














