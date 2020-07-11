data <- read.csv("~/Desktop/Logistic Reg/bankloan.csv")
View(data)
str(data)
data1 <- select(data, -c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7))
str(data1)

#data1$default<- as.factor(data1$default)

head(data1$default)
summary(data1)
describe(data1)
head(data1)

# Decision Tree

library(tree)
library(caTools)
set.seed(1)
split <- sample.split(data1$default, SplitRatio = 0.70)
dataTrain <- subset(data1, split == T)
dataTest <- subset(data1, split == F)
table(data1$default)
table(dataTrain$default)
table(dataTest$default)
prop.table(table(dataTest$default))
prop.table(table(dataTrain$default))
modelClassTree <- tree(default~. , data = dataTrain)
summary(modelClassTree)
plot(modelClassTree)
text(modelClassTree, pretty = 0, cex = 0.75 )
pred <- round(predict(modelClassTree, newdata = dataTest), digits = 3)

head(pred)
str(pred)
pred<- ifelse(pred>0.14, yes = 1, no= 0)

head(pred)
library(class)

conf <- table(dataTest$default, pred)
conf
OAA <- (conf[1,1]+conf[2,2])/sum(conf)
OAA
confusionMatrix(dataTest$default,pred,  threshold = 0.14)
misClassError(dataTest$default, pred, threshold = 0.14)

optimalCutoff <- optimalCutoff(dataTest$default,pred)
optimalCutoff

sensitivity(dataTest$default, pred,threshold = 0.14)
specificity(dataTest$default, pred, threshold = 0.14)

library(ResourceSelection)



chisq.test(data1$address, data1$default)
chisq.test(data1$age, data1$default)
chisq.test(data1$creddebt, data1$default)
chisq.test(data1$debtinc, data1$default)
chisq.test(data1$othdebt, data1$default)
chisq.test(data1$employ, data1$default)


