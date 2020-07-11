data2 <- read.csv("~/Desktop/Logistic Reg/bankloan.csv")
View(data)
str(data)
data1 <- select(data, -c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7))
str(data1)
#data1$default<- as.factor(data1$default)
head(data1$default)
summary(data1)
describe(data1)
head(data1)

# Pruning tree

set.seed(3)
cv.bankloan <- cv.tree(modelClassTree, FUN = prune.misclass)
names(cv.bankloan)
cv.bankloan
plot(cv.bankloan$size, cv.bankloan$dev,
     type = 'b', col = 'red', lwd = 2)
plot(cv.bankloan$k, cv.bankloan$dev,
     type = 'b', col = 'blue',
     lwd = 2)
prune.bankloans <- prune.misclass(modelClassTree, best = 3)
plot(prune.bankloans)
text(prune.bankloans, pretty = 0)
tree.pred2 <- predict(prune.bankloans,
                      dataTest, type = 'class')
table(tree.pred2, dataTest$default)
(139+26)/210
# Random Forest
library(randomForest)
set.seed(1)
head(data1)
head(dataTrain)
str(data1)
str(dataTrain)
str(data1$default)
bag.bankloan <- randomForest(default~. ,
                             data = dataTrain, 
                             mtry = 8, importance = T)
dim(dataTrain)                             
importance(bag.bankloan)
varImpPlot(bag.bankloan, col = "red",
           pch = 10, cex =1.25)
bag.bankloan
test.pred.bag <- predict(bag.bankloan,newdata = dataTest, type = 'class')
table(test.pred.bag,dataTest$default)
(137+24)/210
rary(InformationValue)
optimalCutoff <- optimalCutoff(dataTest$default, test.pred.bag)
optimalCutoff
sensitivity(dataTest$default, test.pred.bag,threshold = optimalCutoff)
specificity(dataTest$default, test.pred.bag)
set.seed(1)
rf.bankloan <- randomForest(default~. ,
                            data = dataTrain,
                            mtry = 3, importance = T)
dim(dataTrain)
varImpPlot(rf.bankloan, col = 'blue',
           pch = 10, cex = 1.25)
rf.bankloan
test.pred.rf <- predict(rf.bankloan,
                        newdata = dataTest,type = 'class')
table(test.pred.rf,dataTest$default)
166/210
sensitivity(data_30$default, predict10, threshold = 0.3)
specificity(data_30$default, predict10, threshold = 0.3)

# Apply knn

library(class)
str(dataTrain)
dim(dataTest)
dim(dataTrain)
dataTrain_labels <- dataTrain[1:nrow(dataTrain),9]
dataTest_labels <- dataTest[1:nrow(dataTest),9]
dataTest_Pred <- knn(train = dataTrain, test = dataTest,
                     cl= dataTrain_labels, k = 3)
summary(dataTest_Pred)
161/210
25/55

sensitivity(data_30$default, predict10, threshold = 0.3)
specificity(data_30$default, predict10, threshold = 0.3)

# Evaluate model

library(gmodels)
(CrossTable(x = dataTest_labels, y = dataTest_Pred,
            prop.chisq = F))

library(InformationValue)
