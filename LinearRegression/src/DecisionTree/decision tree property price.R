data <-  read.csv("~/Desktop/datawd/Property price df2.csv")
data
str(data)
summary(data)
dim(data)
head(data)
describe(data)

# Decision Tree

select_rows <- sample(1:nrow(data), round(0.3*nrow(data)
                                          ), replace = F)

data_test<- data[select_rows,]
data_train<- data[-(select_rows) ,]

library(tree)
modelRegTree<- tree(Sale_Price~Overall_Material+ Neighborhood+Construction_Year+Exposure_Level+Total_Floor_area,
                    data = data_train)
plot(modelRegTree)
text(modelRegTree, pretty = 0, cex= 0.75)

predTree<- predict(modelRegTree, newdata = data_test)
summary(predTree)
summary(data_test$Sale_Price)
head(predTree)
residualTree <- data_test$Sale_Price-predTree
head(residualTree)
ME<- sum(data_test$Sale_Price- predTree)/nrow(data_test)
ME
RSS<- sum((data_test$Sale_Price- predTree)^2)
RSS
RMSE<- sqrt(RSS/nrow(data_test))
RMSE
MAPE<- sum(abs(data_test$Sale_Price-predTree)/data_test$Sale_Price)*100/438
MAPE
plot(predTree, residualTree, ylab= "Residual", xlab= "Fitted")
ggplot(data = data_test) +
  geom_jitter(mapping = aes(x = predTree, y = residualTree, col="red"))
ggplot(data = data_test)+
  geom_jitter(mapping = aes(x = Id, y = residualTree, main = "Independence Of error",
                            col = "red"))

ggplot(data_test, aes(residualTree, fill = cut(residualTree,100)))+
  geom_histogram(show.legend = F)

library(caTools)

names(data_train) 

modelRegTree1<- tree(Sale_Price~.,
                     data = data_train)
summary(modelRegTree1)
plot(modelRegTree1)
text(modelRegTree1, pretty = 0, cex= 0.75)

predTree1<- predict(modelRegTree1, newdata = data_test)
head(predTree1)
residualTree1 <- data_test$Sale_Price-predTree1
head(residualTree1)
ME<- sum(data_test$Sale_Price- predTree1)/nrow(data_test)
ME
RSS<- sum((data_test$Sale_Price- predTree1)^2)
RSS
RMSE<- sqrt(RSS/nrow(data_test))
RMSE
MAPE<- sum(abs(data_test$Sale_Price-predTree1)/data_test$Sale_Price)*100/438
MAPE


ggplot(data = data_test) +
  geom_jitter(mapping = aes(x = predTree1, y = residualTree1, col="red"))
ggplot(data = data_test)+
  geom_jitter(mapping = aes(x = Id, y = residualTree1, main = "Independece Of error",
                            col = "red"))
ggplot(data_test, aes(residualTree1, fill = cut(residualTree1,100)))+
  geom_histogram(show.legend = F)






