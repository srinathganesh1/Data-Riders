

library(dplyr)
library(car)
library(dummies)
library(psych)

#Data Preprocessing----

train <- read.csv('D:/Data Science/Advan house price/Property_Price_Train.csv')
test <- read.csv('D:/Data Science/Advan house price/Property_Price_Test.csv')
head(train)


plot(train$Overall_Material,train$Sale_Price)
abline(lm(Sale_Price~Overall_Material,data=train),col='blue')

plot(train$Screen_Lobby_Area,train$Sale_Price)
abline(lm(Sale_Price~Screen_Lobby_Area,data=train),col='blue')


plot(train$Kitchen_Quality,train$Sale_Price,xlab='Kitchen Quality',ylab='Sale Price')
#abline(lm(Sale_Price~Kitchen_Quality,data=train),col='blue')
plot(train$Heating_Quality,train$Sale_Price,xlab='Heating Quality',ylab='Sale Price')

plot(train$Air_Conditioning,train$Sale_Price,xlab='Air_Conditioning',ylab='Sale Price')

plot(train$First_Floor_Area,train$Sale_Price,xlab='First_Floor_Area',ylab='Sale Price')
abline(lm(Sale_Price~First_Floor_Area,data=train),col='blue')

plot(train$Second_Floor_Area,train$Sale_Price,xlab='Second_Floor_Area',ylab='Sale Price')
abline(lm(Sale_Price~Second_Floor_Area,data=train),col='blue')

plot(train$Construction_Year,train$Sale_Price,xlab='Construction_Year',ylab='Sale Price')
abline(lm(Sale_Price~Construction_Year,data=train),col='blue')


plot(train$Grade_Living_Area,train$Sale_Price,xlab='Grade_Living_Area',ylab='Sale Price')
abline(lm(Sale_Price~Grade_Living_Area,data=train),col='blue')

# c <- train %>% select_if(is.numeric)
# corr <- cor(c)
# library(ggcorrplot)
# ggcorrplot(c)

df <- rbind(train[,-81],test)
df <- df[,-1]
head(df)
dim(df)


hist(train$Sale_Price)
SP <- train$Sale_Price
hist(SP)
describe(SP)
SP <- log(SP)
head(SP)
hist(SP)
describe(SP)
#summary(df)
head(df) #merged data


#Cat and num varibales
df$Year_Sold <- as.factor(df$Year_Sold)
df$Month_Sold <- as.factor(df$Month_Sold)
df$Garage_Built_Year <- as.factor(df$Garage_Built_Year)
df$House_Condition <- as.factor(df$House_Condition)
df$Construction_Year <- as.factor(df$Construction_Year)
df$Overall_Material <- as.factor(df$Overall_Material)



dfnum <- df %>% select_if(is.numeric)
dfcat <- df %>% select_if(is.factor)
names(dfcat)
names(dfnum)
head(dfnum)
summary(dfnum)


#handling num features----
library(ggplot2)
# ggplot(gather(dfnum),aes(value))+
#   geom_density() +
#   facet_wrap(~key, scales = 'free_x')
library(moments)
#skewness
dfnumskew <- skewness(dfnum,na.rm=T)
dfnumskew <- dfnumskew[dfnumskew > 0.7]
dfnumskew

dfnum[dfnumskew,] <- log1p(dfnum[dfnumskew,])# apply log + 1 transformation for all numeric features with skewnes over 0.75
dim(dfnum)
head(dfnum)

#missing values
sum(is.na(dfnum))

apply(df,2,function(col)round(sum(is.na(col))/length(col)*100,2))

library(naniar)
#vis_miss(dfnum)
names(dfnum)
drop <- c("Lot_Extent")
dfnum = dfnum[,!(names(dfnum) %in% drop)]
#vis_miss(dfnum)
library(VIM)
dfnum <- kNN(dfnum)
dim(dfnum)
head(dfnum)
dfnum <- dfnum[,1:29]



#vis_miss(dfnum)

#handling Cat Features----
#missing values
sum(is.na(dfcat))
apply(dfcat,2,function(col)round(sum(is.na(col))/length(col)*100,2))
#vis_miss(dfcat)

head(dfcat)
tail(dfcat$Pool_Quality)
sum(is.na(dfcat$Pool_Quality))


levels <- levels(dfcat$Pool_Quality)
levels[length(levels) + 1] <- "None"
dfcat$Pool_Quality <- factor(dfcat$Pool_Quality, levels = levels)# refactor Species to include "None" as a factor level
dfcat$Pool_Quality[is.na(dfcat$Pool_Quality)] <- "None"# and replace NA with "None"
head(dfcat$Pool_Quality)
class(dfcat$Pool_Quality)

levels <- levels(dfcat$Fence_Quality)
levels[length(levels) + 1] <- "None"
dfcat$Fence_Quality <- factor(dfcat$Fence_Quality, levels = levels)
dfcat$Fence_Quality[is.na(dfcat$Fence_Quality)] <- "None"

levels <- levels(dfcat$Miscellaneous_Feature)
levels[length(levels) + 1] <- "None"
dfcat$Miscellaneous_Feature <- factor(dfcat$Miscellaneous_Feature, levels = levels)
dfcat$Miscellaneous_Feature[is.na(dfcat$Miscellaneous_Feature)] <- "None"

levels <- levels(dfcat$Lane_Type)
levels[length(levels) + 1] <- "None"
dfcat$Lane_Type <- factor(dfcat$Lane_Type, levels = levels)
dfcat$Lane_Type[is.na(dfcat$Lane_Type)] <- "None"

levels <- levels(dfcat$Fireplace_Quality)
levels[length(levels) + 1] <- "None"
dfcat$Fireplace_Quality <- factor(dfcat$Fireplace_Quality, levels = levels)
dfcat$Fireplace_Quality[is.na(dfcat$Fireplace_Quality)] <- "None"

levels <- levels(dfcat$Garage)
levels[length(levels) + 1] <- "None"
dfcat$Garage <- factor(dfcat$Garage, levels = levels)
dfcat$Garage[is.na(dfcat$Garage)] <- "None"

levels <- levels(dfcat$Garage_Built_Year)
levels[length(levels) + 1] <- "None"
dfcat$Garage_Built_Year <- factor(dfcat$Garage_Built_Year, levels = levels)
dfcat$Garage_Built_Year[is.na(dfcat$Garage_Built_Year)] <- "None"

levels <- levels(dfcat$Garage_Finish_Year)
levels[length(levels) + 1] <- "None"
dfcat$Garage_Finish_Year <- factor(dfcat$Garage_Finish_Year, levels = levels)
dfcat$Garage_Finish_Year[is.na(dfcat$Garage_Finish_Year)] <- "None"

levels <- levels(dfcat$Garage_Quality)
levels[length(levels) + 1] <- "None"
dfcat$Garage_Quality <- factor(dfcat$Garage_Quality, levels = levels)
dfcat$Garage_Quality[is.na(dfcat$Garage_Quality)] <- "None"

levels <- levels(dfcat$Garage_Condition)
levels[length(levels) + 1] <- "None"
dfcat$Garage_Condition <- factor(dfcat$Garage_Condition, levels = levels)
dfcat$Garage_Condition[is.na(dfcat$Garage_Condition)] <- "None"

levels <- levels(dfcat$Basement_Height)
levels[length(levels) + 1] <- "None"
dfcat$Basement_Height <- factor(dfcat$Basement_Height, levels = levels)
dfcat$Basement_Height[is.na(dfcat$Basement_Height)] <- "None"

levels <- levels(dfcat$Basement_Condition)
levels[length(levels) + 1] <- "None"
dfcat$Basement_Condition <- factor(dfcat$Basement_Condition, levels = levels)
dfcat$Basement_Condition[is.na(dfcat$Basement_Condition)] <- "None"

levels <- levels(dfcat$BsmtFinType1)
levels[length(levels) + 1] <- "None"
dfcat$BsmtFinType1 <- factor(dfcat$BsmtFinType1, levels = levels)
dfcat$BsmtFinType1[is.na(dfcat$BsmtFinType1)] <- "None"

levels <- levels(dfcat$BsmtFinType2)
levels[length(levels) + 1] <- "None"
dfcat$BsmtFinType2 <- factor(dfcat$BsmtFinType2, levels = levels)
dfcat$BsmtFinType2[is.na(dfcat$BsmtFinType2)] <- "None"

levels <- levels(dfcat$Exposure_Level)
levels[length(levels) + 1] <- "None"
dfcat$Exposure_Level <- factor(dfcat$Exposure_Level, levels = levels)
dfcat$Exposure_Level[is.na(dfcat$Exposure_Level)] <- "None"

levels <- levels(dfcat$Exterior1st)
levels[length(levels) + 1] <- "None"
dfcat$Exterior1st <- factor(dfcat$Exterior1st, levels = levels)
dfcat$Exterior1st[is.na(dfcat$Exterior1st)] <- "None"

levels <- levels(dfcat$Exterior2nd)
levels[length(levels) + 1] <- "None"
dfcat$Exterior2nd <- factor(dfcat$Exterior2nd, levels = levels)
dfcat$Exterior2nd[is.na(dfcat$Exterior2nd)] <- "None"

levels <- levels(dfcat$Brick_Veneer_Type)
levels[length(levels) + 1] <- "No"
dfcat$Brick_Veneer_Type <- factor(dfcat$Brick_Veneer_Type, levels = levels)
dfcat$Brick_Veneer_Type[is.na(dfcat$Brick_Veneer_Type)] <- "No"

levels <- levels(dfcat$Zoning_Class)
levels[length(levels) + 1] <- "No"
dfcat$Zoning_Class <- factor(dfcat$Zoning_Class, levels = levels)
dfcat$Zoning_Class[is.na(dfcat$Zoning_Class)] <- "No"

levels <- levels(dfcat$Electrical_System)
levels[length(levels) + 1] <- "No"
dfcat$Electrical_System <- factor(dfcat$Electrical_System, levels = levels)
dfcat$Electrical_System[is.na(dfcat$Electrical_System)] <- "No"

levels <- levels(dfcat$Kitchen_Quality)
levels[length(levels) + 1] <- "No"
dfcat$Kitchen_Quality <- factor(dfcat$Kitchen_Quality, levels = levels)
dfcat$Kitchen_Quality[is.na(dfcat$Kitchen_Quality)] <- "No"

levels <- levels(dfcat$Functional_Rate)
levels[length(levels) + 1] <- "No"
dfcat$Functional_Rate <- factor(dfcat$Functional_Rate, levels = levels)
dfcat$Functional_Rate[is.na(dfcat$Functional_Rate)] <- "No"

levels <- levels(dfcat$Utility_Type)
levels[length(levels) + 1] <- "No"
dfcat$Utility_Type <- factor(dfcat$Utility_Type, levels = levels)
dfcat$Utility_Type[is.na(dfcat$Utility_Type)] <- "No"

levels <- levels(dfcat$Sale_Type)
levels[length(levels) + 1] <- "No"
dfcat$Sale_Type <- factor(dfcat$Sale_Type, levels = levels)
dfcat$Sale_Type[is.na(dfcat$Sale_Type)] <- "No"

#vis_miss(dfcat)
#vis_miss(dfnum)
head(dfnum)
#Encoding cat var----
#dealing cat variable with dummies
apply(dfcat, 2, function(x) length(unique(x)))
names(dfcat)
dim(dfcat)
cor(train$Construction_Year,train$Sale_Price)
cor(train$Garage_Built_Year,train$Sale_Price)
#train$Neighborhood <- as.numeric(train$Neighborhood)
cor(train$Neighborhood,train$Sale_Price)
#train$Exterior2nd <- as.numeric(train$Exterior2nd)
cor(train$Exterior2nd,train$Sale_Price)
dfcat <- subset(dfcat, select=-c(Construction_Year, Garage_Built_Year,Exterior2nd,Neighborhood))
dim(dfcat)


dfcatdum <- dummy.data.frame(dfcat)
dim(dfcatdum)
head(dfcat)
#model----

dat <- data.frame(dfcatdum,dfnum)
head(dat)
dim(dat)

X <- dat[1:1459,]
X$Sale_Price <- SP


y <- dat[1460:2918,]
dim(X)
dim(y)


model1 <- lm(Sale_Price~. , data=X)
summary(model1)
summary(model1)$coefficients
head(summary(model1)$coefficients[,4])
a <- (summary(model1)$coefficients[-1,4])#without intercept
a <- a[a<=0.05]
vars1 <- names(a)
paste(vars1,collapse = '+')


model2 <- lm(Sale_Price~Zoning_ClassCommer+Zoning_ClassFVR+Zoning_ClassRHD+Zoning_ClassRLD+Lot_ConfigurationCulDSac+Property_SlopeGS+Property_SlopeMS+Condition2PosN+Condition2RRAe+House_TypeTwnhs+Overall_Material1+Overall_Material2+Overall_Material3+Overall_Material4+Overall_Material5+Overall_Material6+Overall_Material7+House_Condition1+House_Condition3+House_Condition4+House_Condition5+House_Condition6+House_Condition7+Roof_DesignFlat+Roof_DesignGable+Roof_DesignGambrel+Roof_DesignHip+Roof_DesignMansard+Roof_QualityCT+Roof_QualityM+Exterior1stBrkComm+Exterior_ConditionPo+Basement_ConditionPo+BsmtFinType1GLQ+Heating_TypeGrav+Heating_QualityEx+Air_ConditioningN+Kitchen_QualityEx+Functional_RateMajD2+Functional_RateMD+Functional_RateSD+Garage2Types+GarageAttchd+GarageDetchd+Garage_Finish_YearRFn+Sale_TypeConLD+Lot_Size+Remodel_Year+First_Floor_Area+Second_Floor_Area+LowQualFinSF+Grade_Living_Area+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area
             ,data=X)
summary(model2)
vif(model2)[vif(model2)>10]
n <- names(vif(model2)[vif(model2)>10])

vars2 <- vars1[!vars1%in%n]


paste(vars2,collapse = '+')

model3 <- lm(Sale_Price~Zoning_ClassCommer+Zoning_ClassFVR+Zoning_ClassRHD+Zoning_ClassRLD+Lot_ConfigurationCulDSac+Condition2PosN+House_TypeTwnhs+Overall_Material1+Overall_Material2+Overall_Material3+Overall_Material4+Overall_Material5+Overall_Material6+Overall_Material7+House_Condition3+House_Condition4+House_Condition5+House_Condition6+Roof_QualityCT+BsmtFinType1GLQ+Heating_TypeGrav+Heating_QualityEx+Air_ConditioningN+Kitchen_QualityEx+Functional_RateMajD2+Functional_RateSD+Garage2Types+Lot_Size+Remodel_Year+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area
             ,data=X)

summary(model3)

b <- (summary(model3)$coefficients[-1,4])#without intercept
b <- b[b<=0.05]
vars3 <- names(b)
paste(vars3,collapse = '+')



paste(vars3,collapse = '+')

model4 <- lm(Sale_Price~Zoning_ClassCommer+Zoning_ClassFVR+Zoning_ClassRHD+Zoning_ClassRLD+Lot_ConfigurationCulDSac+Condition2PosN+House_TypeTwnhs+Overall_Material1+Overall_Material2+Overall_Material3+Overall_Material4+Overall_Material5+Overall_Material6+Overall_Material7+House_Condition3+House_Condition4+House_Condition5+House_Condition6+Roof_QualityCT+BsmtFinType1GLQ+Heating_TypeGrav+Heating_QualityEx+Air_ConditioningN+Kitchen_QualityEx+Functional_RateMajD2+Functional_RateSD+Garage2Types+Lot_Size+Remodel_Year+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area
             ,data=X)

summary(model4)
plot(model4)


vif(model4)
vif(model4)[vif(model4)>5]


#predictions
pred <- exp(predict(model4,data=X))
head(pred)
head(train$Sale_Price)
head(exp(X$Sale_Price))
sqrt(mean(pred-train$Sale_Price)^2)

#prediction on test data
pred1 <- exp(predict(model4,y))
head(pred1)



# #splitting train again into train80 for model and train 20 for testing
# # 
# X <- dat[1:1459,]
# X$Sale_Price <- SP
# dim(X)
# head(X)
# 
# set.seed(123)
# ind <- sample(2,nrow(X),replace=T,prob=c(0.7,0.3))
# X80 <- X[ind==1,]
# X20 <- X[ind==2,]
# 
# dim(X80)
# dim(X20)
# head(X20)
# head(X80)
# model4 <- lm(Sale_Price~Zoning_ClassCommer+Zoning_ClassFVR+Zoning_ClassRHD+Zoning_ClassRLD+Lot_ConfigurationCulDSac+Condition2PosN+House_TypeTwnhs+Overall_Material1+Overall_Material2+Overall_Material3+Overall_Material4+Overall_Material5+Overall_Material6+Overall_Material7+House_Condition3+House_Condition4+House_Condition5+House_Condition6+Roof_QualityCT+BsmtFinType1GLQ+Heating_TypeGrav+Heating_QualityEx+Air_ConditioningN+Kitchen_QualityEx+Functional_RateMajD2+Functional_RateSD+Garage2Types+Lot_Size+Remodel_Year+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Garage_Size+Screen_Lobby_Area
#              ,data=X80)
# 
# summary(model4)
# 
# 
# #predictions on X80
# pred <- exp(predict(model4,data=X80))
# head(pred)
# X80$Sale_Price
# #head(train$Sale_Price)
# exp(X80$Sale_Price)
# z <- (pred-exp(X80$Sale_Price))^2
# sqrt(mean(z))
# 
# #prediction on X20
# pred1 <- exp(predict(model4,X20))
# head(pred1)
# head(X20$Sale_Price)
# exp(X20$Sale_Price)
# exp(X20$Sale_Price)
# z1 <- (pred1-exp(X20$Sale_Price))^2
# sqrt(mean(z1))
# 


#Decision trees----
library(party)
head(train)
tree1 <- ctree(Sale_Price~., train)
plot(tree1)
plot(tree1,type='simple')


pred <- predict(tree1,data=train)
head(pred)
head(train$Sale_Price)
#head(exp(X$Sale_Price))
sqrt(mean(pred-train$Sale_Price)^2)


#prediction on test data
pred1 <- predict(tree1,test)
head(pred1)



