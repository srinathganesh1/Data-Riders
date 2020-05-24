setwd('D:/Data Science/Dr Vinod online classes/Mini project')
getwd()
data <- read.csv('D:/Data Science/Dr Vinod online classes/Mini project/cereals_data.csv')
View(data)
str(data)
summary(data)
sum(complete.cases(data))# 4NAs

#----missing value----
data[21,] #1 NA
data[5,]#1 NA
data[58,]#2 NA


library(VIM)
library(dplyr)
imp_data <- kNN(data)
imp_data <- imp_data[,-c(17:32)]
dim(imp_data)
summary(imp_data)







