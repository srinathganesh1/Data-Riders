str(cereals_data)
summary(cereals_data)

#mean of the data

mean(cereals_data$calories)
mean(cereals_data$fat)
mean(cereals_data$sodium)
mean(cereals_data$fiber)
mean(cereals_data$carbo)
mean(cereals_data$sugars)
mean(cereals_data$sugars, na.rm = TRUE)
mean(cereals_data$potass)
mean(cereals_data$potass, na.rm = TRUE)
mean(cereals_data$vitamins)
mean(cereals_data$shelf)
mean(cereals_data$weight)
mean(cereals_data$cups)
mean(cereals_data$rating)

hist(cereals_data$calories, main = "Histogram of Calories", col = "red")
hist(cereals_data$protein, main = "Histogram of Protein", col = "red")
hist(cereals_data$fat, main = "Histogram of Fat", col = "red")
hist(cereals_data$sodium, main = "Histogram of Sodium", col = "red")
hist(cereals_data$fiber, main = "Histogram of fiber", col = "red")
hist(cereals_data$carbo, main = "Histogram of carbo", col = "red")
hist(cereals_data$sugars, main = "Histogram of Sugars", col = "red")
hist(cereals_data$potass, main = "Histogram of Potass", col = "red")
hist(cereals_data$rating, main = "Histogram of Rating", col = "red")


# boxplot of the nutrients
boxplot(cereals_data$calories, main = "Boxplot of Calories", col = "yellow")
boxplot(cereals_data$protein, main = "Boxplot of Protein", col = "yellow")
boxplot(cereals_data$fat, main = "Boxplot of Fat", col = "yellow")
boxplot(cereals_data$sodium, main = "Boxplot of Sodium", col = "yellow")
boxplot(cereals_data$fiber, main = "Boxplot of Fiber", col = "yellow")
boxplot(cereals_data$carbo, main = "Boxplot of Carbo", col = "yellow")
boxplot(cereals_data$sugars, main = "Boxplot of Sugars", col = "yellow")
boxplot(cereals_data$potass, main = "Boxplot of Potass", col = "yellow")

cer<-cereals_data
View(cer)
str(cer)

#describing the ratings variable
plot(density(cer$rating), main = "Density plot of ratings")
polygon(density(cer$rating), col = "red")


boxplot(cer$rating)

shapiro.test(cer$rating)

library(psych)
#library(psych)
describe(cer$rating)

#just to identfy which cereals have highest/lowest ratings
cer_1<-cer[with(cer, order(rating)),]
View(cer_1)

cer_2<-cer[with(cer, order(-rating)),]
View(cer_2)
head(cer_2$rating)

#creating new varible calories_ with two categories
c<-within(cer, {
  calories_1<-NA
  calories_1[calories<=100] <- "<= 100"
  calories_1[calories>100]  <- ">100"
})
names(c)
str(c)
View(c)
t<-as.factor(c$calories_1)
str(t)
t

#frequencies
table(c$calories_1)

# what is the average rating for each group?
tapply(c$rating, c$calories_1, mean)


# check normality of the rating variable in both groups
low_cal<-subset(c, calories_1 == "<= 100")
head(low_cal)
mean(low_cal$rating)

shapiro.test(low_cal$rating)

high_cal<-subset(c, calories_1 == ">100")
mean(high_cal$rating)
shapiro.test(high_cal$rating)

#now that both p valus are >0.05, we confirm normality in both groups and run independent t test#
t.test(c$rating~c$calories_1)

# checking for manufacturer and type
table<-table(c$mfr, c$calories_1)
table
chisq.test(table)
# significant which means we can say that their calories levels are different,
#but dont know if this is relevant

table(c$type)
table(c$type, c$calories_1)


#Removing missing values
install.packages("VIM")
library(VIM)

cer_3 <-kNN(cereals_data)
str(cer_3)
View(cer_3)
names(cer_3)

# converting mfr into a catagorical variable
cer_3$mfr <- as.factor(cer_3$mfr)
str(cer_3)

mfr_rat <- tapply(cer_3$rating, cer_3$mfr, mean )
View(mfr_rat)
head(mfr_rat)

cal_rat <- tapply(cer_3$calories, cer_3$mfr, mean )
head(cal_rat)

# annova test to check for mean rating across different catagories of manufacturers
results <- aov(rating~mfr, data = cer_3)
summary(results)
TukeyHSD(results)
plot(rating~mfr, data = cer_3, col = heat.colors(7))


#remove unwanted columns
cer_4<-subset(cer_3, select = c(1:17))
names(cer_4)

#Mutate all varables to "per 100 calories)
install.packages("dplyr")
library(dplyr)

cal_100<- mutate(cer_4, cups_100cal = cups*100/calories)
head(cal_100)

#reducing decimal spaces
round(cal_100$cups_100cal, 2)
View(cal_100)# doesnt work in view

cal_100<-mutate(cal_100, fiber_100cal = fiber*100/calories)
cal_100<-mutate(cal_100, potass_100cal = potass*100/calories)
cal_100<-mutate(cal_100, protein_100cal = protein*100/calories)
cal_100<-mutate(cal_100, carbo_100cal = carbo*100/calories)
cal_100<-mutate(cal_100, sugars_100cal = sugars*100/calories)
cal_100<-mutate(cal_100, fat_100cal = fat*100/calories)
cal_100<-mutate(cal_100, vitamins_100cal = vitamins*100/calories)
cal_100<-mutate(cal_100, sodium_100cal = sodium*100/calories)
cal_100<-mutate(cal_100, potass_100cal = potass*100/calories)
cal_100<-mutate(cal_100, weight_100cal = weight*100/calories)
View(cal_100)
write.csv(cal_100, "C:/carrer/DSP-29/project 1/cal_100.csv")


hist(cal_100$sugars_100cal)

# Arranging dataframe, cal_100 in descending order
cereal_grid <- arrange(cal_100, desc(fiber_100cal, potass_100cal, protein_100cal, carbo_100cal, sugars_100cal, fat_100cal, vitamins_100cal, sodium_100cal, weight_100cal))
View(cereal_grid)
names(cereal_grid)

# subsetting the dataframe
cereal_grid <- subset(cereal_grid, select = c(1,18,19,20,21,22,23,24,25,26,27))

# rounding the decimals points upto 2 decimals places
install.packages("forestmangr")
library(forestmangr)
cereal_grid <- round_df(cereal_grid, digits = 2)
View(cereal_grid)
head(cereal_grid)

# Cluster Analysis begins from this point: data: cereal_grid
#changing rownames
names(cereal_grid)
crl <-cereal_grid[ ,c(5,6,7,8)]
View(crl)

row.names(crl)<-cereal_grid$name
View(crl)

df<-crl

dim(df)

#due to divergence in magnitudes, the data will be scaled. 
#From each value, mean will be subtracted and this will be divided by std. deviation
#we could have nornalised also.# See formula in notes
df<-scale(df)
View(df)
head(df, 3)

#install the following packages for cluster analysis
install.packages(c('cluster', 'factoextra'))# This has been done on Naina's pc.
library(cluster)
library(factoextra)

#check on a sample of 15 observations whether cluster analysis can be done on them

set.seed(123)
ss<-sample(1:77, 15)

df1<-crl[ss, ]
head(df1, 3)


df1.scaled<-scale(df1)
head(df1.scaled, 3)

# get euclidean distance between the points
dist.eucl_15<-dist(df1.scaled, method ='euclidean')
head(dist.eucl_15, 3)

#convert this into a matrix to see distance between each combination of states
round(as.matrix(dist.eucl_15)[1:3, 1:3], 1)

#to visualise the dissimilarity matrix
fviz_dist(dist.eucl_15) #this tells us that data can be custered#

#Now we do clustering on the entire data - make clusters +
#find the optimal nuber of clusters
fviz_nbclust(df, kmeans, method = 'wss') + geom_vline(xintercept = 5, linetype = 5, col = 'red')

#kMeans-clustering
set.seed(123)
km.res<-kmeans(df, 5, nstart = 25)
km.res

km.res$totss
km.res$betweenss

#see means of variables in original data, clusterwise

aggregate(crl, by = list(cluster = km.res$cluster), mean)

#Add cluster membership in data file
df_m<-cbind(crl, cluster = km.res$cluster)
head(df_m)
View(df_m)
write.csv(df_m, "C:/carrer/DSP-29/project 1/cluster_final.csv")

fviz_cluster(km.res, data = df,
             palette = c('orange', 'violetred2', 'blue', 'green4', 'red'),
             ellipse.type = 'euclid',
             star.plot = TRUE,
             REPEL = TRUE,
             ggtheme = theme())

#checking whether difference in means is significant
View(df)
View(df_m)
dim(df_m)
str(df_m)
df_anova<-df_m
str(df_anova)

#convert the cluster variable into a factor
df_anova$cluster<-as.factor(df_anova$cluster)
str(df_anova)
#Proteins
plot(protein_100cal~cluster, data = df_anova, col = topo.colors(5))
results<-aov(protein_100cal~cluster, data = df_anova)
summary(results)
TukeyHSD(results)
#Carbohydrates
plot(carbo_100cal~cluster, data = df_anova, col = heat.colors(5))

results_1<-aov(carbo_100cal~cluster, data = df_anova)
summary(results_1)
TukeyHSD(results_1)

#Sugars
plot(sugars_100cal~cluster, data = df_anova, col = topo.colors(5))

results_2<-aov(sugars_100cal~cluster, data = df_anova)
summary(results_2)
TukeyHSD(results_2)

#fat
plot(fat_100cal~cluster, data = df_anova, col = heat.colors(5))

results_3<-aov(fat_100cal~cluster, data = df_anova)
summary(results_3)
TukeyHSD(results_3)