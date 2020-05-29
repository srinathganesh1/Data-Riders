#----Cluster Analysis----

#----Data Preparation----
#objective-we were to choose any 4 variables(calories must)
#remove NAs and do scaling or normalizing to perform CA

df <- imp_data

str(df)
summary(df)
names(df)
df <- df[,c('calories','protein','fat','rating')]
dim(df)
View(df)
row.names(df) <- imp_data[,1]
df <- scale(df)
head(df,3)


#install cluster, factoextra
library(cluster)
library(factoextra)


#----Assessing tha data for cluster tendancy -

#visual asses by dist matrix by taking sample rows(here 15) from data
set.seed(123)
ind <- sample(1:50,15)
df1 <- df[ind,]
head(df1,3)
df1.scaled <- scale(df1)
str(df1)
head(df1,3)
dist.ecul_15 <- dist(df1,method = "euclidean")
as.matrix(dist.ecul_15) 
round(as.matrix(dist.ecul_15)[1:3,1:3],1)
fviz_dist(dist.ecul_15)#clustering possible


#----Find optimal No of clusters----.
# 1.wss 

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 5,col="red")+
  labs(subtitle = "Elbow method")


#----1.K means Clustering----
#compute
set.seed(123) # for reproducibility
km.res <- kmeans(df, 5, nstart = 25)#df is scaled data, default uses euclidean method
km.res
km.res$cluster
# Visualize
fviz_cluster(km.res, data = df, col=rainbow(8),
             star.plot=T,
             repel = T,
             ellipse.type = "euclid",
             ggtheme = theme())
fviz_cluster(km.res, data = df, palette = "jco",
             ggtheme = theme_minimal())

#adding membership to original data
df_km <- cbind(imp_data,clustertype=km.res$cluster)
head(df_km)
View(df_km)

#Means of all variables clusterwise in org data

aggregate(df_km,list(cluster=km.res$cluster),mean)#vecotr wont work so made list

#derive insights from custerwise means
#1.Cluster 4 has high rated cereals because it is rich in fiber,K and low sugar,fat,carbo,less weight.
#2.Cluster 1 has very poor rated cereals as it opp to cluster 4.






