library(tidyverse)
library(cluster)
library(factoextra)

df<-read.csv('wine.csv')
orig<-df
df$class <- NULL
#df<-na.omit(df) #removes missing values
head(df)
df<scale(df) #normalize data by scaling
head(df)

distance<-get_dist(df)
fviz_dist(distance,order = TRUE,
          show_labels = TRUE,
          lab_size = NULL,
          gradient = list(low = "#AB82FF", mid = "white", high = "#D01679"))

set.seed(1234)
mycluster<-kmeans(df,centers=2,nstart=25)
str(mycluster)
print(mycluster)

fviz_cluster(mycluster, data=df)
fviz_nbclust(df,kmeans,method="wss")#elbow method
fviz_nbclust(df,kmeans,method="silhouette")#silhouette method

gap_stat<-clusGap(df, FUN=kmeans, nstart=25, K.max = 10,B=50) # gap statistics method
fviz_gap_stat(gap_stat)

set.seed(1234)
#with optimal number of clusters
mycluster<-kmeans(df,centers=3,nstart=25)
str(mycluster)
print(mycluster)
mycluster$size
mycluster$centers
fviz_cluster(mycluster, data=df)
# Confusion Matrix
table(mycluster$cluster,orig$class)

orig%>%
  mutate(Cluster = mycluster$cluster)%>%
  group_by(Cluster)%>%
  summarize_all("mean")

