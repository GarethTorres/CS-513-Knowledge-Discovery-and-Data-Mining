#Clear the memory
rm(list=ls())

# Import data
df <- read.csv("AL_NJ_Income_PCT.csv",na.strings = "?")

# View and summarize the data
View(df)
summary(df)
table(df$State)

# Use the kmeans clustering method to create two clusters for the “AL_NJ_Income_pct” dataset. 
df<-df[,c(-1)]
View(df)
kmeans_data<- kmeans(df[,c(3,4,5,6,7,8)],2,nstart = 10)
kmeans_data$cluster

# Show the cross tabulation of the clusters versus the State feature.
table(kmeans_data$cluster,df[,1])

# Use the hierarchical clustering method and single linkage to create 4 clusters for the the “AL_NJ_Income_pct” dataset.  
data_dist<-dist(df[,c(3,4,5,6,7,8)])
hclust_results<-hclust(data_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,4)

# Show the cross tabulation of the clusters versus the State feature.
table(hclust_2,df[,1])

# Identify the outliers (if any)
boxplot(hclust_2,
        main = "Boxplot on hclust")
outl <- boxplot.stats(hclust_2)$out
mtext(paste("Outliers: ", paste(outl, collapse = ", ")))
