# Name   : Guizhi Xu
# CWID   : 20008770

rm(list=ls())

#Importing csv file to r file
HW_08 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_08
nrow(HW_08)
table(HW_08$diagnosis)

#Removing missing values
HW_08<-na.omit(HW_08)
View(HW_08)
nrow(HW_08)

HW_08<-HW_08[-1]

#Kmeans
kmeans_A<- kmeans(HW_08[,-1],2,nstart = 10)
kmeans_A$cluster
plotcluster(HW_08[,-1], kmeans_A$cluster)
kmeans_A$centers
table(kmeans_A$cluster,HW_08[,1])