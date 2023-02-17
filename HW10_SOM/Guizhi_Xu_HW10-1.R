# Name   : Guizhi Xu
# CWID   : 20008770
# HW_10_SOM

# Clearing objects of R-environment
rm(list=ls())

#Importing csv to r
HW_10 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_10

#Removing missing values
HW_10<-na.omit(HW_10)
View(HW_10)


rdata <- HW_10[-1]
diagnosis <- as.factor(HW_10$diagnosis)
diagnosis <- data.frame(diagnosis)
rdata <- cbind(data.frame(diagnosis), rdata[-1])

data <- rdata[,2:13]

grp <- class::somgrid(topo = "hexagonal")
rdata.som <- class::SOM(data, grp)
plot(rdata.som)

# plot

rdata.som2 <- class::SOM(data,grp,alpha = list(seq(0.05,0,len=1e4), seq(0.02,0,len=1e5)), 
                         radii = list(seq(8,1,len=1e4), seq(4,1,len=1e5)))
plot(rdata.som2)

training<-HW_10[,-1],2,nstart = 10)
HW_10_som<-som(as.matrix(training), grid = somgrid(3,1))

summary(HW_10_som)
str(HW_10_som)
HW_10_som$unit.classif

table(cluster=HW_10_som$unit.classif,HW_10[,1])

plot(HW_10_som)

summary(HW_10_som)

map(HW_10_som,as.matrix(HW_10[,1]))