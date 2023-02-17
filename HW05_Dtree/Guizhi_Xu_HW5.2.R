# Name: Guizhi Xu
# CWID: 20008770

#5.1 Construct a classification and regression tree to classify salary based on the other variables only one split level.
#5.2 Use these categorized features to answer the following questions.
#Important: make sure your categories are represented by the “factor” data type in R and DO NOT replace the missing values.   
#Use the CART methodology to develop a classification model for the Diagnosis. 

rm(list=ls())

# Import all the libraries we need
library(rpart)
library(rattle)          
library(rpart.plot)  		
library(RColorBrewer)     

#Import dataset to r_studio
Homewrok5 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
Homewrok5

#Convert class column to factor
Homewrok5$Class<-factor(Homewrok5$Class, levels = c(2,4), labels = c("benign","malignment"))
View(Homewrok5$Class)
is.factor(Homewrok5$Class)

#Get data
set.seed(111)

#30% test data and 70% training data
idx<-sort(sample(nrow(Homewrok5),as.integer(.70*nrow(Homewrok5))))
idx
training<-Homewrok5[idx,]
nrow(training)
test<-Homewrok5[-idx,]
nrow(test)

#Growing the tree
dev.off()

#plot the tree
CART_class<-rpart(Class~.,data=training[,-1])
rpart.plot(CART_class)
summary(CART_class)

CART_predict2<-predict(CART_class, test, type="class") 

#Creating the Table
table(Actual=test[,11],CART=CART_predict2)

#Measure percentage accuracy
match<-(test[,11]==CART_predict2)*100
accuracy<-sum(match)/length(match)
accuracy

#Measure error rate
wrong<- sum(test[,11]!=CART_predict2)
c_rate<-wrong/length(test[,11])
c_rate

#Use the CART methodology to develop a classification model for the Diagnosis. 
prp(CART_class)
fancyRpartPlot(CART_class)