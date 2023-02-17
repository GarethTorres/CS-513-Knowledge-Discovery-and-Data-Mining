# Name: Guizhi Xu
# CWID: 20008770

#6.1 Use the C5.0 methodology to develop a classification model for the Diagnosis.  

rm(list=ls())

#install.packages("C50", repos="http://R-Forge.R-project.org")
install.packages("C50")
library('C50')

#Import dataset to r_studio
Homework6 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
Homework6

#Convert class column to factor
Homework6$Class<-factor(Homework6$Class, levels = c(2,4), labels = c("benign","malignment"))
View(Homework6$Class)
is.factor(Homework6$Class)

#Get data
set.seed(111)

#30% test data and 70% training data
idx<-sort(sample(nrow(Homework6),as.integer(.70*nrow(Homework6))))
idx
training<-Homework6[idx,]
nrow(training)
test<-Homework6[-idx,]
nrow(test)

#C50
C50_class <- C5.0(Class~.,training[,-1] )
summary(C50_class )
dev.off()
plot(C50_class)

C50_predict<-predict( C50_class ,test[,-1] , type="class" )
str(C50_predict)
a<-table(actual=test[,11],C50=C50_predict)

#Measure error rate
wrong<- (test[,11]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,11])
c50_rate

#Measure accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy