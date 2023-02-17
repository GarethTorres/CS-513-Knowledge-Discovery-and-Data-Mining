# Name: Guizhi Xu
# CWID: 20008770

#6.2 Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features.

rm(list=ls())

install.packages('randomForest')

#Import dataset to r
Homework6 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
Homework6

#Remove all the missing values
Homework6_notmissing<-na.omit(Homework6)

#Convert class column to factor
Homework6_notmissing$Class<-factor(Homework6_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(Homework6_notmissing$Class)
is.factor(Homework6_notmissing$Class)

#Get data
set.seed(111)

#30% test data and 70% training data
idx<-sort(sample(nrow(Homework6_notmissing),as.integer(.70*nrow(Homework6_notmissing))))
idx
training<-Homework6_notmissing[idx,]
nrow(training)
test<-Homework6_notmissing[-idx,]
nrow(test)

#Random Forest
library(randomForest)

fit <- randomForest( Class~., data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
dev.off()
RPrediction <- predict(fit, test)
a<-table(actual=test$Class,RPrediction)
a

#Measure error rate
wrong<- (test$Class!=RPrediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Measure accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy