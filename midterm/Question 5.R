# Name        : Guizhi Xu
# CWID        : 20008770

#Load the CANVAS “IBM_attrition_v2B.CSV” dataset into R/Python. 
#Remove the missing values.   
#Construct a knn (k=3) model to classify attrition (attrition=’yes’) based on the other variables. 
#Predict attrition for a random sample (30%) of the data (test dataset). 
#Measure the accuracy, precision, recall and F1 of the model.

rm(list=ls())

#Load the CANVAS “IBM_attrition_v2B.CSV” dataset into R/Python. 
midtermquestion5 <- read.csv("IBM_attrition_v2B.csv", header = TRUE, na.strings = c("?"))
midtermquestion5

#Remove the missing values.
midtermquestion5_notmissing<-na.omit(midtermquestion5)

#Convert class column to factor
midtermquestion5_notmissing$Attrition <- factor(midtermquestion5_notmissing$Attrition, labels = c("yes","no"))
midtermquestion5_notmissing
is.factor(midtermquestion5_notmissing$Attrition)

#Predict attrition for a random sample (30%) of the data (test dataset). 
idx<-sort(sample(nrow(midtermquestion5_notmissing),as.integer(.70*nrow(midtermquestion5_notmissing))))
idx
training<-midtermquestion5_notmissing[idx,]
nrow(training)
test<-midtermquestion5_notmissing[-idx,]
nrow(test)

#Use the R library("kknn") 
library(kknn)

#knn (k=3)
predict_k3<-kknn(formula = Attrition~., training, test, k=3, kernel = "triangular")
fit <- fitted(predict_k3)
n3 <- table(Actual=test$Attrition,Fitted=fit)
n3

#Measure the accuracy
accuracy_k3 <- {sum(diag(n3)/(sum(rowSums(n3)))) * 100}
accuracy_k3

#Measure the precision
precision <- {conf_mat[1,1]/sum(conf_mat[1,1:2])*100}
precision

#Measure the recall
recall <- {conf_mat[1,1]/sum(conf_mat[1:2,1])*100}
recall

#Measure the F1
f1_score <- {(2*precision*recall)/(precision+recall)*100}
f1_score
