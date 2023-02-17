#Name: Guizhi Xu
#CWID: 20008770

#The “breast cancer dataset” in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. 
#The features in the dataset, described below, have been categorized from 1 to 10.
#Use the Naïve Bayes methodology to develop a classification model for the Diagnosis. 
#Important: make sure your categories are represented by the “factor” data type in R and delete the rows with missing values. Use 30% test 70% training data  

rm(list=ls())

install.packages("e1071")
library(e1071)
library(class) 

#Import dataset to r_studio
Homework4 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
Homework4

#Remove all missing values
Homework4_notmissing<-na.omit(Homework4)

#Convert class column to factor
Homework4_notmissing$Class<-factor(Homework4_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(Homework4_notmissing$Class)
is.factor(Homework4_notmissing$Class)

#30% test data and 70% training data
idx<-sort(sample(nrow(Homework4_notmissing),as.integer(.70*nrow(Homework4_notmissing))))
idx
test<-Homework4_notmissing[-idx,-1]
nrow(test)
training<-Homework4_notmissing[idx,-1]
nrow(training)

#Use the Naïve Bayes methodology to develop a classification model for the Diagnosis.
nBayes_class <- naiveBayes(formula = Class~., data =training)
category_class<-predict(nBayes_class,test)

head(cbind(category_class, test))

a4 <-table(NBayes=category_class,Class=test$Class)
a4

NB_wrong<-sum(category_class!=test$Class)

#Measure accuracy
accuracy_a4 <- {sum(diag(a4)/(sum(rowSums(a4)))) * 100}
accuracy_a4

#Measure error rate
NB_error_rate<-NB_wrong/length(category_class)
NB_error_rate