# Guizhi Xu
# CWID:20008770

# The “breast cancer dataset” in CANVAS was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. The features in the dataset, described below, have been categorized from 1 to 10.
# Use the knn methodology (k = 3,5 and 10) to develop a classification models for the Diagnosis. 
# Important: make sure your categories are represented by the “factor” data type in R and delete the rows with missing value. Use 30% test 70% training data.

## remove all objects
rm(list=ls())

Guizhi_Xu_HW3 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(Guizhi_Xu_HW3)

# Remove the missing Values
HW3_notmissing<-na.omit(Guizhi_Xu_HW3)

# Convert class column to Factor
HW3_notmissing$Class<-factor(HW3_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW3_notmissing$Class)
is.factor(HW3_notmissing$Class)

# Use 30% test 70% training data.
idx<-sort(sample(nrow(HW3_notmissing),as.integer(.70*nrow(HW3_notmissing))))
idx
training<-HW3_notmissing[idx,-1]
nrow(training)
test<-HW3_notmissing[-idx,-1]
nrow(test)

# Use the library("kknn") 
library(kknn)

# When the value of k = 3
predict_k3<-kknn(formula = Class~., training, test, k=3, kernel = "triangular")
fit <- fitted(predict_k3)
n3 <- table(Actual=test$Class,Fitted=fit)
n3

# Accuracy_k = 3
accuracy_k3 <- {sum(diag(n3)/(sum(rowSums(n3)))) * 100}
accuracy_k3

# When the value of k = 5
predict_k5<-kknn(formula = Class~., training, test, k=5, kernel = "triangular")
fit <- fitted(predict_k5)
n5 <- table(Actual=test$Class,Fitted=fit)
n5

# Accuracy_k = 5
accuracy_k5 <- {sum(diag(n5)/(sum(rowSums(n5)))) * 100}
accuracy_k5

# When the value of k = 10
predict_k10<-kknn(formula = Class~., training, test, k=10, kernel = "triangular")
fit <- fitted(predict_k10)
n10 <- table(Actual=test$Class,Fitted=fit)
n10

# Accuracy_k = 10
accuracy_k10 <- {sum(diag(n10)/(sum(rowSums(n10)))) * 100}
accuracy_k10
