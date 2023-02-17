#Guizhi Xu
#CWID:20008770

rm(list=ls())

#Import data to r
BreastCancerWisconsinData <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
BreastCancerWisconsinData

#I. Summarizing each column (e.g. min, max, mean )
summary(BreastCancerWisconsinData)

File1 <- data.frame(BreastCancerWisconsinData)
File1

is.na(BreastCancerWisconsinData)

#II. Identifying missing values
#From summary, missing values (NA) exist only in column F6
NAdata<-File1[which(is.na(BreastCancerWisconsinData[7])),]
View(NAdata)
print('Total number of missing values:')
nrow(NAdata)


#III. Replacing the missing values with the “mean” of the column.

WithoutNA <-BreastCancerWisconsinData[-which(is.na(BreastCancerWisconsinData['F6'])),]
WithoutNA
meanwithNA <-mean(BreastCancerWisconsinData$F6, na.rm = TRUE)
BreastCancerWisconsinData[is.na(BreastCancerWisconsinData)] <-meanwithNA
print(BreastCancerWisconsinData)


#IV. Displaying the frequency table of “Class” vs. F6
install.packages('plyr')
library(plyr)
freqtable<-ddply(BreastCancerWisconsinData,.(BreastCancerWisconsinData$Class, BreastCancerWisconsinData$F6), nrow)
names(freqtable)<-c("Class", "F6", "Frequency") 
# names of the columns of the frequency table
View(freqtable)
print(freqtable)

#V. Displaying the scatter plot of F1 to F6, one pair at a time
scatterplot<-plot(BreastCancerWisconsinData[2:5], main="Scatterplot for Breast Cancer Wisconsin Data", pch=10, col = "purple")

#VI. Show histogram box plot for columns F7 to F9
#histogram box plot
boxplot(BreastCancerWisconsinData$F7~BreastCancerWisconsinData$F9,main='Histogram Box Plot for F7-F9', xlab='X-axis', ylab='Y-axis', col = "pink",border = "green")

# Histogram plot
hist(BreastCancerWisconsinData$F7,main='Histogram Plot for F7', xlab='X-axis', ylab='Y-axis', col = "pink",border = "green")
hist(BreastCancerWisconsinData$F8,main='Histogram Plot for F8', xlab='X-axis', ylab='Y-axis', col = "pink",border = "green")
hist(BreastCancerWisconsinData$F9,main='Histogram Plot for F9', xlab='X-axis', ylab='Y-axis', col = "pink",border = "green")


#2. Delete all the objects from your R- environment. Reload the “breast-cancer-wisconsin.data.csv” 
#from canvas into R. Remove any row with a missing value in any of the columns.

rm("BreastCancerWisconsinData")

BreastCancerWisconsinData_Reload <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
BreastCancerWisconsinData_Reload
BreastCancerWisconsinData_omit <-na.omit(BreastCancerWisconsinData_Reload)
BreastCancerWisconsinData_omit

