strokedata<-read.csv("D:/Data_Mining/StrokeData.csv", header=TRUE, sep=",")
strokedata
str(strokedata)

#missing value find out:
colSums(is.na(strokedata))

#outlier detect:
which(is.na(strokedata$gender))
which(is.na(strokedata$ever_married))
which(is.na(strokedata$work_type))
which(is.na(strokedata$Residence_type))
which(is.na(strokedata$smoking_status))
which(is.na(strokedata$id))
which(is.na(strokedata$age))
which(is.na(strokedata$hypertension))
which(is.na(strokedata$avg_glucose_level))
which(is.na(strokedata$heart_disease))
which(is.na(strokedata$stroke))


#Annotate Gender Male as 1 , Female as 2, Other as 3:
strokedata$gender<-
  factor(strokedata$gender,
         levels = c("Male", "Female"),
         labels = c(1,2))
strokedata$gender

#Annotate Ever_Married yes as 1, no as 2:
strokedata$ever_married<-
  factor(strokedata$ever_married,
         levels = c("Yes","No"),
         labels = c(1,2))
strokedata$ever_married

#Annotate Work_Type Private as 1, Self-employed as 2, Govt_job as 3, 
#children as 4, Never_worked as 5:
strokedata$work_type<-
  factor(strokedata$work_type,
         levels = c("Private","Self-employed","Govt_job", 
                    "children", "Never_worked"),
         labels = c(1,2,3,4,5))
strokedata$work_type

#Annotate Residence_Type Urban as 1, Rural as 2:
strokedata$Residence_type<-
  factor(strokedata$Residence_type,
         levels = c("Urban","Rural"),
         labels = c(1,2))
strokedata$Residence_type

#Annotate smoking_status 'formerly smoked' as 1, 'never smoked' as 2, 'Unknown' as 3, 'smokes' as 4:
strokedata$smoking_status<-
  factor(strokedata$smoking_status,
         levels = c("formerly smoked","never smoked", "Unknown", "smokes"),
         labels = c(1,2,3,4))
strokedata$smoking_status

str(strokedata)

# Delete the rows with missing values:
remove<-na.omit(strokedata)
strokedata

#Normalize the function:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
strokedata1<- as.data.frame(lapply(strokedata[6:11], normalize))
strokedata1

summary(strokedata1)

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

# Splitting data into train and test data
split <- sample.split(strokedata1, SplitRatio = 0.7)
train_cl <- subset(strokedata1, split == "TRUE")
test_cl <- subset(strokedata1, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])


#Fitting KNN Model to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$stroke,
                      k = 1)
classifier_knn

## Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$stroke)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$stroke,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$stroke)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$stroke,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$stroke)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$stroke,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$stroke)
print(paste('Accuracy =', 1-misClassError))

install.packages("gmodels")
library(gmodels)
CrossTable(x=test_cl$stroke , y=classifier_knn, prop.chisq = FALSE)

install.packages('caret')
library(caret)
confusionMatrix(table(classifier_knn ,test_cl$stroke))

#Euclidean distance
euclidean_distance = function(age, avg_glucose_level){
  #  We check that they have the same number of observation
  if(length(age) == length(avg_glucose_level)){
    sqrt(sum((age-avg_glucose_level)^2))  
  } else{
    stop('Vectors must be of the same length')
  }
}
euclidean_distance(1:2556, 2556:5111)

#Manhattan Distance
manhattan_distance = function(age, avg_glucose_level){
  #  We check that they have the same number of observation
  if(length(age) == length(avg_glucose_level)){
    sum(abs(age-avg_glucose_level))
  } else{
    stop('Vectors must be of the same length')
  }
}
manhattan_distance(1:2556, 2556:5111)

