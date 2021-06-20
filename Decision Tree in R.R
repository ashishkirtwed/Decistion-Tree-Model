#Decision Tree Classfication
#Machine Learning From A-z kirill eremenko
#install.packages("tidyverse")
#install.packages('caTools')
library("tidyverse")
library(caTools)

#Reading the dataset
dataset = read.csv("C:/Users/Ashish/Desktop/Decision Tree/Social_Network_Ads.csv")
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
#splitting the dataset
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 3/4)
training_set=subset(dataset,split==TRUE)
test_set = subset(dataset,split==FALSE)

#Feature Scaling
#https://datatricks.co.uk/feature-scaling-in-r-five-simple-methods
training_set[,1:2] =scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

# Traing the Decision Tree Machine Learning Model
# https://www.datacamp.com/community/tutorials/decision-trees-R?utm_source=adwords_ppc&utm_campaignid=1455363063&utm_adgroupid=65083631748&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=332602034358&utm_targetid=dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9300710&gclid=CjwKCAjwq7aGBhADEiwA6uGZp9PkhsZ9zljaP5Wbpv_QI05LXWeujUs_MI6YbqIBbo3WWBlUrHsWmhoCj54QAvD_BwE
#install.packages('ISLR')
library(rpart)
classifier = rpart(formula = Purchased~.,
                   data = training_set,method="class")

#predicting the new test result
y_pred = predict(classifier,newdata=test_set[-3],type='class')
length(y_pred)

#Making the confusion matrix
cm=table(test_set$Purchased,y_pred)

#Accuracy score
#install.packages('caret')
#install.packages('e1071')
library(caret)
library(e1071)
cm=confusionMatrix(test_set$Purchased,y_pred) #Confusion Matrix method 
#generate more descriptive statistical information
print(cm)

#Visualizing the Training set results
library('ElemStatLearn')
set = training_set
#Generating x1 and x2 sequence for min()-max() and x2 sequence
# min()-max()
x1 = seq(min(set[,1])-1,max(set[,1])+1,by = 0.01)
x2 = seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
#Now making a grid matrix
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier,newdata=grid_set,type="class")
#https://www.datamentor.io/r-programming/plot-function/
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#Visualizing the Test set results
library('ElemStatLearn')
set = training_set
#Generating x1 and x2 sequence for min()-max() and x2 sequence
# min()-max()
x1 = seq(min(set[,1])-1,max(set[,1])+1,by = 0.01)
x2 = seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
#Now making a grid matrix
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier,newdata=grid_set,type="class")
#https://www.datamentor.io/r-programming/plot-function/
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


