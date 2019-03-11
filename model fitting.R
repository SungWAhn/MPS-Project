
# NB 
library(e1071)
library(mlbench)
training<-read.csv('training.csv', header = TRUE)
validation<-read.csv('validation.csv', header=TRUE)
fit1 <- naiveBayes(group~., data=training)
pred1 <- predict(fit1, validation, type='raw')
summary(pred1)
#check accuracy
#table(pred1, validation$group)

#SVM
library(kernlab)
library(mlbench)
fit2 <- ksvm(group~., data=training, kernel="rbfdot")
pred2 <- predict(fit2, validation, type="response")

#CART
library(rpart)
fit3 <- rpart(group~., data=training)