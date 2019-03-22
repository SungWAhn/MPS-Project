library("data.table")
train.final<-fread('training_final.csv', header=T, integer64='character')
valid.final<-fread('validation_final.csv',header=T, integer64='character')
valid.final<-as.data.frame(valid.final)

#fit NB model
library(e1071)
library(mlbench)
fit1 <- naiveBayes(as.factor(group)~., data=train.final)
pred1 <- predict(fit1, newdata=valid.final, type='raw')
summary(pred1)

#confusion matrix
table(pred, valid.new$group)
#misclarification rate
mean(pred!=valid.new$group)
#log-loss 
n=dim(pred1)[1]
ngroup=dim(pred1)[2]
log=0
for (i in 1:n){
  for (j in 1:ngroup){
    log=log+log(pred1[[i,j]])
  }
}
logloss=-log/n
