library("data.table")
train.final<-fread('training_final.csv', header=T, integer64='character')
valid.final<-fread('validation_final.csv',header=T, integer64='character')
valid.final<-as.data.frame(valid.final)
train.final<-as.data.frame(train.final)
colnames(train.final)[2] <- 'count.app'
colnames(train.final)[4] <- 'May01'
colnames(train.final)[5] <- 'Apr30'
colnames(train.final)[6] <- 'May02'
colnames(train.final)[7] <- 'May03'
colnames(train.final)[8] <- 'May04'
colnames(train.final)[9] <- 'May05'
colnames(train.final)[10] <- 'May06'
colnames(train.final)[11] <- 'May07'


colnames(valid.final)[2] <- 'count.app'
colnames(valid.final)[4] <- 'May01'
colnames(valid.final)[5] <- 'Apr30'
colnames(valid.final)[6] <- 'May02'
colnames(valid.final)[7] <- 'May03'
colnames(valid.final)[8] <- 'May04'
colnames(valid.final)[9] <- 'May05'
colnames(valid.final)[10] <- 'May06'
colnames(valid.final)[11] <- 'May07'


#fit NB model
library(e1071)
library(mlbench)
valid.final$group <- as.factor(valid.final$group)
fit1 <- naiveBayes(as.factor(group)~., data=train.final)
pred1 <- predict(fit1, newdata=valid.final, type='class')
summary(pred1)
table(pred1, valid.final$group)
#confusion matrix
table(pred1, valid.new$group)
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

par(mfrow=c(5,2), mar=c(1,1,1,1))
for (i in 10:20) {
  plot(fit1$tables[[i]][,1], type='b')
}

f23meansd <- matrix(rep(0,42*3),ncol=3)
names = names(train.final)
names = names[-18]
for (i in 2:43){
  temp = fit1$tables[[i]][1,]
  f23meansd[i-1,1] = names[i]
  f23meansd[i-1,2] = temp[1]
  f23meansd[i-1,3] = temp[2]
}


f23meansd <- fit1$tables$count.app[1,]

obs<-valid.final$group
mis<-valid.final[which(pred=='F23-' & pred != obs),]
f23<-valid.final[which(valid.final$group=='F23-'),]
mis<-cbind(mis, rep('0', 3158))
colnames(mis)[45]<-'classifier'
f23<-cbind(f23, rep('1',285))
colnames(f23)[45]<-'classifier'
combined<-rbind(mis, f23)
library(ggplot2)
ggplot(data=combined, aes(x=count.app, fill=classifier, color=classifier))+
  geom_histogram(stat= 'count', alpha=0.5, position='identity')
