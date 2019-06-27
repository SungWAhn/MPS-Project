library("data.table")


train.final<-fread('training_final.csv', header=T, integer64='character')
valid.final<-fread('validation_final.csv',header=T, integer64='character')
#match two datasets
valid.final<-valid.final[,-4]
train.final<-train.final[, -11]
library(e1071)
library(mlbench)

#features: n_distinct(app_id), count_event, weekday, group
train1<-cbind(train.final[,2], train.final[,3], train.final[,11], train.final[,17])
valid1<-cbind(valid.final[,2], valid.final[,3], valid.final[,11], valid.final[,17])
fit1<-naiveBayes(as.factor(group)~., data=train1)
valid1$group<-as.factor(valid1$group)
pred1<-predict(fit1, valid1)
summary(pred1)
# F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
#1566      0      0      0      0      0   2136    283      9      0    428    231 
mean(pred1!=valid1$group)
#[1] 0.9024285

#features: n_distinct(app_id), count_event, weekday, group, 2016-04-30--2016-05-07
train2<-cbind(train1, train.final[, c(seq(4, 10))])
valid2<-cbind(valid1, valid.final[, c(seq(4, 10))])
fit2<-naiveBayes(as.factor(group)~., data=train2)
valid2$group<-as.factor(valid2$group)
pred2<-predict(fit2, valid2)
summary(pred2)
#F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
#3813      0      0     35     44     74    213    105     20     46     63    240  
mean(pred2!=valid2$group)
# 0.9204814

#features: n_distinct(app_id), count_event, weekday, group, count.4_8, count.8_12, count.12_16, count.16-20, count.20-24
train3<-cbind(train1, train.final[, c(seq(12, 16))])
valid3<-cbind(valid1, valid.final[, c(seq(12, 16))])
fit3<-naiveBayes(as.factor(group)~., data=train3)
valid3$group<-as.factor(valid3$group)
pred3<-predict(fit3, valid3)
summary(pred3)
#F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
#78      0      2     45      8     45   3874    168     23      8    190    212 
mean(pred3!=valid3$group)
#0.9065119

#features: n_distinct(app_id), count_event, weekday, group, count.pc
train4<-cbind(train1, train.final[, c(seq(20, 28))])
valid4<-cbind(valid1, valid.final[, c(seq(20, 28))])
fit4<-naiveBayes(as.factor(group)~., data=train4)
valid4$group<-as.factor(valid4$group)
pred4<-predict(fit4, valid4)
summary(pred4)
#F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
# 118      2   3561      1    195    246      2     12     48     79     17    372  
mean(pred4!=valid4$group)
#0.9314421

#features:  n_distinct(app_id), count_event, weekday, group, ratio.pc
train5<-cbind(train1, train.final[, c(seq(29, 41))])
valid5<-cbind(valid1, valid.final[, c(seq(29, 41))])
fit5<-naiveBayes(as.factor(group)~., data=train5)
valid5$group<-as.factor(valid5$group)
pred5<-predict(fit5, valid5)
summary(pred5)
# F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
# 3189      5    618     30     14    192     69    141     44     39    207    105 
mean(pred5!=valid5$group)
# 0.9228455

#features: n_distinct(app_id), count_event, weekday, group, longitude, latitude
train6<-cbind(train1, train.final[, c(42,43)])
valid6<-cbind(valid1, valid.final[, c(42,43)])
fit6<-naiveBayes(as.factor(group)~., data=train6)
valid6$group<-as.factor(valid6$group)
pred6<-predict(fit6, valid6)
summary(pred6)
#F23- F24-26 F27-28 F29-32 F33-42   F43+   M22- M23-26 M27-28 M29-31 M32-38   M39+ 
#2770      0    154      0     40      0    437    233      9      0    839    171 
mean(pred6!=valid6$group)
# 0.8934021