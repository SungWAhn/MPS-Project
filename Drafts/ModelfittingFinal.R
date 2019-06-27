library(knitr)
library(data.table)
library(bit64)
library(Rmpfr)
library(tidyverse)
library(kableExtra)
library(VIM)
library(DT)
library(e1071)
library(klaR)
library(mlbench)
library(corrplot)
library(bnlearn)
library(gridExtra)
library(plotly)
library(MLmetrics)
library(nnet)
library(randomForest)
library(caret)

## Data Merge and Cleaning ## -----

# Data Import -----

# Plain read.csv can take a while. fread from data.table library is much more efficient
events <- fread(file = 'events.csv', header = T, integer64 = "character")
app_events <- fread('app_events.csv', header = T, integer64 = "character")
app_labels <- fread('app_labels.csv', header = T, integer64 = "character")
labelcategory <- fread('label_standardized categories.csv', header = T, integer64 = "character")
gender_age <- fread('gender_age_train.csv', header = T, integer64 = "character")

# Use read_csv here because of the Chinese characters issue
phonebrand <- read_csv('phone_brand_device_model.csv', locale = locale(encoding = 'UTF-8'))
# Manually convert device_id to character type
phonebrand$device_id <- as.character(phonebrand$device_id)

## Data Cleaning -----

### Translate Chinese characters in the phone brand and model columns

# Read in the translation dataset
chin_eng_brand <- read_excel("phone_brand-translate.xlsx", sheet = "Sheet1")
chin_eng_model <- read_excel("device_model translate.xlsx", sheet="Sheet1")

kable(chin_eng_brand[1:10,], caption = 'Preview of Phone Brand Translation Key') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(chin_eng_model[1:10,], caption = 'Preview of Phone Model Translation Key') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)


## Model Fitting ## -----

# Datasets -----
train.final <- fread('training_final.csv', header=T, integer64='character')
valid.final <- fread('validation_final.csv',header=T, integer64='character')
train.NoNANoM <- fread('training_NoNA_NoMultc.csv', header = T, integer64 = 'character')
valid.NoNANoM <- fread('validation_NoNA_NoMultc.csv', header = T, integer64 = 'character')

traintotal <- cbind(train.NoNANoM[,-c(1,3,4,66,67)], train.final[,c(3,4)])
validtotal <- cbind(valid.NoNANoM[,-c(1,3,4,66,67)], valid.final[,c(3,4)])
traintotal$group <- as.factor(traintotal$group)
validtotal$group <- as.factor(validtotal$group)


# Naive Bayes: All Features -----
fitnball <- naiveBayes(group~., data=traintotal)
prednb <- predict(fitnball, validtotal)
prednbprobs <- predict(fitnball, validtotal, type='raw')

# Misclassification rate
misclassnb <- mean(prednb!=validtotal$group)
#log-loss
loglossnb <- MultiLogLoss(y_pred = prednbprobs, y_true = validtotal$group)
loglossnb
misclassnb

# Performance
kable(data.frame('Misclassification'=c("91.77%"), 'logloss' = loglossnb), caption = 'Naive Bayes Performance: All features') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Confusion Matrix
confusionnball <- as.data.frame(prop.table(table(prednb, validtotal$group),2))
names(confusionnball)[3] <- "Prop"
pnball <- ggplot(confusionnball, aes(Var2, prednb)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='All Features')
pnball

# Naive Bayes: Independence Assumption -----
# Correlation Plot
corrM <- cor(traintotal[,-1])
corrplot(corrM, type = 'lower', tl.col = 'black', tl.srt = 45)

# Remove highly correlated features
highcorr <- findCorrelation(corrM,cutoff=.5,exact = T,names = T)
highcorr

# New training set without the correlated features
traintotaluncorr <- select(traintotal, -c(highcorr[1:length(highcorr)]))
validtotaluncorr <- select(validtotal, -c(highcorr[1:length(highcorr)]))
traintotaluncorr$group <- as.factor(traintotaluncorr$group)
validtotaluncorr$group <- as.factor(validtotaluncorr$group)

# Fit new Naive Bayes Model
fitnb <- naiveBayes(group~., data=traintotaluncorr)
prednb <- predict(fitnb, validtotaluncorr)
prednbprobs <- predict(fitnb, validtotaluncorr, type = 'raw')

# Misclassification rate
misclassnb <- mean(prednb!=validtotaluncorr$group)
# log-loss
loglossnb <- MultiLogLoss(y_pred = prednbprobs, y_true = validtotaluncorr$group)
loglossnb
misclassnb

# Performance
kable(data.frame('Misclassification'=c('92.48%'), 'logloss' = c(9.842902)), caption = 'Naive Bayes Performance: Without Correlated Predictors') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Confusion Matrix
confusionnbred <- as.data.frame(prop.table(table(prednb, validtotaluncorr$group),2))
names(confusionnbred)[3] <- "Prop"
pnbred <- ggplot(confusionnbred, aes(Var2, prednb)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Without Correlated Predictors')
pnbred

  # Naive Bayes: Normality Assumption Charts -----
ggplot(data = traintotal, aes(x = count_event)) +
  geom_histogram(binwidth = 10) +
  xlim(c(0,250)) +
  ylim(c(0,4500)) +
  ggtitle("Event Count")+
  theme(plot.title = element_text(size=12))

ggplot(data = traintotal, aes(x = count.app)) +
  geom_histogram(binwidth = 10) +
    xlim(c(0,250)) +
  ggtitle("App Count")+
  theme(plot.title = element_text(size=12))

ggplot(data = train.NoNANoM, aes(x = photography.appCt)) +
  geom_histogram(binwidth = 10) +
  xlim(c(0,250)) +
  ylim(c(0,2500))+
  ggtitle("Photography App Usage Count")+
  theme(plot.title = element_text(size=12))

ggplot(data = train.NoNANoM, aes(x = health.appCt)) +
  geom_histogram(binwidth = 10) +
  xlim(c(0,250)) +
  ylim(c(0,2500))+
  ggtitle("Health App Usage Count")+
  theme(plot.title = element_text(size=12))

# Plot the normal distributions Naive bayes' generates
# Look at the mean and sd of the gaussian distribution it is using
distrs <- cbind(fitnball$tables$count.app, fitnball$tables$count_event, fitnball$tables$weekday)
colnames(distrs)<-c("count.appmean", "count.appsd", "count_eventmean", "count_eventsd", "weekdaymean", "weekdaysd")

# Now plot the Normal distributions
# Distribution of the App Count
caseqs <- matrix(nrow=100,ncol=12)
ecseqs <- matrix(nrow=100,ncol=12)
weseqs <- matrix(nrow=100,ncol=12)
cadists <- matrix(nrow=100,ncol=12)
ecdists <- matrix(nrow=100,ncol=12)
wedists <- matrix(nrow=100,ncol=12)

for (i in 1:12) {
  caseqs[,i]<-seq(distrs[i,1]-4*distrs[i,2],distrs[i,1]+4*distrs[i,2], length=100)
  cadists[,i]<-dnorm(caseqs[,i],mean = distrs[i,1], sd=distrs[i,2])
  
  ecseqs[,i]<-seq(distrs[i,3]-4*distrs[i,4],distrs[i,3]+4*distrs[i,4], length=100)
  ecdists[,i]<-dnorm(ecseqs[,i],mean = distrs[i,3], sd=distrs[i,4])
  
  weseqs[,i]<-seq(distrs[i,5]-4*distrs[i,6],distrs[i,5]+4*distrs[i,6], length=100)
  wedists[,i]<-dnorm(weseqs[,i],mean = distrs[i,5], sd=distrs[i,6])
}

caseqs<-c(caseqs[,1], caseqs[,2], caseqs[,3], caseqs[,4], caseqs[,5], caseqs[,6], caseqs[,7], caseqs[,8], caseqs[,9], caseqs[,10], caseqs[,11], caseqs[,12])
caseqs<- data.frame('x'=caseqs, 'group'=c(rep(fitnball$levels[1], 100), rep(fitnball$levels[2], 100), rep(fitnball$levels[3], 100), rep(fitnball$levels[4], 100), rep(fitnball$levels[5], 100), rep(fitnball$levels[6], 100), rep(fitnball$levels[7], 100), rep(fitnball$levels[8], 100), rep(fitnball$levels[9], 100), rep(fitnball$levels[10], 100), rep(fitnball$levels[11], 100), rep(fitnball$levels[12], 100)))
caseqs$y <- c(cadists[,1], cadists[,2], cadists[,3], cadists[,4], cadists[,5], cadists[,6], cadists[,7], cadists[,8], cadists[,9], cadists[,10], cadists[,11], cadists[,12])

ecseqs<-c(ecseqs[,1], ecseqs[,2], ecseqs[,3], ecseqs[,4], ecseqs[,5], ecseqs[,6], ecseqs[,7], ecseqs[,8], ecseqs[,9], ecseqs[,10], ecseqs[,11], ecseqs[,12])
ecseqs<- data.frame('x'=ecseqs, 'group'=c(rep(fitnball$levels[1], 100), rep(fitnball$levels[2], 100), rep(fitnball$levels[3], 100), rep(fitnball$levels[4], 100), rep(fitnball$levels[5], 100), rep(fitnball$levels[6], 100), rep(fitnball$levels[7], 100), rep(fitnball$levels[8], 100), rep(fitnball$levels[9], 100), rep(fitnball$levels[10], 100), rep(fitnball$levels[11], 100), rep(fitnball$levels[12], 100)))
ecseqs$y <- c(ecdists[,1], ecdists[,2], ecdists[,3], ecdists[,4], ecdists[,5], ecdists[,6], ecdists[,7], ecdists[,8], ecdists[,9], ecdists[,10], ecdists[,11], ecdists[,12])


weseqs<-c(weseqs[,1], weseqs[,2], weseqs[,3], weseqs[,4], weseqs[,5], weseqs[,6], weseqs[,7], weseqs[,8], weseqs[,9], weseqs[,10], weseqs[,11], weseqs[,12])
weseqs<- data.frame('x'=weseqs, 'group'=c(rep(fitnball$levels[1], 100), rep(fitnball$levels[2], 100), rep(fitnball$levels[3], 100), rep(fitnball$levels[4], 100), rep(fitnball$levels[5], 100), rep(fitnball$levels[6], 100), rep(fitnball$levels[7], 100), rep(fitnball$levels[8], 100), rep(fitnball$levels[9], 100), rep(fitnball$levels[10], 100), rep(fitnball$levels[11], 100), rep(fitnball$levels[12], 100)))
weseqs$y <- c(wedists[,1], wedists[,2], wedists[,3], wedists[,4], wedists[,5], wedists[,6], wedists[,7], wedists[,8], wedists[,9], wedists[,10], wedists[,11], wedists[,12])


ggplot(caseqs, aes(x=x,y=y, color=group)) +
  geom_point(position = 'jitter', size=1) +
  ggtitle('Normal Distribution Generated with App Count')+
  xlab('Counts')+
  ylab('Probability Density')+
  theme(plot.title = element_text(size=11),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=8))

ggplot(ecseqs, aes(x=x,y=y, color=group)) +
  geom_point(position = 'jitter', size=1) +
  ggtitle('Normal Distribution Generated with Event Count')+
  xlab('Counts')+
  ylab('Probability Density')+
  theme(plot.title = element_text(size=11),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=8))

ggplot(weseqs, aes(x=x,y=y, color=group)) +
  geom_point(position = 'jitter', size=1) +
  ggtitle('Normal Distribution Generated with Weekday Count')+
  xlab('Counts')+
  ylab('Probability Density')+
  theme(plot.title = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=8))


# Bin the features -----
# Obtain the app count features we want to bin
trainingctEvent <- as.data.frame(train.NoNANoM[,5])
# Stack the app count features into one long vector
counts <- stack(trainingctEvent)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingctEvent[trainingctEvent==0] <- 0
trainingctEvent[trainingctEvent>0 & trainingctEvent <= 4] <- 1
trainingctEvent[trainingctEvent>4 & trainingctEvent <= 15] <- 2
trainingctEvent[trainingctEvent>15 & trainingctEvent <= 51] <- 3
trainingctEvent[trainingctEvent>51] <- 4


# Convert the categorical into factor variables
trainingctEvent$count_event<- as.factor(trainingctEvent$count_event) 
# The levels are not consistent across features: make them all 5
trainingctEvent$count_event <- factor(trainingctEvent$count_event, levels = c("0","1","2","3","4"))
names(trainingctEvent) <- paste0(names(trainingctEvent),'cat')

# Do the same thing as above to the validation dataset
validationctEvent <- as.data.frame(valid.NoNANoM[,5])

validationctEvent[validationctEvent==0] <- 0
validationctEvent[validationctEvent>0 & validationctEvent <= 4] <- 1
validationctEvent[validationctEvent>4 & validationctEvent <= 15] <- 2
validationctEvent[validationctEvent>15 & validationctEvent <= 51] <- 3
validationctEvent[validationctEvent>51] <- 4

validationctEvent$count_event<- as.factor(validationctEvent$count_event)
# The levels are not consistent across features: make them all 5
validationctEvent$count_event <- factor(validationctEvent$count_event, levels = c("0","1","2","3","4"))
names(validationctEvent) <- paste0(names(validationctEvent),'cat')


# Obtain the app count features we want to bin
trainingctApp <- as.data.frame(train.NoNANoM[,6])
# Stack the app count features into one long vector
counts <- stack(trainingctApp)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingctApp[trainingctApp==0] <- 0
trainingctApp[trainingctApp>0 & trainingctApp <= 24] <- 1
trainingctApp[trainingctApp>24 & trainingctApp <= 35] <- 2
trainingctApp[trainingctApp>35 & trainingctApp <= 50] <- 3
trainingctApp[trainingctApp>50] <- 4


# Convert the categorical into factor variables
trainingctApp$count.app<- as.factor(trainingctApp$count.app) 
# The levels are not consistent across features: make them all 5
trainingctApp$count.app <- factor(trainingctApp$count.app, levels = c("0","1","2","3"))
names(trainingctApp) <- paste0(names(trainingctApp),'cat')

# Do the same thing as above to the validation dataset
validationctApp <- as.data.frame(valid.NoNANoM[,6])



validationctApp[validationctApp==0] <- 0
validationctApp[validationctApp>0 & validationctApp <= 24] <- 1
validationctApp[validationctApp>24 & validationctApp <= 35] <- 2
validationctApp[validationctApp>35 & validationctApp <= 50] <- 3
validationctApp[validationctApp>50] <- 4


validationctApp$count.app<- as.factor(validationctApp$count.app) 
# The levels are not consistent across features: make them all 5
validationctApp$count.app <- factor(validationctApp$count.app, levels = c("0","1","2","3"))
names(validationctApp) <- paste0(names(validationctApp),'cat')




### App Counts

# Obtain the app count features we want to bin
trainingAppct <- as.data.frame(train.NoNANoM[,7:29])
# Stack the app count features into one long vector
counts <- stack(trainingAppct)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingAppct[trainingAppct==0] <- 0
trainingAppct[trainingAppct>0 & trainingAppct <= 5] <- 1
trainingAppct[trainingAppct>5 & trainingAppct <= 35] <- 2
trainingAppct[trainingAppct>35] <- 3


# Convert the categorical into factor variables
for (i in 1:23) {
  trainingAppct[,i] <- as.factor(trainingAppct[,i]) 
}
for (i in 1:23) {
  names(trainingAppct)[i] <- paste0(names(trainingAppct)[i],'cat')
}

# The levels are not consistent across features: make them all 5
for (i in 1:23) {
  trainingAppct[,i] <- factor(trainingAppct[,i], levels = c("0","1","2","3"))
}

# Do the same thing as above to the validation dataset
validationAppct <- as.data.frame(valid.NoNANoM[,7:29])



validationAppct[validationAppct==0] <- 0
validationAppct[validationAppct>0 & validationAppct <= 5] <- 1
validationAppct[validationAppct>5 & validationAppct <= 35] <- 2
validationAppct[validationAppct>35] <- 3


for (i in 1:23) {
  validationAppct[,i] <- as.factor(validationAppct[,i]) 
}
for (i in 1:23) {
  names(validationAppct)[i] <- paste0(names(validationAppct)[i],'cat')
}

for (i in 1:23) {
  validationAppct[,i] <- factor(validationAppct[,i], levels = c("0","1","2","3"))
}


### Is_active Ratio

# Obtain the app count features we want to bin
trainingIsactive <- as.data.frame(train.NoNANoM[,30:51])
# Stack the app count features into one long vector
counts <- stack(trainingIsactive)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingIsactive[trainingIsactive==0] <- 0
trainingIsactive[trainingIsactive>0 & trainingIsactive <= 3] <- 1
trainingIsactive[trainingIsactive>3] <- 2

# Convert the categorical into factor variables
for (i in 1:22) {
  trainingIsactive[,i] <- as.factor(trainingIsactive[,i]) 
}
for (i in 1:22) {
  names(trainingIsactive)[i] <- paste0(names(trainingIsactive)[i],'cat')
}

# The levels are not consistent across features: make them all 5
for (i in 1:22) {
  trainingIsactive[,i] <- factor(trainingIsactive[,i], levels = c("0","1","2"))
}

# Do the same thing as above to the validation dataset
validIsactive <- as.data.frame(valid.NoNANoM[,30:51])



validIsactive[validIsactive==0] <- 0
validIsactive[validIsactive>0 & validIsactive <= 3] <- 1
validIsactive[validIsactive>3] <- 2

for (i in 1:22) {
  validIsactive[,i] <- as.factor(validIsactive[,i]) 
}
for (i in 1:22) {
  names(validIsactive)[i] <- paste0(names(validIsactive)[i],'cat')
}

for (i in 1:22) {
  validIsactive[,i] <- factor(validIsactive[,i], levels = c("0","1","2"))
}



### Weekday

# Obtain the app count features we want to bin
trainingWeekday <- as.data.frame(train.NoNANoM[,60])
# Stack the app count features into one long vector
counts <- stack(trainingWeekday)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingWeekday[trainingWeekday==0] <- 0
trainingWeekday[trainingWeekday>0 & trainingWeekday <= 3] <- 1
trainingWeekday[trainingWeekday>3 & trainingWeekday <= 10] <- 2
trainingWeekday[trainingWeekday>10 & trainingWeekday <= 37] <- 3
trainingWeekday[trainingWeekday>37] <- 4


for (i in 1) {
  trainingWeekday[,i] <- as.factor(trainingWeekday[,i]) 
}
for (i in 1) {
  names(trainingWeekday)[i] <- paste0(names(trainingWeekday)[i],'cat')
}

# The levels are not consistent across features: make them all 5
for (i in 1) {
  trainingWeekday[,i] <- factor(trainingWeekday[,i], levels = c("0","1","2","3","4"))
}

# Do the same thing as above to the validation dataset
validWeekday <- as.data.frame(valid.NoNANoM[,60])



validWeekday[validWeekday==0] <- 0
validWeekday[validWeekday>0 & validWeekday <= 3] <- 1
validWeekday[validWeekday>3 & validWeekday <= 10] <- 2
validWeekday[validWeekday>10 & validWeekday <= 37] <- 3
validWeekday[validWeekday>37] <- 4


for (i in 1) {
  validWeekday[,i] <- as.factor(validWeekday[,i]) 
}
for (i in 1) {
  names(validWeekday)[i] <- paste0(names(validWeekday)[i],'cat')
}

for (i in 1) {
  validWeekday[,i] <- factor(validWeekday[,i], levels = c("0","1","2","3","4"))
}


### Hourly Counts

trainCount48<-data.matrix(train.NoNANoM[, 'count.4_8'])
trainCount812<-data.matrix(train.NoNANoM[, 'count.8_12'])
trainCount1216<-data.matrix(train.NoNANoM[, 'count.12_16'])
trainCount1620<-data.matrix(train.NoNANoM[, 'count.16_20'])
trainCount2024<-data.matrix(train.NoNANoM[, 'count.20_24'])
hourcount<-rbind(trainCount48, trainCount812, trainCount1216, trainCount1620, trainCount2024) # 0 1 2
quantile<-quantile(hourcount, c(0.25, 0.5, 0.75))
quantile

trainCount48[trainCount48==0]<-0
trainCount48[trainCount48>0 & trainCount48<2]<-1
trainCount48[trainCount48>=2 & trainCount48<8]<-2
trainCount48[trainCount48>=8]<-3

trainCount812[trainCount812==0]<-0
trainCount812[trainCount812>0 & trainCount812<2]<-1
trainCount812[trainCount812>=2 & trainCount812<8]<-2
trainCount812[trainCount812>=8]<-3

trainCount1216[trainCount1216==0 ]<-0
trainCount1216[trainCount1216>0 & trainCount1216<2]<-1
trainCount1216[trainCount1216>=2 & trainCount1216<8]<-2
trainCount1216[trainCount1216>=8]<-3


trainCount1620[trainCount1620==0]<-0
trainCount1620[trainCount1620>0 & trainCount1620<2]<-1
trainCount1620[trainCount1620>=2 & trainCount1620<8]<-2
trainCount1620[trainCount1620>=8]<-3


trainCount2024[trainCount2024==0]<-0
trainCount2024[trainCount2024>0 & trainCount2024<2]<-1
trainCount2024[trainCount2024>=2 & trainCount2024<8]<-2
trainCount2024[trainCount2024>=8]<-3

traincount <- cbind(trainCount48,trainCount812,trainCount1216,trainCount1620,trainCount2024)
traincount <-as.data.frame(traincount)
for (i in 1:5) {
  traincount[,i] <- as.factor(traincount[,i])
  names(traincount)[i] <- paste0(names(traincount)[i],'cat')
  # The levels are not consistent across features: make them all 4
  traincount[,i] <- factor(traincount[,i], levels = c("0","1","2","3"))
}

validCount48<-data.matrix(valid.NoNANoM[, 'count.4_8'])
validCount812<-data.matrix(valid.NoNANoM[, 'count.8_12'])
validCount1216<-data.matrix(valid.NoNANoM[, 'count.12_16'])
validCount1620<-data.matrix(valid.NoNANoM[, 'count.16_20'])
validCount2024<-data.matrix(valid.NoNANoM[, 'count.20_24'])



validCount48[validCount48==0]<-0
validCount48[validCount48>0 & validCount48<2]<-1
validCount48[validCount48>=2 & validCount48<8]<-2
validCount48[validCount48>=8]<-3

validCount812[validCount812==0]<-0
validCount812[validCount812>0 & validCount812<2]<-1
validCount812[validCount812>=2 & validCount812<8]<-2
validCount812[validCount812>=8]<-3

validCount1216[validCount1216==0 ]<-0
validCount1216[validCount1216>0 & validCount1216<2]<-1
validCount1216[validCount1216>=2 & validCount1216<8]<-2
validCount1216[validCount1216>=8]<-3


validCount1620[validCount1620==0]<-0
validCount1620[validCount1620>0 & validCount1620<2]<-1
validCount1620[validCount1620>=2 & validCount1620<8]<-2
validCount1620[validCount1620>=8]<-3


validCount2024[validCount2024==0]<-0
validCount2024[validCount2024>0 & validCount2024<2]<-1
validCount2024[validCount2024>=2 & validCount2024<8]<-2
validCount2024[validCount2024>=8]<-3
validcount <- cbind(validCount48,validCount812,validCount1216,validCount1620,validCount2024)
validcount<-as.data.frame(validcount)

for (i in 1:5) {
  validcount[,i] <- as.factor(validcount[,i]) 
  names(validcount)[i] <- paste0(names(validcount)[i],'cat')
  # The levels are not consistent across features: make them all 4
  validcount[,i] <- factor(validcount[,i], levels = c("0","1","2","3"))
}



### Day Count

# Obtain the day count features we want to bin
trainingDay <- as.data.frame(train.NoNANoM[,52:59])
# Stack the day count features into one long vector
counts <- stack(trainingDay)
# Calculate the quantiles
quantiles <- quantile(counts[,1], c(.25,.5,.75))
quantiles

# Construct the categorical features based on the quantiles
trainingDay[trainingDay==0] <- 0
trainingDay[trainingDay>0 & trainingDay <= 1] <- 1
trainingDay[trainingDay>1 & trainingDay <= 5] <- 2
trainingDay[trainingDay>5] <- 3

# Convert the categorical into factor variables
for (i in 1:8) {
  trainingDay[,i] <- as.factor(trainingDay[,i]) 
  names(trainingDay)[i] <- paste0(names(trainingDay)[i],'cat')
  # The levels are not consistent across features: make them all 4
  trainingDay[,i] <- factor(trainingDay[,i], levels = c("0","1","2","3"))
}


# Do the same thing as above to the validation dataset
validationDay <- as.data.frame(valid.NoNANoM[,52:59])

validationDay[validationDay==0] <- 0
validationDay[validationDay>0 & validationDay <= 1] <- 1
validationDay[validationDay>1 & validationDay <= 5] <- 2
validationDay[validationDay>5] <- 3

for (i in 1:8) {
  validationDay[,i] <- as.factor(validationDay[,i]) 
  names(validationDay)[i] <- paste0(names(validationDay)[i],'cat')
  # The levels are not consistent across features: make them all 4
  validationDay[,i] <- factor(validationDay[,i], levels = c("0","1","2","3"))
}

# fit the model with transformed categorical features
train.daycat <- trainingDay
valid.daycat <- validationDay


# # Naive Bayes: Normality Assumption Model -----
traincateg<-cbind(trainingctApp, trainingctEvent, trainingAppct, trainingIsactive, trainingWeekday, traincount, train.daycat)
traincateg$longitude<- traintotal$longitude
traincateg$latitude <- traintotal$latitude
traincateg$group <- traintotal$group
validcateg<-cbind(validationctApp, validationctEvent, validationAppct, validIsactive, validWeekday, validcount, valid.daycat)
validcateg$longitude<- validtotal$longitude
validcateg$latitude <- validtotal$latitude
validcateg$group <- validtotal$group

highcorrcat <- paste0(highcorr,'cat')
traincateguncorr <- select(traincateg, -c(highcorrcat[1:length(highcorrcat)]))
validcateguncorr <- select(validcateg, -c(highcorrcat[1:length(highcorrcat)]))

fitnb<-naiveBayes(group~., data=traincateg)
prednb<-predict(fitnb, validcateg)
prednbprobs<-predict(fitnb, validcateg, type='raw')

# Misclassification rate
mean(prednb!=validcateg$group)
#log-loss
MultiLogLoss(y_pred = prednbprobs, y_true = validcateg$group)

performancecategall <-data.frame('Misclassification' = c("84.96%"), 'Logloss' = c(4.382397))
kable(performancecategall, caption = "Naive Bayes Performance: All Categorical Predictors") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F) %>%
  column_spec(1, width = '7em') %>%
  column_spec(2, width = "7em")

confusion <- as.data.frame(prop.table(table(prednb, validcateg$group),2))
names(confusion)[3] <-'Prop'
pcategall<-ggplot(confusion, aes(Var2, prednb)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Discretizing All Features')
pcategall


# Naive Bayes Categorical Uncorrelated
fitnb<-naiveBayes(group~., data=traincateguncorr)
prednb<-predict(fitnb, validcateguncorr)
prednbprobs<-predict(fitnb, validcateguncorr, type='raw')

# Misclassification rate
mean(prednb!=validcateguncorr$group)
#log-loss
MultiLogLoss(y_pred = prednbprobs, y_true = validcateguncorr$group)

performancecategred <-data.frame('Misclassification' = c("84.01%"), 'Logloss' = c(2.480219))
kable(performancecategred, caption = "Naive Bayes Performance: Reduced Categorical Predictors") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F) %>%
  column_spec(1, width = '8em') %>%
  column_spec(2, width = "8em")

confusion <- as.data.frame(prop.table(table(prednb, validcateguncorr$group),2))
names(confusion)[3] <-'Prop'
pcategred<-ggplot(confusion, aes(Var2, prednb)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Discretizing Reduced Features')
pcategred

performanceuncorr <-data.frame('Improvements'='Remove correlated features','Misclassification'=c('92.48%'), 'Logloss' = c(9.842902))
performancecategred <-data.frame('Improvements'='Discretize','Misclassification' = c("84.01%"), 'Logloss' = c(2.480219))
summar<-rbind(performanceuncorr,performancecategred)
kable(summar, caption = "Naive Bayes Performance: Summary") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F)

# Naive Bayes: Flexible Distribution -----
fitnb<-NaiveBayes(group~., data=traintotaluncorr, usekernel=TRUE)
prednb<-predict(fitnb, validtotaluncorr, threshold = .0001)

mean(prednb$class!=validtotaluncorr$group)
#log-loss
MultiLogLoss(y_pred = prednb$posterior, y_true = validtotaluncorr$group)

performanceredflexnb <-data.frame('Misclassification' = c("90.87%"), 'Logloss' = c(7.535583))
kable(performancebase) %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F) %>%
  column_spec(1, width = '7em') %>%
  column_spec(2, width = "7em") %>% 
  footnote(general = "Naive Bayes Performance:\n Reduced Features\n & Flexible Distribution")

confusionredflexnb <- as.data.frame(prop.table(table(prednb$class, validtotal$group),2))
names(confusionredflexnb)[3] <- 'Prop'
predflexnb <-ggplot(confusionredflexnb, aes(Var2, Var1)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Reduced Features & Flexible Distribution')
predflexnb


# Naive Bayes: Class imbalance -----
set.seed(1223)
downSampledcategTrain <- downSample(x = traincateguncorr[,-23],
                                    y = traincateguncorr$group,
                                    yname = "group")
upSampledcategTrain <- upSample(x = traincateguncorr[,-23], y = traincateguncorr$group,
                                yname = 'group')

# Model for upsampled data
fitnbupcateg<-naiveBayes(group~., data=upSampledcategTrain)
prednbupcateg<-predict(fitnbupcateg, validcateguncorr)
prednbupcategprobs<-predict(fitnbupcateg, validcateguncorr, type='raw')

# Misclassification rate
mean(prednbupcateg!=validcateguncorr$group)
#log-loss
MultiLogLoss(y_pred = prednbupcategprobs, y_true = validcateguncorr$group)

# Performance
performanceup <-data.frame('Misclassification' = c("88.85%"), 'Logloss' = c(2.573699))
kable(performanceup) %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F) %>%
  column_spec(1, width = '7em') %>%
  column_spec(2, width = "7em") %>% 
  footnote(general = "Naive Bayes Performance:\n Reduced Categorical Predictors & Upsampled", footnote_as_chunk = T, title_format = c("italic", "underline"), threeparttable = T
  )
  
confusionup <- as.data.frame(prop.table(table(prednbupcateg, validcateguncorr$group),2))
names(confusionup)[3]<-'Prop'
pup<-ggplot(confusionup, aes(Var2, prednbupcateg)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Reduced Categorical Features, upsampled')
pup


fitnbdowncateg<-naiveBayes(group~., data=downSampledcategTrain)
prednbdowncateg<-predict(fitnbdowncateg, validcateguncorr)
prednbdowncategprobs<-predict(fitnbdowncateg, validcateguncorr, type='raw')

# Misclassification rate
mean(prednbdowncateg!=validcateguncorr$group)
#log-loss
MultiLogLoss(y_pred = prednbdowncategprobs, y_true = validcateguncorr$group)

# Performance
performancedown <-data.frame('Misclassification' = c("88.74%"), 'Logloss' = c(2.60324))
kable(performancedown) %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F) %>%
  column_spec(1, width = '7em') %>%
  column_spec(2, width = "7em") %>% 
  footnote(general = "Naive Bayes Performance:\n Reduced Categorical Predictors & downsampled", footnote_as_chunk = T, title_format = c("italic", "underline"), threeparttable = T
  )

confusiondown <- as.data.frame(prop.table(table(prednbdowncateg, validcateguncorr$group),2))
names(confusiondown)[3] <- 'Prop'
pdown<-ggplot(confusiondown, aes(Var2, prednbdowncateg)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Reduced Categorical Features, downsampled')
pdown

# Random Forest -----
set.seed(1223)
fit.rf <- randomForest(as.factor(group)~., data = traintotal, na.action = na.omit)

rf.pred <- predict(fit.rf, newdata=validtotal)
mean(rf.pred[is.na(rf.pred)==FALSE]!=validtotal$group[is.na(rf.pred)==FALSE])

predrfprobs <-predict(fit.rf, validtotal, type = 'prob')
MultiLogLoss(y_pred = predrfprobs, y_true = validtotal$group)

# Gini Index in order
ginis<-importance(fit.rf)
ginis<-data.frame('MeanDecreaseGini'=ginis[,1], 'Features' = rownames(ginis))
ginis<-arrange(ginis, desc(MeanDecreaseGini))

performancerfall <-data.frame('Misclassification' = c("79.60%"), 'Logloss' = c(2.264044))
kable(performancerfall, caption = "Random Forest Performance: All Features") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionrfall <- as.data.frame(prop.table(table(rf.pred, validtotal$group),2))
names(confusionrfall)[3]<-'Prop'
prfall<-ggplot(confusionrfall, aes(Var2, rf.pred)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='All Features')
prfall

# Random Forest class imbalance -----
set.seed(1223)
downSampledTrain <- downSample(x = traintotal[,-1],
                               y = traintotal$group,
                               yname = "group")
upSampledTrain <- upSample(x = traintotal[,-1], y = traintotal$group,
                           yname = 'group')

# Upsample
set.seed(1223)
fit.rfup <- randomForest(as.factor(group)~., data = upSampledTrain, na.action = na.omit)
rf.predup <- predict(fit.rfup, newdata=validtotal)
predrfupprobs <-predict(fit.rfup, validtotal, type = 'prob')

mean(rf.predup[is.na(rf.predup)==FALSE]!=validtotal$group[is.na(rf.predup)==FALSE])
MultiLogLoss(y_pred = predrfupprobs, y_true = validtotal$group)

performancerfallup <-data.frame('Misclassification' = c("80.59%"), 'Logloss' = c(2.285933))
kable(performancerfallup, caption = "Random Forest Performance: All Features & Upsampled") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionrfallup <- as.data.frame(prop.table(table(rf.predup, validtotal$group),2))
names(confusionrfallup)[3] <- 'Prop'
prfallup<-ggplot(confusionrfallup, aes(Var2, rf.predup)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='All Features & Upsampled')
prfallup


# Downsample
set.seed(1223)
fit.rfdown <- randomForest(as.factor(group)~., data = downSampledTrain, na.action = na.omit)
rf.preddown <- predict(fit.rfdown, newdata=validtotal)
predrfdownprobs <-predict(fit.rfdown, validtotal, type = 'prob')

mean(rf.preddown[is.na(rf.preddown)==FALSE]!=validtotal$group[is.na(rf.preddown)==FALSE])
MultiLogLoss(y_pred = predrfdownprobs, y_true = validtotal$group)

performancerfalldown <-data.frame('Misclassification' = c("83.15%"), 'Logloss' = c(2.357747))
kable(performancerfalldown, caption = "Random Forest Performance: All Features & Downsampled") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionrfalldown <- as.data.frame(prop.table(table(rf.preddown, validtotal$group),2))
names(confusionrfalldown)[3] <- 'Prop'
prfalldown<-ggplot(confusionrfalldown, aes(Var2, rf.preddown)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='All Features & Downsampled')
prfalldown

# SVM: All features -----
set.seed(1223)
fit.svm <- svm(as.factor(group) ~ . , traintotal, probability=TRUE)
predsvm <- predict(fit.svm, validtotal, probability = T)
predsvmprobs <- attr(predsvm, 'probabilities')

mean(as.character(predsvm) != as.character(validtotal$group))
MultiLogLoss(y_pred = predsvmprobs, y_true = validtotal$group)

performancesvmall <-data.frame('Misclassification' = c("81.32%"), 'Logloss' = c(2.656179))
kable(performancesvmall, caption = "SVM Performance: All Features") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionsvmall <- as.data.frame(prop.table(table(predsvm, validtotal$group),2))
names(confusionsvmall)[3]<-'Prop'
psvmall<-ggplot(confusionsvmall, aes(Var2, predsvm)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='All Features')
psvmall

# SVM: reduce features -----
ginisel<-as.character(ginis[,2])
traingini <- select(traintotal, -c(ginisel[39:63]))
validgini <- select(validtotal, -c(ginisel[39:63]))

set.seed(1223)
fit.svmn <- svm(as.factor(group) ~ ., traingini, probability=TRUE)
predsvmn <- predict(fit.svmn, validgini, probability = T)
predsvmnprobs <- attr(predsvmn, 'probabilities')

mean(as.character(predsvmn) != as.character(validgini$group))
MultiLogLoss(y_pred = predsvmnprobs, y_true = validgini$group)

performancesvmn <-data.frame('Misclassification' = c("81.88%"), 'Logloss' = c(2.633561))
kable(performancesvmn, caption = "SVM Performance: Reduced Features") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionsvmn <- as.data.frame(prop.table(table(predsvmn, validgini$group),2))
names(confusionsvmn)[3]<-'Prop'
psvmn<-ggplot(confusionsvmn, aes(Var2, predsvmn)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Reduced Features')
psvmn

# Logistic Regression: Reduced features -----
train_log<- traingini
train_log<-na.omit(train_log)
valid_log<- validgini
valid_log<-na.omit(valid_log)

model_log <- multinom(group ~ ., data = train_log, MaxNWts = 100000, model = TRUE, probability = TRUE)
pred_log<-predict(model_log,valid_log)
pred_log_score<-predict (model_log,valid_log, 'probs')
mean(as.character(pred_log) != as.character(valid_log$group))
MultiLogLoss(y_pred = pred_log_score, y_true = valid_log$group)

performancelogred <-data.frame('Misclassification' = c("82.53%"), 'Logloss' = c(2.385173))
kable(performancelogred, caption = "Multinomial Logistic Regression Performance:\n Reduced Features") %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "condensed"), full_width = F)

confusionlogred <- as.data.frame(prop.table(table(pred_log, valid_log$group),2))
names(confusionlogred)[3]<-'Prop'
plogred<-ggplot(confusionlogred, aes(Var2, pred_log)) +
  geom_tile(aes(fill = Prop), colour='gray') +
  scale_fill_gradient(low='antiquewhite', high = 'purple') +
  theme(axis.text.x = element_text(angle = 80, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12)) +
  labs(x= 'Observed', y= 'Predicted', title = 'Confusion Matrix', subtitle='Reduced Features')
plogred

