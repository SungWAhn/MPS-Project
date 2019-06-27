# group distribution
group <- train$group
red <- rep("red",6)
green <- rep("green", 6)
colors <- c(red, green)

plot(factor(group), col = colors)
# frequency table
table(group)




# location distribution
sum(is.na(train$latitude))   # 0, no missing data
sum(train$longitude==0| train$longitude==1 | train$latitude==0 | train$latitude==1)
# 7450667
sum(train$longitude==0| train$longitude==1 | train$latitude==0 | train$latitude==1)/dim(train)[1]
# 0.5777316

table(train$device_id.event_id)







# count the min/mean/median/max of number of event_id in each device_id
library(plyr)
library(dplyr)
attach(train)


require(data.table)
setDT(train)[, count := uniqueN(event_id), by = device_id]
count_event = train[, list(device_id, group,count)]
count_event2 = unique(count_event)  # length 23309
sum(count_event2$count)  # 1215595 = unique event_id

summary(count_event2$count)    # 23309 rows
# min 1
# max 4150
# median 15
# mean 52.15
count_max = count_event2[count_event2$count==4150]
count_min = count_event2[count_event2$count==1]    # 2375 rows
count_median = count_event2[count_event2$count==15]  # 335
sum(count_event2$count>52)    # 5680 rows
hist(count_event2$count,breaks=50)

# get the frequency count for each device_id in the 12 groups
F1 <- count_event2[group=='F23-']
F2 <- count_event2[group=='F24-26']
F3 <- count_event2[group=='F27-28']
F4 <- count_event2[group=='F29-32']
F5 <- count_event2[group=='F33-42']
F6 <- count_event2[group=='F43+']
M1 <- count_event2[group=='M22-']
M2 <- count_event2[group=='M23-26']
M3 <- count_event2[group=='M27-28']
M4 <- count_event2[group=='M29-31']
M5 <- count_event2[group=='M32-38']
M6 <- count_event2[group=='M39+']




grp <- c('F23-','F24-26','F27-28','F29-32','F33-42','F43+','M22-','M23-26','M27-28','M29-31','M32-38','M39+')
len <- length(grp)
grp_num <- rep(0, len)
grp_sum <- rep(0, len)
grp_mean <- rep(0, len)
grp_median <- rep(0, len)
grp_max <- rep(0, len)
grp_min <- rep(0, len)
for (i in 1:len){
  thisgroup <- count_event[group==grp[i]]
  grp_num[i] <- dim(thisgroup)[1]
  grp_sum[i] <- sum(thisgroup$count)
  grp_mean[i] <- mean(thisgroup$count)
  grp_median[i] <- median(thisgroup$count)
  grp_max[i] <- max(thisgroup$count)
  grp_min[i] <- min(thisgroup$count)
}
barplot(grp_sum,names.arg=grp, las=2)
barplot(grp_num,names.arg=grp, las=2)
barplot(grp_mean,names.arg=grp, las=2)
grp_event <- data.frame('group'=grp,'number'=grp_num,'sum'=grp_sum,'mean'=grp_mean,'median'=grp_median,
                        'max'=grp_max,'min'=grp_min)
grp_event






#write.csv(labelsdat, file = "labelsdat.csv")

