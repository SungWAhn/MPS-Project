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
count_event = train[, c(2,14)]
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






# regular expressions
# make more general categories
library(stringr)
str<- str_to_lower(labelcategory$category)
word <- str_split(str, boundary("word"))
freq<- table(unlist(word))
count <- as.data.frame(freq)
 
count <- count[order(count$Freq,decreasing=T), ]






write.csv(labelsdat, file = "labelsdat.csv")

