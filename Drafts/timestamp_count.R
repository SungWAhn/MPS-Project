library(dplyr)
library(data.table)
library(tidyverse)
library(chron)

# take a subset of the training data 
# containing information of event_id, device_id and timestamp
train2 <- train[, list(event_id, device_id, timestamp)]

# checking whether each event_id has a timestamp
sum(is.na(train$event_id) & !is.na(train$timestamp))    # 0
sum(!is.na(train$event_id) & is.na(train$timestamp))    # 0

# get rid of the NA values
train2 <- train2[!is.na(train2$event_id)]     # 12896417 rows (original 12896749 rows)

# separate date and time
timesplit <- unlist(strsplit(train2$timestamp, ' '))
len <- length(timesplit)
date.index <- seq(1, len-1, by=2)
time.index <- seq(2, len, by=2)
date <- timesplit[date.index]
time <- timesplit[time.index]
train2 <- cbind(train2,date,time)



# check how many unique dates in the dataset
date.unique <- unique(train2$date)
print(date.unique)
# change the two values to follow time order
temp <- daate.unique[1]
date.unique[1] <- date.unique[2]
date.unique[2] <- temp

# count number of unique event_id per device_id in a specific date
# count is a list with each element being a two-column data frame
# storing the device id and its number of unique events on a specific date 
# from 2016-04-30 to 2016-05-08
count <- list()
date.unique <- unique(train2$date)
for (i in 1:length(date.unique)){
  temp <- train2[train2$date==date.unique[i]]
  setDT(temp)[, count := uniqueN(event_id), by = device_id]
  count[[i]] <- unique(temp[,list(device_id,count)])
  names(count[[i]])[2] <- date.unique[i]
}




# merge the 9 tables to form a single table of counts
merged <- count %>% reduce(full_join, by = "device_id")
# replace NA values with 0
merged[is.na(merged)] <- 0

# check whether merge retrieve back all device id
length(unique(train2$device_id))   # 23309 
dim(merged)   # 23309 rows



# count number of events based on weekday/weekend
# weekend: 2016-04-30(Sat) 2016-05-01(Sun) 2016-05-07(Sat) 2016-05-08(Sun)
# weekday: 2016-05-02(Mon) 2016-05-03(Tue) 2016-05-04(Wed) 2016-05-05(Thu) 2016-05-06(Fri)
weekend <- apply(merged[,c(2,3,9,10)], 1, sum)
weekday <- apply(merged[,4:8], 1, sum)

# append these two columns to merged dataset
date.count <- cbind(merged, weekend, weekday)

















# the time column is in character type, change it to time datatype
temp.time <- chron(times=train2$time)   # note this will take a while
time.hour <- hours(temp.time)

# separate train2 dataset according to morning/afternoon 
morning.data <- train2[time.hour < 12,]
afternoon.data <- train2[time.hour >= 12,]
# count events per device
setDT(morning.data)[, count := uniqueN(event_id), by = device_id]
setDT(afternoon.data)[, count := uniqueN(event_id), by = device_id]
names(morning.data)[6] <- 'morning'
names(afternoon.data)[6] <- 'afternoon'
morning.count <- unique(morning.data[,list(device_id,morning)])
afternoon.count <- unique(afternoon.data[,list(device_id,afternoon)])

# merge the 2 tables to form a single table of counts
time.count <- list(morning.count,afternoon.count) %>% reduce(full_join, by = "device_id")
# replace NA values with 0
time.count[is.na(time.count)] <- 0


# check whether merge retrieve back all device id
length(unique(train2$device_id))   # 23309 
dim(time.count)   # 23309 rows




# merge date.count and time.count
timestamp.count <- list(date.count,time.count) %>% reduce(full_join, by = "device_id")
dim(timestamp.count)    # 23309    14





