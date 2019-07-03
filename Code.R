# This is all the code used on the project

library(knitr)
library(data.table)
library(bit64)
library(Rmpfr)
library(readxl)
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
library(ggmap)
library(scales)
library(chron)


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


# Data Cleaning -----

# Translate Chinese characters in the phone brand and model columns

# Read in the translation key dataset
chin_eng_brand <- read_excel("phone_brand-translate.xlsx", sheet = "Sheet1")
chin_eng_model <- read_excel("device_model translate.xlsx", sheet="Sheet1")

kable(chin_eng_brand[1:10,], caption = 'Preview of Phone Brand Translation Key') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(chin_eng_model[1:10,], caption = 'Preview of Phone Model Translation Key') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Replace each phone brand name with an english one with for loop
phone <- phonebrand$phone_brand
for (i in 1:121) {
  phone[phone==chin_eng_brand$`original phone brand`[i]] = chin_eng_brand$`new phone brand`[i]
}
phonebrand$phone_brand <- phone

# Replace each phone model name with an english one with for loop
model<-phonebrand$device_model
for (i in 1:178){
  model[model==chin_eng_model$`original model name`[i]]=chin_eng_model$`new model name`[i]
}
phonebrand$device_model <- model

# Check to make sure that all Chinese characters have been replaced
leftoversbr <- phonebrand[which(grepl("[[:alnum:]]", phonebrand$phone_brand) == FALSE), ]
leftoversbr

leftoversmod <- phonebrand[which(grepl("[[:alnum:]]", phonebrand$device_model) == FALSE), ]
leftoversmod

# Dataset structure -----

## events dataset
kable(arrange(events, event_id)[1:15,], caption = 'Preview of Events Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicate event_id? NO
dim(events[which(duplicated(events$event_id) == T),])
# Any duplicated device_id? YES
dim(events[which(duplicated(events$device_id) == T),])

# An of the example duplicates
kable(events[device_id==events[[81, 2]]][1:10,], caption = 'One device_id can have many Events') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# This dataset has a list of unique events, but multiple events could have same device_ids. The event_id is unique index.

## app_events dataset
kable(arrange(app_events, event_id)[1:15,], caption = 'Preview of App_Events Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicate event_id? YES
dim(app_events[which(duplicated(app_events$event_id) == T),])
# Any duplicate app_id? YES
dim(app_events[which(duplicated(app_events$app_id) == T),])

# An example of the duplicates
kable(app_events[event_id == 568,][1:10], caption = 'Multiple Apps Used in One Event') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(app_events[app_id == '6284164581582112235',][1:10], caption = 'Same Apps are used in Multiple Events') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# For every event_id, there are multiple app_ids associated. Same app_ids can be associated with different event_ids. This dataset has no unique index.

## app_labels dataset
kable(arrange(app_labels, app_id)[1:15,], caption = 'Preview of App_labels Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicate app_id? YES
dim(app_labels[which(duplicated(app_labels$app_id) == T),])
# Any duplicated label_id? YES
dim(app_labels[which(duplicated(app_labels$label_id) == T),])

# An of the example duplicates
kable(app_labels[app_id==app_labels[[4, 1]],][1:10], caption = 'Same App is Under Multiple Categories') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(app_labels[label_id==app_labels[[5, 2]],][1:10], caption = 'Multiple Apps Are Under Same Category') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# App_ids can be categorized under multiple label_ids, and the same label_ids are used multiple times for different app_ids. No unique index.

## label_category dataset
kable(arrange(labelcategory, label_id)[1:15,], caption = 'Preview of Label_Category Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicate label_id? NO
dim(labelcategory[which(duplicated(labelcategory$label_id) == T),])
# Any duplicated category? YES
dim(labelcategory[which(duplicated(labelcategory$`sub-category`) == T),])

# An of the example duplicates
kable(labelcategory[`sub-category`=="Medical"], caption = 'Some Label_id have same sub-category') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(labelcategory[category=="game"][1:10], caption = 'Many Label_id have same category') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Label_id is unique. Sub-categories and categries are not unique, so multiple label_ids can have the same sub-category or categories.

## Phonebrand dataset
kable(arrange(phonebrand, device_id)[1:15,], caption = 'Preview of Phonebrand Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicates in device_id? YES
dim(phonebrand[which(duplicated(phonebrand$device_id) == T),])

# Example duplicates
kable(phonebrand[phonebrand$device_id==phonebrand[[22626,1]],], caption = 'Some Device_id are repeated with exact same information') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)
kable(phonebrand[which(phonebrand$device_id==phonebrand[[34351,1]]),], caption = 'Some Device_id are repeated with exact same information') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

## Gender_age dataset
kable(arrange(gender_age, device_id)[1:15,], caption = 'Preview of Gender_Age Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Any duplicate device_id? NO
dim(gender_age[which(duplicated(gender_age$device_id) == T),])


# Gender_age has a unique device_id column. Every individual has a gender/age class.


# Merge Operations -----
# First delete the duplicate rows in phonebrand dataset, while at the same time 
# keeping the device_ids that have two different phone models. The unique() 
# function will do this for us since it only deletes rows with exact same information.

uniquephones <- unique(phonebrand)


# First left join phonebrand with gender_age by device_id
# Convert uniquephones to a data.table to prevent any memory issues
merged1 <- merge(x = gender_age, y = setDT(uniquephones), by = 'device_id', all.x = T)

# The result
kable(merged1[1:6,], caption = 'Gender_Age + Phonebrand') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(1, width = "15em")



# Second Merge: Left Join the previous dataset with Events by device_id
merged2 <- merge(x = merged1, y = events, by = 'device_id', all.x = T)


# Before moving on to the next merge, remove observations where every 
# column except the response columns, gender, age, and group, is missing
  
  
merged2.noNA <- merged2[is.na(merged2$phone_brand)==FALSE | is.na(merged2$event_id)==FALSE]

# The result
kable(merged2.noNA[1:6,], caption = 'Previous + Events') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(1, width = "12em") %>%
  column_spec(6, width = "6em")



# The third merge. Left join the previous dataset with App_Events by event_id.

# Call this third merge the train dataset
train <- merge(x = merged2.noNA, y = app_events, by = 'event_id', all.x = T)

# The result
kable(train[1:6,], caption = "Previous + App_Events") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)


# Save the final train dataset
fwrite(train, "train.csv", na="NA")

# There are still 2 datasets left pertaining to app descriptions. Merged separately to keep the train dataset from getting very large.
# Left Join App_labels with Label_category by label_id. Call this labelsdat

# Merge the app_label with label_category
labelsdat <- merge(x = app_labels, y = labelcategory, by = 'label_id', all.x = T)

kable(labelsdat[1:10,], caption = 'App_labels + Label_category') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Try merging a few columns from the train dataset with the labelsdat dataset
response <- train[, list(device_id, event_id, app_id)]

kable(response[1:10,], caption = 'Subset of Train Dataset') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Left join subset of train with Labelsdat by App_id. Call this labeltrain
# In labelsdat, each app_id can have repeated categories. Before merging, 
# filter the labelsdat dataset so that each app_id has a unique list of categories
uniquelabels <- labelsdat %>%
  arrange(app_id) %>%
  group_by(app_id, category) %>%
  distinct(category)

labeltrain <- merge(x = response, y = uniquelabels, by = 'app_id', all.x = T, allow.cartesian = T)

# The result
kable(labeltrain[is.na(app_id) == FALSE,][1:10,], caption = 'Subset of Train + Labelsdat: Labeltrain') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

# Save dataset
fwrite(labeltrain, "labeltrain.csv", na="NA")
fwrite(labelsdat, "labelsdat.csv", na="NA")
fwrite(uniquelabels, "uniquelabels.csv", na="NA")


# Merge Summary

# Observation count of first three merges
merge.dims <- data.frame("Table" = c("Gender_Age", "Previous + Phonebrand", "Previous + Events", "Delete Unneeded Rows", "Previous + AppEvents"), "Join Type" = c("Starting Point", "Left Join", "Left Join", "", "Left Join"), "Merge Key" = c("Device ID", "Device ID", "Device ID", "", "Event ID"), "Uniqueness" = c("Unique", "Unique", "Not Unique", "", "Not Unique"), "Obs Count" = c(dim(gender_age)[1], dim(merged1)[1], dim(merged2)[1], dim(merged2.noNA)[1], dim(train)[1]))

# Observation count of the last two merges
merge.dims2 <- data.frame("Table" = c("App_labels", "Previous + Label_category", "Previous + Subset of Train"), "Join Type" = c("Starting Point", "Left Join", "Left Join"), "Merge Key" = c("Label ID", "Label ID", "App ID"), "Uniqueness" = c("Not Unique", "Unique", "Not Unique"), "Obs Count" = c(dim(app_labels)[1], dim(labelsdat)[1], dim(labeltrain)[1]))


kable(merge.dims, caption = "Change in Length of Table for train dataset") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(2, width = "12em")
kable(merge.dims2, caption = "Change in Length of Table for Labeltrain") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(2, width = "12em")



# Device id count
merge.idcount <- data.frame("Table" = c("Gender_Age", "Previous + Phonebrand", "Previous + Events", "Delete Unneeded Rows", "Previous + AppEvents"), "Join Type" = c("Starting Point", "Left Join", "Left Join", "", "Left Join"), "Merge Key" = c("Device ID", "Device ID", "Device ID", "", "Event ID"), "Uniqueness" = c("Unique", "Unique", "Not Unique", "", "Not Unique"), "Device ID Count" = c(length(unique(gender_age$device_id)), length(unique(merged1$device_id)), length(unique(merged2$device_id)), length(unique(merged2.noNA$device_id)), length(unique(train$device_id))))


merge.idcount2 <- data.frame("Table" = c("App_labels", "Previous + Label_category", "Previous + Subset of Train"), "Join Type" = c("Starting Point", "Left Join", "Left Join"), "Merge Key" = c("Label ID", "Label ID", "App ID"), "Uniqueness" = c("Not Unique", "Unique", "Not Unique"), "Device ID Count" = c(length(unique(app_labels$app_id)), length(unique(labelsdat$app_id)), length(unique(labeltrain$app_id))))


merge.idcount3 <- data.frame("Table" = c("App_labels", "Previous + Label_category", "Previous + Subset of Train"), "Join Type" = c("Starting Point", "Left Join", "Left Join"), "Merge Key" = c("Label ID", "Label ID", "App ID"), "Uniqueness" = c("Not Unique", "Unique", "Not Unique"), "Device ID Count" = c("NA", "NA", length(unique(labeltrain$device_id))))


kable(merge.idcount, caption = "Change in Device ID Count for train dataset") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(2, width = "9em")
kable(merge.idcount3, caption = "Device ID Count for labeltrain dataset") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F) %>%
  column_spec(2, width = "9em")

# Exploratory Analysis -----
train <- fread("train.csv", header = T, integer64 = "character", na.strings = "NA")
setDT(train[,-1])
labelsdat <- fread("labelsdat.csv", header = T, integer64 = "character", na.strings = "NA")
labeltrain <- fread("labeltrain.csv", header = T, integer64 = "character", na.strings = "NA")
uniquelabels <- fread("uniquelabels.csv", header = T, integer64 = "character", na.strings = "NA")

## Missing Values -----

### Location Data
dim(train[is.na(longitude) | is.na(latitude) | longitude == "" | latitude == ""])
dim(train[is.na(longitude) | is.na(latitude) | longitude == "" | latitude ==
            ""])[1]/dim(train)[1]*100

inva_locations<-dim(train[(longitude == 1 & latitude ==0) | (longitude ==0 & latitude ==1) | (longitude ==1 & latitude ==1) | (longitude ==0 & latitude ==0)])[1]/length(train$longitude)
inva_locations

# There are 332 missing observations which is only .0026%. Another 58% of the location data is invalid since the longitude and latitude take values of 0 or 1.

### Phone Brand and Model Data
dim(train[train$phone_brand==""])
dim(train[train$phone_brand==""])[1]/dim(train)[1]*100

dim(train[train$device_model==""])
dim(train[train$device_model==""])[1]/dim(train)[1]*100

# Almost 100% of phonebrands and phone models are missing

### Is_Installed and Is_Active Data
dim(train[is.na(is_installed)])
dim(train[is.na(is_installed)])[1]/dim(train)[1]*100

dim(train[is.na(is_active)])
dim(train[is.na(is_active)])[1]/dim(train)[1]*100

# 5.11% missing in is_installed and is_active variables


dim(train[is.na(app_id) | app_id == ""])
dim(train[is.na(app_id)])[1]/dim(train)[1]*100

# 5.11% missing in app_id

dim(train[is.na(timestamp) | timestamp == ""])
dim(train[is.na(timestamp) | timestamp == ""])[1]/dim(train)[1]*100
dim(train[is.na(event_id) | event_id == ""])
dim(train[is.na(event_id) | event_id == ""])[1]/dim(train)[1]*100

# There are 332 missing observations in both timestamp and event_id.

### The rest: no missing values
dim(train[is.na(device_id) | device_id == ""])
dim(train[is.na(gender) | gender == ""])
dim(train[is.na(age) | age == ""])
dim(train[is.na(group) | group == ""])

## Dataset Overview by Event ID Count and Device ID Count -----
train2 <- train
require(data.table)
setDT(train2)[, count := uniqueN(event_id), by = device_id]
count_event = train2[, list(device_id, group,count)]
count_event2 = unique(count_event)  # length 23309

grp <- c('F23-','F24-26','F27-28','F29-32','F33-42','F43+','M22-','M23-26','M27-28','M29-31','M32-38','M39+')
len <- length(grp)
grp_num <- rep(0, len)
grp_sum <- rep(0, len)
grp_mean <- rep(0, len)
grp_median <- rep(0, len)
grp_max <- rep(0, len)
grp_min <- rep(0, len)

for (i in 1:len){
  thisgroup <- count_event2[group==grp[i]]
  grp_num[i] <- dim(thisgroup)[1]
  grp_sum[i] <- sum(thisgroup$count)
  grp_mean[i] <- mean(thisgroup$count)
  grp_median[i] <- median(thisgroup$count)
  grp_max[i] <- max(thisgroup$count)
  grp_min[i] <- min(thisgroup$count)
}

grp_event <- data.frame('group'=grp,'number'=grp_num,'sum'=grp_sum,'mean'=grp_mean,'median'=grp_median,
                        'max'=grp_max,'min'=grp_min)

grp_event %>%
  kable(escape = F, caption = 'Event and Device ID Count by Group') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive"), full_width = F)

gender <- c(rep('Female', 6),rep('Male', 6))
num_percent <- grp_event$number/sum(grp_event$number)
num_percent <- signif(num_percent, digits = 3)
sum_percent <- grp_event$sum/sum(grp_event$sum)
sum_percent <- signif(sum_percent, digits = 3)
grp_event2 <- cbind(gender, grp_event, num_percent, sum_percent)

ggplot(data=grp_event2,aes(x=group,y=num_percent,fill=gender))+
  geom_bar(position='dodge', stat='identity')+
  geom_text(aes(label=number), vjust=-0.25)+
  geom_text(aes(label=paste0(num_percent*100,"%")), vjust=1.75,color='white',size=3.2)+
  scale_y_continuous(labels = percent) +
  ggtitle('Unique Device ID by Group')+
  ylab('percentage')

ggplot(data=grp_event2,aes(x=group,y=sum_percent,fill=gender))+
  geom_bar(position='dodge', stat='identity')+
  geom_text(aes(label=sum), vjust=-0.25, size=3)+
  geom_text(aes(label=paste0(sum_percent*100,"%")), vjust=1.75,color='white',size=3.2)+
  scale_y_continuous(labels = percent) +
  ggtitle('Event Distribution by Group')+
  ylab('percentage')

ggplot(data=grp_event2,aes(x=group,y=mean,fill=gender))+
  geom_bar(position='dodge', stat='identity')+
  geom_text(aes(label=round(mean)), vjust=-0.25, size=3.5)+
  ggtitle('Mean Event Count by Group')+
  ylab('count')

## Age-Gender distribution -----
agebydevice <- train %>%
  select(device_id, gender, age) %>%
  distinct(device_id, .keep_all = T)

ggplot(data = agebydevice, aes(x = age, fill = gender, color = gender)) +
  geom_histogram(stat = "count",alpha = .5 ,position = "identity")+
  ggtitle("Age-Gender Distribution by Device Count")

## Phonebrand distribution -----
phone_count2 <- train %>%
  group_by(phone_brand) %>%
  distinct(device_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(data = phone_count2[2:21,], aes(x = reorder(phone_brand, -count), y = count, fill=count)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  labs(x = 'Phone Brands', y = 'Count', title = 'Phone Brand Distribution')

## App label count -----
labelcount <- uniquelabels %>%
  group_by(category) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>% 
  arrange(desc(percent))


ggplot(data = labelcount, aes(x = reorder(category,-percent), y=percent, fill = percent)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 13),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7)) +
  scale_y_continuous(labels = percent) +
  labs(x = 'App Categories', y = 'Percent', title = 'App Category Distribution')

## Is_active distribution
isactiveperc <- train %>%
  group_by(is_active) %>%
  dplyr::summarise(n= n()) %>%
  mutate(percent = n/sum(n))

ggplot(data=isactiveperc,aes(x=factor(is_active), y = percent))+
  geom_bar(stat="identity", fill="steelblue")+
  scale_y_continuous(labels = percent) +
  ggtitle("Is_Active Count")+
  labs(x = "is_active", y = "Count")

## Location data -----
# This takes a long time to load!
locations <- train %>%
  select(longitude, latitude) %>%
  filter(longitude != 1 & longitude != 0 & latitude !=1 & latitude != 0)
box <- make_bbox(lon = locations$longitude, lat = locations$latitude, f = .1)

map <- get_map(location = box, maptype = 'terrain', source = 'google')

p <- ggmap(map)
p<-p + geom_point(data = locations, mapping = aes(x = longitude, y = latitude), color = "red")

# Feature Engineering -----

## Is_Active Ratio feature -----
# Subset of the training data
response <- train[, list(device_id, event_id, app_id, is_active)]

# Vector of all unique device_ids
deviceid <- unique(train$device_id)

# Merge response with uniquelables
isactivecats <- merge(x = response, y = uniquelabels, by = 'app_id', all.x = T, allow.cartesian = T)
isactivecats <- isactivecats[order(device_id)]

# Only keep observations with is_active=1, count number of is_active occurrences in each category
sum.isactive.bycat <- isactivecats %>%
  filter(is_active == 1) %>%
  group_by(device_id, category, is_active) %>%
  dplyr::summarise(isactive.count = n())

# Count the number of apps used in each category
sum.apps.bycat <- isactivecats %>%
  filter(is_active == 1) %>%
  group_by(device_id, category, app_id) %>%
  distinct(app_id, .keep_all = T) %>%
  #dplyr::summarise(count = n()) %>%
  group_by(device_id, category) %>%
  dplyr::summarise(count = n())

sum.isactive.bycat$appcount <- sum.apps.bycat$count

kable(sum.isactive.bycat[1:15,4:5], caption = 'App count vs. Is_Active Count') %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "responsive", "condensed"), full_width = F)

# Calculate the ratios
sum.isactive.bycat <- sum.isactive.bycat %>%
  mutate(ratio = isactive.count/appcount)

feature <- sum.isactive.bycat %>%
  select(-one_of("is_active", "isactive.count", "appcount")) %>%
  spread(key = category, value = ratio)

feature[is.na(feature)]=0

# Some device_ids not in the final table: these devices had no is_active cases
missing <- setdiff(deviceid, feature$device_id)
nas <- matrix(nrow = 394, ncol = 23)
missingdata <- cbind(missing, nas)
colnames(missingdata) <- colnames(feature)
feature <- rbind.data.frame(feature, missingdata)

## Location, event count, app count -----
test<-train
test$longitude[test$longitude==0 | test$longitude==1]<-NA
test$latitude[test$latitude==0 | test$latitude==1]<-NA

test1<-test %>%
  select(device_id, longitude, latitude, event_id) %>%
  group_by(device_id) %>%
  dplyr::summarise(longitude=mean(longitude, na.rm=TRUE), latitude=mean(latitude, na.rm=TRUE), count_event=n_distinct(event_id))

require(data.table)
setDT(test1)
setDT(labeltrain)

labeltest<-labeltrain %>%
  group_by(device_id) %>%
  dplyr::summarise(n_distinct(app_id))

require(data.table)
setDT(labeltest)

m <- merge(x = labeltest, y = test1, by = 'device_id', all.x = T)

# Count for AM/PM, Weekend/Weekday, April 30 - May 8
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
temp <- date.unique[1]
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

# count number of unique events for each device in each timechunk of 4 hours
count.timechunk <- list()
count.name <- c('count.0_4','count.4_8','count.8_12','count.12_16','count.16_20','count.20_24')
for (i in 1:6){
  temp <- train2[time.hour < i*4 & time.hour >= (i-1)*4,]
  setDT(temp)[, count := uniqueN(event_id), by = device_id]
  count.timechunk[[i]] <- unique(temp[,list(device_id,count)])
  names(count.timechunk[[i]])[2] <- count.name[i]
}

# merge the 6 tables to form a single table of counts
time.count <- count.timechunk %>% reduce(full_join, by = "device_id")
# replace NA values with 0
time.count[is.na(time.count)] <- 0

# check whether merge retrieve back all device id
length(unique(train2$device_id))   # 23309 
dim(time.count)   # 23309 rows

# merge date.count and time.count
timestamp.count <- list(date.count,time.count) %>% reduce(full_join, by = "device_id")
dim(timestamp.count)    # 23309 * 18

## App category count -----
category_feature <- data.frame(xtabs(~device_id + category, data = labeltrain)) %>%
  spread(category, Freq)

# Create the final matrix by merging all the features
finaltable <- list(m, category_feature, timestamp.count, feature) %>% reduce(full_join, by = "device_id")
finaltable <- merge(x = finaltable, y = gender_age[,list(device_id, group)], by = "device_id", all.x = T)
brandmodel <- train[,list(device_id, phone_brand, device_model)]
finaltable <- merge(x = finaltable, y = unique(brandmodel), by = "device_id")
finaltable$group <- as.factor(finaltable$group)

str(finaltable)
head(finaltable, n = 25)
summary(finaltable$group)

## Training Validation split -----
set.seed(1)
sample <- sample.int(n=nrow(finaltable), size = floor(.8*nrow(finaltable)), replace = F)
validation <- finaltable[-sample,]
training <- finaltable[sample,]

fwrite(validation, "validation.csv")
fwrite(training, "training.csv")


# Model Fitting -----

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

