# load data and merge
library(knitr)
library(readr)
library(data.table)
library(tidyverse)
library(bit64)

# Plain setwd doesn't work for some reason
# setwd('D:\Cornell\MPS Project\data')
events <- fread(file = 'events.csv', header = T)
app_events <- fread('app_events.csv', header = T)
app_labels <- fread('app_labels.csv', header = T)
labelcategory <- fread('label_categories.csv', header = T)
gender_age <- fread('gender_age_train.csv', header = T)
  
# Use read_csv here because of the Chinese characters issue
phonebrand <- read_csv('phone_brand_device_model.csv', locale = locale(encoding = 'UTF-8'))
  
uniquephones <- unique(phonebrand)
  
# Merge sequentially, starting with datasets that have unique indexes
# Convert uniquephones to a data.table to prevent any memory issues
merged1 <- merge(x = setDT(uniquephones), y = gender_age, by = 'device_id')
merged2 <- merge(x = merged1, y = events, by = 'device_id')
merged3 <- merge(x = merged2, y = app_events, by = 'event_id', all.x=T)
train <- merged3
# Merge the app_label with label_category
labelsdat <- merge(x = app_labels, y = labelcategory, by = 'label_id', all.x = T)
category <- labelsdat


# Maybe don't need to do this final merge?
# Convert the data.frames to a data.table to prevent any memory issues
# finalmerge <- merge(x = setDT(merged3), y = setDT(labelsdat), by = 'app_id', allow.cartesian = T, all.x = T)

hist(train$age,breaks=20)