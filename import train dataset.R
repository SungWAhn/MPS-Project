train <- read.csv("train.csv", head = T)
require(data.table)
setDT(train)
train <- train[,-1]


app_labels <- fread('app_labels.csv', header = T)
labelcategory <- fread('label_standardized categories.csv', header = T)
labelsdat <- merge(x = app_labels, y = labelcategory, by = 'label_id', all.x = T)


