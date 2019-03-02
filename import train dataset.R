<<<<<<< HEAD
train <- read.csv("train.csv", head = T)
require(data.table)
setDT(train)
train <- train[,-1]


app_labels <- fread('app_labels.csv', header = T)
labelcategory <- fread('label_standardized categories.csv', header = T)
labelsdat <- merge(x = app_labels, y = labelcategory, by = 'label_id', all.x = T)


=======
train <- read.csv("train.csv", head = T)
require(data.table)
setDT(train)



app_labels <- fread('app_labels.csv', header = T)
labelcategory <- fread('label_standardized categories.csv', header = T)
labelsdat <- merge(x = app_labels, y = labelcategory, by = 'label_id', all.x = T)

table(labelsdat$category)
>>>>>>> 0f64c57cb0bd70330a252e8f1adb586fab843f35
