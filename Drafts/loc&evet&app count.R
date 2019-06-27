test<-train
test$longitude[test$longitude==0 | test$longitude==1]<-NA
test$latitude[test$latitude==0 | test$latitude==1]<-NA

test1<-test %>%
select(device_id, longitude, latitude, event_id) %>%
group_by(device_id) %>%
summarise(longitude=mean(longitude, na.rm=TRUE), latitude=mean(latitude, na.rm=TRUE), count_event=n_distinct(event_id))

require(data.table)
setDT(test1)
setDT(labeltrain2)

labeltest<-labeltrain2 %>%
group_by(device_id) %>%
summarise(n_distinct(app_id))

require(data.table)
setDT(labeltest)

m <- merge(x = labeltest, y = test1, by = 'device_id', all.x = T)