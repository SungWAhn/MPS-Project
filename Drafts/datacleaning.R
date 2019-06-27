phone <- train$phone_brand

phone[phone=='乐视'] = 'le'
phone[phone=='美图'] = 'meitu'
phone[phone=='锤子'] = 'smartisan'
phone[phone=='亿通'] = 'yitong'
phone[phone=='中兴'] = 'zte'
phone[phone=='酷比'] = 'koobee'
phone[phone=='世纪天元'] = 'ctyon'
phone[phone=='朵唯'] = 'doov'
phone[phone=='努比亚'] = 'nubia'
phone[phone=='酷派'] = 'coolpad'
phone[phone=='联想'] = 'lenovo'
phone[phone=='金立'] = 'gionee'
phone[phone=='华为'] = 'huawei'
phone[phone=='魅族'] = 'meizu'
phone[phone=='小米'] = 'mi'
phone[phone=='三星'] = 'samsung'


train2 <- train
train2$phone_brand <- phone

device <- train$device_model
device[device=='联想黄金斗士S8'] = 'lenovo s8'
device[device=='畅享5'] = 'enjoy 5'
device[device=='荣耀6'] = 'honor 6'
device[device=='超级手机1s'] = 'superphone 1s'
device[device=='荣耀6 Plus'] = 'honor 6 plus'
device[device=='红米'] = 'redmi'
device[device=='大神F1'] = 'dazen f1'
device[device=='荣耀3C'] = 'honor 3c'
device[device=='红米Note2'] = 'redmi note2'
device[device=='红米note'] = 'redmi note2'
device[device=='2016版 Galaxy A9'] = '2016 Galaxy A9'
device[device=='荣耀畅玩5X'] = 'honor 5x'
device[device=='荣耀畅玩4X'] = 'honor 4x'
device[device=='魅蓝metal'] = 'm1 metal'
device[device=='红米1S'] = 'redmi 1s'
device[device=='红米2A'] = 'redmi 2a'
device[device=='魅蓝'] = 'm1'
device[device=='魅蓝2'] = 'm1 2'
device[device=='note顶配版'] = 'mi note pro'
device[device=='乐檬K3 Note'] = 'k3 note'
device[device=='魅蓝Note 2'] = 'm1 note 2'
device[device=='荣耀7'] = 'honor 7'
device[device=='荣耀4A'] = 'honor 4a'
device[device=='魅蓝NOTE'] = 'n1 note'
device[device=='超级手机1'] = 'superphone 1'
device[device=='锋尚Pro'] = 'fs pro'
device[device=='远航3'] = 'a3'
device[device=='荣耀3C畅玩版'] = 'honor 3c pro'
# device[device==''] = ''


train2$device_model <- device

write.csv(train2, file = "train_clean.csv", row.names=FALSE)
