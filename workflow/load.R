## load original data

library(data.table)
library(plyr)

#=================================================================================

app.tr <- data.frame(fread("data/application_train.csv",stringsAsFactors=TRUE))
app.tr.x <- app.tr[,-2]
app.tr.y <- app.tr[,2]
app.ts <- data.frame(fread("data/application_test.csv",stringsAsFactors=TRUE))
app.tot <- rbind.fill(app.tr,app.ts)

save(app.tr,app.tr.x,app.tr.y,app.ts,app.tot,file="data/app_data.Rda")

#=================================================================================

load("data/app_data.Rda")

prev_app <- data.frame(fread("data/previous_application.csv",stringsAsFactors=TRUE))

bu <- data.frame(fread("data/bureau.csv",stringsAsFactors=TRUE))
bu_bal <- data.frame(fread("data/bureau_balance.csv",stringsAsFactors=TRUE))

cc_bal <- data.frame(fread("data/credit_card_balance.csv",stringsAsFactors=TRUE))

payments <- data.frame(fread("data/installments_payments.csv",stringsAsFactors=TRUE))

cash_bal <- data.frame(fread("data/POS_CASH_balance.csv",stringsAsFactors=TRUE))


