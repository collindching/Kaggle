library(caret)
library(e1071)
library(tidyverse)

source("workflow/functions.R")

##=========================== application without engineered features ===========================
load("data/app_data.Rda")

## convert empty levels to unknown
fcols <- which(sapply(app.tot,is.factor))
has_empty <- fcols[which(sapply(app.tot[,fcols],function(x) "" %in% levels(x)))]
app.tot[,fcols] <- lapply(app.tot[,fcols],function(x) {
    levels(x)[levels(x)==""] <- "unknown"
    return(x)})

## clean ORGANIZATION_TYPE levels
business_names <- paste("Business Entity Type 1",1:3)
industry_names <- paste("Industry: type",1:13)
trade_names <- paste("Trade: type",1:7)
transport_names <- paste("Transport: type", 1:4)

app.tot$ORGANIZATION_TYPE <- fct_collapse(app.tot$ORGANIZATION_TYPE,
                                          Business = c("Business Entity Type 1",
                                                       "Business Entity Type 2",
                                                       "Business Entity Type 3"),
                                          Industry = industry_names,
                                          Trade = trade_names,
                                          Transport = transport_names)

## retain predictors with less than 50% missing
app.tot <- app.tot[,which(colMeans(is.na(app.tot))<=.6)]
dim(app.tot)

##  SPlIT DATA 

app.tr <- app.tot[1:nrow(app.tr),]
app.ts <- app.tot[-c(1:nrow(app.tr)),-2]

## CLEAN SEPARATELY

## remove XNA gender
app.tr <- app.tr[app.tr$CODE_GENDER!="XNA",]
app.tr$CODE_GENDER <- factor(app.tr$CODE_GENDER)
app.ts$CODE_GENDER <- factor(app.ts$CODE_GENDER)

## remove Maternity leave
app.tr <- app.tr[app.tr$NAME_INCOME_TYPE!="Maternity leave",]
app.tr$NAME_INCOME_TYPE <- factor(app.tr$NAME_INCOME_TYPE)
app.ts$NAME_INCOME_TYPE <- factor(app.ts$NAME_INCOME_TYPE)

## remove Unknown
app.tr <- app.tr[app.tr$NAME_FAMILY_STATUS!="Unknown",]
app.tr$NAME_FAMILY_STATUS <- factor(app.tr$NAME_FAMILY_STATUS)
app.ts$NAME_FAMILY_STATUS <- factor(app.ts$NAME_FAMILY_STATUS)

lapply(app.ts[,c(3,12,14)],levels)
str(app.tot,list.len=5)
str(app.tr,list.len=5)

## split up for imputing
app.tr.x <- app.tr[,-2]
app.tr.y <- app.tr[,2]

## imputing with median... must find numeric but non-binary columns first
num_cols <- which(sapply(app.tr.x,is.numeric))
bin_cols <- which(sapply(app.tr.x,is_binary))
num_cols <- setdiff(num_cols,bin_cols)

## impute training and test set
app.tr_meds <- sapply(app.tr.x[,num_cols],function(x) median(x,na.rm=TRUE))
app.tr.x[,num_cols] <- lapply(app.tr.x[,num_cols],impute_median)
for (i in 1:length(num_cols)) {
    app.ts[,num_cols[i]] <- impute_median(app.ts[,num_cols[i]],colmed=app.tr_meds[i])
}

## cleaned and imputed
app.tr <- cbind(app.tr.x,TARGET=app.tr.y)
app.ts <- app.ts

save(app.tr,app.ts,file="data/processed/app_clean.Rda")

##=========================== final data: adding engineered features ===========================
load("data/processed/app_clean.Rda")
load("data/processed/app_features.Rda")
load("data/processed/bu_features.Rda")
load("data/processed/prev_features.Rda")

final.tr.x <- list(app.tr[,-106],app_features,bu_features,prev_features) %>%
    reduce(left_join,by=c("SK_ID_CURR","SK_ID_CURR","SK_ID_CURR"))

final.tr.y <- app.tr$TARGET
final.ts <- list(app.ts,app_features,bu_features,prev_features) %>%
    reduce(left_join,by=c("SK_ID_CURR","SK_ID_CURR","SK_ID_CURR"))

## impute new columns with training medians 
icols <- 106:ncol(final.tr.x)
final.tr_meds <- sapply(final.tr.x[,icols],function(x) median(x,na.rm=TRUE))
final.tr.x[,icols] <- lapply(final.tr.x[,icols],impute_median)
for (i in 1:length(icols)) {
    final.ts[,icols[i]] <- impute_median(final.ts[,icols[i]],colmed=final.tr_meds[i])
}

final.tr <- cbind(final.tr.x,TARGET=final.tr.y)
final.ts <- final.ts

save(final.tr,final.ts,file="data/processed/final_data.Rda")


