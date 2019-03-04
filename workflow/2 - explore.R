library(fastDummies)
library(tidyverse)

options(scipen=5)
source("workflow/functions.R")
load("data/app_data.Rda")

##====================================== app ======================================
str(app.tot,list.len=ncol(app.tot))

hist(app.tot$TARGET,main="Default class imbalance",xlab="Default?")

## total NAs
barplot(head(sort(sapply(app.tot,count_na),decreasing=TRUE),65),xaxt='n',main='Missingness') 

## examine factor levels and distribution 
fcols <- which(sapply(app.tot,is.factor))
lapply(app.tot[,fcols],levels)
## convert "" to NA

par(mfrow=c(4,4),mar=c(1,1,1,1),cex=.3)
for (fi in fcols) {
    f_var <- app.tot[,fi,drop=FALSE]
    bake_pie(f_var,names(f_var))
}
# Many variables with very small/narrow levels... how to handle these factors? Definitely inspect manually

## see if factors levels match up in training and testing data 
lvmatch <- rep(NA,length(fcols))
for (i in 1:length(fcols)) {
    fi <- fcols[i]
    lvmatch[i] <- identical(levels(app.tr.x[,fi]),levels(app.ts[,fi]))
}
(mismatch <- fcols[which(!lvmatch)])

levels(app.tr.x[,mismatch[1]])
levels(app.ts[,mismatch[1]])
## remove XNAs in processing

levels(app.tr.x[,mismatch[2]])
levels(app.ts[,mismatch[2]])
data.frame(table(app.tr.x[,mismatch[2]]),
           prob=round(prop.table(table(app.tr.x[,mismatch[2]],app.tr.y),1)[,2],4),
           row.names=NULL)
## remove Maternity leave

levels(app.tr.x[,mismatch[3]])
levels(app.ts[,mismatch[3]])
table(app.tr$NAME_FAMILY_STATUS)
## remove Unknown

## check near-zero variance dummies.. not entirely sure how to handle yet ==========
app.tr.x.d <- dummy_cols(app.tr.x,remove_first_dummy=TRUE)

table(app.tr.x.d$NAME_INCOME_TYPE_Student,app.tr.y)
target_prop(app.tr.x.d$NAME_INCOME_TYPE_Student,app.tr.y)

table(app.tr.x.d$FLAG_MOBIL,app.tr.y)
target_prop(app.tr.x.d$NAME_INCOME_TYPE_Student,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_2,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_2,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_4,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_4,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_7,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_7,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_10,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_10,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_12,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_12,app.tr.y)

table(app.tr.x.d$FLAG_DOCUMENT_19,app.tr.y)
target_prop(app.tr.x.d$FLAG_DOCUMENT_19,app.tr.y)

#====================================== bureau ======================================
summary(bu)
head(bu)

hist(bu$DAYS_CREDIT,main="Days before current application of CB credit",xlab="DAYS_CREDIT")

hist(log(bu$CREDIT_DAY_OVERDUE),main="Days overdue of CB credit",xlab="log(CREDIT_DAY_OVERDUE)")
## max is 2792 days... bad sign

## how to visualize table?
table(bu$CNT_CREDIT_PROLONG)

hist(bu$DAYS_CREDIT_ENDDATE,main="Days remaining in CB credit, relative to current application",xlab="DAYS_CREDIT_ENDDATE")

hist(bu$AMT_CREDIT_SUM_LIMIT)

summary(bu$AMT_CREDIT_SUM_LIMIT[bu$AMT_CREDIT_SUM_LIMIT>0])
hist(bu$AMT_CREDIT_SUM_LIMIT[bu$AMT_CREDIT_SUM_LIMIT>0 & bu$AMT_CREDIT_SUM_LIMIT<1000],
     main="Distribution of credit limits under $1000",
     xlab="Credit limit")


hist(bu$DAYS_CREDIT_ENDDATE)
hist(bu$DAYS_CREDIT_ENDDATE[bu$DAYS_CREDIT_ENDDATE>0])
summary(bu$DAYS_CREDIT_ENDDATE[bu$DAYS_CREDIT_ENDDATE>0])

#====================================== prev ======================================
qplot(prev_app$NAME_CONTRACT_STATUS,geom="bar",xlab="NAME_CONTRACT_STATUS",ylab="Freq")
hist(prev_app$AMT_ANNUITY)

#====================================== bu_bal ======================================


#============================== engineered features ==============================
load("data/processed/bu_features.Rda")

bu_features.nas <- sort(sapply(bu_features,count_na),decreasing=TRUE)
plt <- barplot(bu_features.nas,names.arg=names(bu_features.nas),
               main="Missingness for bureau features",
               xaxt="n",
               ylab="Freq")
text(x=plt,y=-2,labels=names(bu_features.nas),cex=.5,srt=45,adj=1,xpd=TRUE)
























