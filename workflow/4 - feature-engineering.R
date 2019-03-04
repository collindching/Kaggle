library(tidyverse)
source("workflow/functions.R")

summary_fns <- funs(sum(.,na.rm=TRUE),
                    mean(.,na.rm=TRUE),
                    min(.),
                    max(.))

#====================================== app ======================================
## https://medium.com/comet-ml/manual-feature-engineering-kaggle-home-credit-db1362d683c4
names(app.tot)

## unique features to be interested in
    ## credit-to-income ratio
    ## credit-to-goods ratio
    ## debt service coverage ratio
app_features <- app.tot %>% 
    transmute(SK_ID_CURR = SK_ID_CURR,
            credit_to_income = AMT_CREDIT/AMT_INCOME_TOTAL,
            goods_to_income = AMT_GOODS_PRICE/AMT_INCOME_TOTAL,
            credit_to_goods = AMT_CREDIT/AMT_GOODS_PRICE,
            debt_service_coverage_ratio = AMT_INCOME_TOTAL/(12*AMT_ANNUITY), 
            income_per_child = AMT_INCOME_TOTAL/(CNT_CHILDREN+1),
            income_per_person = AMT_INCOME_TOTAL/CNT_FAM_MEMBERS,
            employment_to_age = DAYS_EMPLOYED/DAYS_BIRTH,
            payment_rate = AMT_ANNUITY/AMT_CREDIT)

save(app_features,file="data/processed/app_features.Rda")

#====================================== bureau ======================================
bu$AMT_CREDIT_SUM_LIMIT[bu$AMT_CREDIT_SUM_LIMIT<=0] <- NA

bu_features1 <- bu %>% 
    mutate(credit_utilization=AMT_CREDIT_SUM_DEBT/AMT_CREDIT_SUM_LIMIT) %>%
    select(-SK_ID_BUREAU) %>%
    group_by(SK_ID_CURR) %>%
    summarize_if(is.numeric,summary_fns) %>%
    as.data.frame()

bu_features2 <- bu %>%
    group_by(SK_ID_CURR) %>%
    summarize(outstanding_cb_credits_ct=sum(DAYS_CREDIT_ENDDATE>0,na.rm=TRUE)) %>%
    as.data.frame()

bu_features <- inner_join(bu_features1,bu_features2,by="SK_ID_CURR")
names(bu_features)

save(bu_features,file="data/processed/bu_features.Rda")

#====================================== prev ======================================
prev_app$AMT_GOODS_PRICE[prev_app$AMT_GOODS_PRICE==0] <- NA
prev_app$AMT_APPLICATION[prev_app$AMT_APPLICATION==0] <- NA
    
prev_features1 <- prev_app %>% 
    select(-SK_ID_PREV) %>%
    mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING==365243,NA,DAYS_FIRST_DRAWING),
           DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE==365243,NA,DAYS_FIRST_DUE),
           DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION==365243,NA,DAYS_LAST_DUE),
           DAYS_LAST_DUE == ifelse(DAYS_LAST_DUE==365243,NA,DAYS_LAST_DUE),
           DAYS_TERMINATION = ifelse(DAYS_TERMINATION==365243,NA,DAYS_TERMINATION),
           ask_to_goods = AMT_APPLICATION/AMT_GOODS_PRICE,
           credit_to_goods = AMT_CREDIT/AMT_GOODS_PRICE,
           credit_to_ask = AMT_CREDIT/AMT_APPLICATION,
           payment_rate = AMT_ANNUITY/AMT_CREDIT,) %>%
    group_by(SK_ID_CURR) %>%
    summarize_if(is.numeric,summary_fns) %>%
    as.data.frame()
    
prev_features2 <- prev_app %>%
    group_by(SK_ID_CURR) %>%
    summarize(prev_apps_ct = n(),
              approval_rate = sum(NAME_CONTRACT_STATUS=="Approved")/
                  (sum(NAME_CONTRACT_STATUS=="Approved")+sum(NAME_CONTRACT_STATUS=="Refused")))

prev_features <- inner_join(prev_features1,prev_features2,by="SK_ID_CURR")
rm(prev_features1,prev_features2)
names(prev_features)[2:ncol(prev_features)] <- paste("prev",names(prev_features)[2:ncol(prev_features)],sep=".")

summary(prev_features)
              
save(prev_features,file="data/processed/prev_features.Rda")