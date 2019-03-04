library(glmnet)

source("workflow/functions.R")
load("data/processed/app_clean.Rda")
load("workflow/models/lasso/lasso_baseline.Rda")
       
#================================ unstandardized lasso ================================
tr.x <- model.matrix(~.-SK_ID_CURR-TARGET,app.tr)[,-1]
tr.y <- app.tr$TARGET
ts <- model.matrix(~.-SK_ID_CURR,app.ts)[,-1]

lasso1 <- cv.glmnet(tr.x,tr.y,family="binomial",alpha=1,nfolds=5,type.measure="auc",standardize=FALSE)
plot(lasso1)

pred.lasso1 <- predict(lasso1,ts,s="lambda.1se",type="response")
submission <- data.frame(SK_ID_CURR=app.ts$SK_ID_CURR,TARGET=as.vector(pred.lasso1))
submit(submission)

#============================== lasso, standardized by glmnet ==============================
tr.x <- model.matrix(~.-SK_ID_CURR-TARGET,app.tr)[,-1]
tr.y <- app.tr$TARGET
ts <- model.matrix(~.-SK_ID_CURR,app.ts)[,-1]

lasso2 <- cv.glmnet(tr.x,tr.y,
                    family="binomial",alpha=1,
                    nfolds=5,type.measure="auc") ## standardize=TRUE by default

pred.lasso2 <- predict(lasso2,ts,s="lambda.1se",type="response")
submission <- data.frame(SK_ID_CURR=app.ts$SK_ID_CURR,TARGET=as.vector(pred.lasso2))
submit(submission)

#=============================================================================================== 
save(lasso1,lasso2,file="workflow/models/lasso/lasso_baseline.Rda")
