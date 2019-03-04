library(caret)
library(glmnet)
library(parallel)
library(doParallel)

source("workflow/functions.R")
load("data/processed/final_data.Rda")
#load("workflow/models/lasso/lasso_final.Rda")

## level 1 lasso logistic regression =============
tr.x <- model.matrix(~.-TARGET,final.tr[,-1])[,-1]
tr.y <- final.tr$TARGET
ts <- model.matrix(~.,final.ts[,-1])[,-1]

cl <- makeCluster(detectCores()-4)
registerDoParallel(cl)
system.time(
    lasso <- cv.glmnet(tr.x,tr.y,family="binomial",
                       alpha=1,nfolds=5,type.measure="auc",
                       parallel=TRUE)
)
stopCluster(cl)
save(lasso,file="workflow/models/lasso/lasso_lvl1.Rda")

plot(lasso)
pred.lasso <- predict(lasso,ts,s="lambda.1se",type="response")
coef(lasso,c="lambda.1se")
submission <- data.frame(SK_ID_CURR=final.ts$SK_ID_CURR,TARGET=as.vector(pred.lasso))
submit(submission)
