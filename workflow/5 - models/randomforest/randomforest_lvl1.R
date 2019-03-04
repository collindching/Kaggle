library(parallel)
library(pROC)
library(ranger)
library(Rborist)
library(tuneRanger)
source("workflow/functions.R")

load("data/processed/final_data.Rda")
## save 15% of training data for validation
ival15 <- sample(nrow(final.tr),.15*nrow(final.tr))
tr <- final.tr[-ival15,]
val <- final.tr[ival15,]
ts <- final.ts

## tune mtry and leaf size =====================
mtry = seq(4,16,2)
auc_val <- rep(NA,length(mtry))
for (i in 1:length(mtry)) {
    rg3 <- ranger(factor(TARGET)~.,data=tr[,-1],
                  num.trees=200,
                  mtry=mtry[i],
                  min.node.size=100,
                  importance="impurity",
                  probability=TRUE,
                  replace=TRUE,
                  respect.unordered.factors=TRUE,
                  num.threads=4,
                  verbose=TRUE)
    
    pred.rg3 <- predict(rg3,val[,-1])
    rg3_roc <- roc(val$TARGET,pred.rg3$predictions[,2])
    auc_val[i] <- as.numeric(rg3_roc$auc)
}

plot(mtry,auc_score,main="mtry validation curve")

min.node.size = seq(50,250,25)
auc_train <- rep(NA,length(min.node.size))
auc_val <- rep(NA,length(min.node.size))

for (i in 1:length(auc_train)) {
    rg3 <- ranger(factor(TARGET)~.,data=tr[,-1],
                  num.trees=200,
                  mtry=10,
                  min.node.size=min.node.sizej[i],
                  importance="impurity",
                  probability=TRUE,
                  replace=TRUE,
                  respect.unordered.factors=TRUE,
                  num.threads=4,
                  verbose=TRUE)
    auc_trainj[i] <- as.numeric(roc(tr$TARGET,rg3$predictions[,2])$auc)
    pred.rg3 <- predict(rg3,val[,-1])
    auc_valj[i] <- as.numeric(roc(val$TARGET,pred.rg3$predictions[,2])$auc)
}

plot(min.node.size,auc_train,type='l',ylim=c(.72,.75),col="blue",ylab="auc",main="Learning curves for min.node.size")
lines(min.node.size,auc_val,col="darkgreen")
legend("bottomright",legend=c("Train","Validation"),col=c("blue","darkgreen"),lty=1)

## ranger lvl 1 model ===============================
system.time(
    rg <- ranger(factor(TARGET)~.,data=final.tr[,-1],
                 num.trees=200,
                 mtry=10,
                 min.node.size=250,
                 importance="impurity",
                 probability=TRUE,
                 replace=TRUE,
                 respect.unordered.factors=TRUE,
                 num.threads=4,
                 verbose=TRUE)
)
save(rg,file="workflow/models/randomforest/randomforest_lvl1.Rda")

load("workflow/models/randomforest/randomforest_lvl1.Rda")
pred.rg <- predict(rg,final.ts[,-1])
submission <- data.frame(SK_ID_CURR=final.ts$SK_ID_CURR,TARGET=as.vector(pred.rg$predictions[,2]))
submit(submission)
