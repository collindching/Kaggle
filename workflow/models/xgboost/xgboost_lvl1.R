## https://github.com/dmlc/xgboost/tree/master/demo#tutorials
## https://github.com/dmlc/xgboost/blob/master/demo/kaggle-higgs/higgs-train.R
## https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/

## tuning steps:
##   1 tune tree size
##   2 tune sample size
##   3 tune child weight
##   4 tune feature sampling
##   5 decrease learning rate, then get optimum num_round
library(forcats)
library(gridExtra)
library(rBayesianOptimization)
library(reshape2)
library(tidyverse)
library(xgboost)
library(MlBayesOpt)
source("workflow/functions.R")
load("data/processed/final_data.Rda")

## train-validation split
set.seed(2018)
ival15 <- sample(nrow(final.tr),.15*nrow(final.tr))
tr <- final.tr[-ival15,]
val <- final.tr[ival15,] ## this is used after CV rounds
ts <- final.ts

## dummify data frames
tr.x <- model.matrix(~.-TARGET,tr[,-1])[,-1]
tr.y <- tr$TARGET

val.x <- model.matrix(~.-TARGET,val[,-1])[,-1]
val.y <- val$TARGET

ts <- model.matrix(~.,ts[,-1])[,-1]
save(tr.x,tr.y,val.x,val.y,ts,file="workflow/models/xgboost/xgb_data.Rda")

## group data in DMatrices
load("workflow/models/xgboost/xgb_data.Rda")
dtrain <- xgb.DMatrix(data=tr.x,label=tr.y)
dval <- xgb.DMatrix(data=val.x,label=val.y)
dtest <- xgb.DMatrix(data=ts)

## tune tree depth with CV ===============
md <- 2:7
niters <- 200

tr.md_auc <- matrix(NA,niters,length(md))
val.md_auc <- matrix(NA,niters,length(md))

system.time(
    for (i in 1:length(md)) {
        params <- list(max_depth=md[i],eta=.8,
                       objective="binary:logistic",eval_metric="auc")
        
        xgb <- xgb.cv(params=params,
                      data=dtrain,
                      nrounds=niters,
                      nfold=3,stratified=TRUE,
                      nthread=4,
                      early_stopping_rounds=20,
                      maximize=TRUE,
                      verbose=1,print_every_n=5)
    
        stop_len <- length(xgb$evaluation_log$train_auc_mean)
        tr.md_auc[1:stop_len,i] <- xgb$evaluation_log$train_auc_mean
        val.md_auc[1:stop_len,i] <- xgb$evaluation_log$test_auc_mean
    }
)

tr.md_auc <- data.frame(1:niters,tr.md_auc)
val.md_auc <- data.frame(1:niters,val.md_auc)

colnames(tr.md_auc) <- c("iter",md)
colnames(val.md_auc) <- c("iter",md)

tr.md_auc <- melt(tr.md_auc,id.vars="iter",variable.name="max_depth",value.name="train_auc")
val.md_auc <- melt(val.md_auc,id.vars="iter",variable.name="max_depth",value.name="val_auc")
save(tr.md_auc,val.md_auc,file="workflow/models/xgboost/tune_md.Rda")

## validation curves
load("workflow/models/xgboost/tune_md.Rda")
(plt1 <- ggplot() + 
    geom_line(data=tr.md_auc,aes(x=iter, y=train_auc,col="training")) +
    geom_line(data=val.md_auc,aes(x=iter, y=val_auc,col="validation")) +
    facet_wrap(~max_depth,nrow=2,ncol=3) + 
    xlim(c(0,125)) +
    scale_color_manual(name="",values=c("red","darkgreen")) +
    labs(title="Tree depth CV (K=3)",y="mean AUC") + 
    theme_bw() + 
    theme(legend.position="bottom", 
          plot.title=element_text(hjust=0.5),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())) 
## based on this, want to try 4, 5, or 6 for max_depth

(plt2 <- ggplot() + 
    geom_line(data=tr.md_auc,aes(x=iter, y=train_auc,col=max_depth),alpha=.5) +
    geom_line(data=val.md_auc,aes(x=iter, y=val_auc,col=max_depth),linetype="dashed") +
    xlim(c(0,125)) +
    labs(title="CV AUC spread (K=3)",y="mean AUC") + 
    theme_bw() + 
    theme(legend.position="bottom", 
          plot.title=element_text(hjust=0.5),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()))

## for now, set max_depth=6

## roughly tune sampling ====================================
samp_grid <- expand.grid(cs=seq(1/3,1,1/3),ss=seq(1/3,1,1/3))
comb_names <- paste("comb",1:9,sep='')
samp_grid <- data.frame(combination=comb_names,samp_grid)

niters <- 100

tr.samp_auc <- matrix(NA,niters,nrow(samp_grid))
val.samp_auc <- matrix(NA,niters,nrow(samp_grid))

system.time(
    for (i in 1:nrow(samp_grid)) {
        ps <- list(colsample_bytree = samp_grid$cs[i], 
                   subsample        = samp_grid$ss[i],
                   max_depth        = 5,
                   eta              = .75,
                   objective        = "binary:logistic",
                   eval_metric      = "auc") 
        
        xgb <- xgb.cv(params                = ps,
                      data                  = dtrain,
                      nrounds               = niters,
                      nfold                 = 3,
                      stratified            = TRUE,
                      nthread               = 4,
                      early_stopping_rounds = 20,
                      maximize              = TRUE,
                      verbose               = 1,
                      print_every_n         = 3)
        
        stop_len <- length(xgb$evaluation_log$train_auc_mean)
        tr.samp_auc[1:stop_len,i] <- xgb$evaluation_log$train_auc_mean
        val.samp_auc[1:stop_len,i] <- xgb$evaluation_log$test_auc_mean
    }
)

tr.samp_auc <- data.frame(1:niters,tr.samp_auc)
val.samp_auc <- data.frame(1:niters,val.samp_auc)

## graphing
colnames(tr.samp_auc) <- c("iters",comb_names)
colnames(val.samp_auc) <- c("iters",comb_names)

tr.samp_auc <- melt(tr.samp_auc,id.vars="iters",variable.name="combination",value.name="train_auc")
val.samp_auc <- melt(val.samp_auc,id.vars="iters",variable.name="combination",value.name="val_auc")

tr.samp_auc <- inner_join(samp_grid,tr.samp_auc,by="combination") %>%
    select(iters,everything(),-combination)
val.samp_auc <- inner_join(samp_grid,val.samp_auc,by="combination") %>%
    select(iters,everything(),-combination)

tot.samp_auc <- data.frame(tr.samp_auc,val_auc=val.samp_auc$val_auc)
tot.samp_auc$cs <- factor(tot.samp_auc$cs,labels=c("33% vars","66% vars","100% vars"))
tot.samp_auc$ss <- factor(tot.samp_auc$ss,labels=c("33% data","66% data","100% data"))
tot.samp_auc <- melt(tot.samp_auc,id.vars=c("iters","cs","ss"),variable.name="data",value.name="mean_AUC")
save(tot.samp_auc,file="workflow/models/xgboost/tune_samp_rough.Rda")

load("workflow/models/xgboost/tune_samp_rough.Rda")
ggplot(tot.samp_auc) + 
    geom_line(aes(x=iters,y=mean_AUC,col=data)) +
    scale_color_manual(name="",values=c("red","darkgreen"),labels=c("training","validation")) +
    facet_grid(rows=vars(fct_rev(ss)),cols=vars(cs)) +
    scale_x_continuous(breaks=c(0,50),limits=c(0,50)) +
    labs(y="AUC",title="Sampling CV (K=3)") +
    theme_bw() + theme(legend.position="bottom",plot.title=element_text(hjust=0.5))

## tune sampling more precisely ==============================================
## can't use MLR because it doesn't have early stopping with xgb.cv watchlists
## don't want to use handwritten method, too much programming
## MlBayesOpt bad because doesn't have colsample_bytree
## steps: 1) make data, 2) make function to maximize, 3) execute bayesian optimization
## reference: https://ymattu.github.io/MlBayesOpt/articles/MlBayesOpt.html
cv_folds <- KFold(tr.y,nfolds=3,stratified=TRUE,seed=2018)

xgb_cv_bayes.samp <- function(subsample,colsample_bytree) {
    ps <- list(subsample         = subsample,
               colsample_bytree  = colsample_bytree,
               booster           = "gbtree",
               max_depth         = 5,
               eta               = .75,
               objective         = "binary:logistic",
               eval_metric       = "auc")
               
    cv_fn <- xgb.cv(params                = ps,
                    data                  = dtrain,
                    nrounds                = 100,
                    folds                 = cv_folds,
                    prediction            = TRUE,
                    showsd                = TRUE,
                    nthread               = 4,
                    early_stopping_rounds = 10,
                    maximize              = TRUE,
                    verbose               = 0)
    
    list(Score = cv_fn$evaluation_log$test_auc_mean[cv_fn$best_iteration],
         Pred  = cv_fn$pred)
}

bounds <- list(subsample=c(.75,1),
               colsample_bytree=c(.7,1))
system.time(
    bayes_opt.samp <- BayesianOptimization(xgb_cv_bayes.samp,
                                      bounds=bounds,
                                      init_points=3,
                                      n_iter= 15,
                                      acq="ucb",
                                      kappa=2.576,
                                      eps=0,
                                      verbose=TRUE)
)
save(bayes_opt.samp,file="workflow/models/xgboost/tune_samp_bayes.Rda")

## subsample=1, colsample_bytree=.9

## tune min_child_weight =================================
## Bayes optimization
cv_folds <- KFold(tr.y,nfolds=3,stratified=TRUE,seed=2018)

xgb_cv_bayes.weight <- function(min_child_weight) {
    ps <- list(min_child_weight = min_child_weight,
               subsample         = 1,
               colsample_bytree  = .9,
               booster           = "gbtree",
               max_depth         = 5,
               eta               = .75,
               objective         = "binary:logistic",
               eval_metric       = "auc")
    
    cv_fn <- xgb.cv(params                = ps,
                    data                  = dtrain,
                    nrounds                = 100,
                    folds                 = cv_folds,
                    prediction            = TRUE,
                    showsd                = TRUE,
                    nthread               = 4,
                    early_stopping_rounds = 10,
                    maximize              = TRUE,
                    verbose               = 0)
    
    list(Score = cv_fn$evaluation_log$test_auc_mean[cv_fn$best_iteration],
         Pred  = cv_fn$pred)
}

bounds <- list(min_child_weight=c(20L,400L))
system.time(
    bayes_opt.samp <- BayesianOptimization(xgb_cv_bayes.weight,
                                           bounds=bounds,
                                           init_points=5,
                                           n_iter= 10,
                                           acq="ucb",
                                           kappa=2.576,
                                           eps=0,
                                           verbose=TRUE)
)

## step 2: grid-search
weight_try <- data.frame(weight=c(180, 250, 500, 700, 1200, 1400, 2000, 2200,4000),cv_auc=rep(NA))
for (i in 1:nrow(weight_try)) {
    ps <- list(min_child_weight  = weight_try$weight[i],
               subsample         = 1,
               colsample_bytree  = .9,
               booster           = "gbtree",
               max_depth         = 5,
               eta               = .75,
               objective         = "binary:logistic",
               eval_metric       = "auc")
    
    cv_it <- xgb.cv(params                = ps,
                    data                  = dtrain,
                    nrounds                = 200,
                    folds                 = cv_folds,
                    prediction            = TRUE,
                    showsd                = TRUE,
                    nthread               = 4,
                    early_stopping_rounds = 10,
                    maximize              = TRUE,
                    verbose               = 1,
                    print_every_n         = 5)
    
    weight_try$cv_auc[i] <- cv_it$evaluation_log$test_auc_mean[cv_it$best_iteration]
}

## conclusions: 
##     max_depth should be more than 5: try 6,7,8,9
##     guessing that I want min_child_weight roughly between 600 and 2000

## tune gamma ================================================
## raise max_depth to 6 since it looks like we're underfitting
cv_folds <- KFold(tr.y,nfolds=3,stratified=TRUE,seed=2018)

gamma_try <- data.frame(gamma=c(.1,.3,1,3,10),cv_auc=rep(NA))
for (i in 1:nrow(gamma_try)) {
    ps <- list(gamma = gamma_try$gamma[i],
               min_child_weight  = 1000,
               subsample         = 1,
               colsample_bytree  = .9,
               booster           = "gbtree",
               max_depth         = 6,
               eta               = .75,
               objective         = "binary:logistic",
               eval_metric       = "auc")
    
    cv_it <- xgb.cv(params                = ps,
                    data                  = dtrain,
                    nrounds                = 200,
                    folds                 = cv_folds,
                    prediction            = TRUE,
                    showsd                = TRUE,
                    nthread               = 4,
                    early_stopping_rounds = 10,
                    maximize              = TRUE,
                    verbose               = 1,
                    print_every_n         = 5)
    
    gamma_try$cv_auc[i] <- cv_it$evaluation_log$test_auc_mean[cv_it$best_iteration]
}
## conclusions:
##     either I tuned very well or I'm underfitting... try max_depth at 7 as well
##     gamma between 0 and .3

## pulling eta back ===============
ps <- list(gamma = .1,
           min_child_weight = 1000,
           subsample = 1,
           colsample_bytree = .9, 
           max_depth = 6,
           booster = "gbtree",
           eta = .1,
           objective = "binary:logistic",
           eval_metric="auc") 

xgb <- xgb.cv(params=ps,
              data=dtrain,
              nrounds=400,
              nfold=3,
              stratified=TRUE,
              verbose=1,
              print_every_n=5,
              early_stopping_rounds=10,
              maximize=TRUE)
## gives optimal test auc of .772!

## check model ================================
ps <- list(gamma = .1,
           min_child_weight = 1000,
           subsample = 1,
           colsample_bytree = .9, 
           max_depth = 6,
           booster = "gbtree",
           eta = .1,
           objective = "binary:logistic",
           eval_metric="auc") 

xgb_final <- xgb.train(params=ps,
                       data=dtrain,
                       nrounds=194,
                       verbose=1,
                       print_every_n=5)

## using predictions using full dataset ====
tr <- final.tr
ts <- final.ts

tr.x <- model.matrix(~.-TARGET,tr[,-1])[,-1]
tr.y <- tr$TARGET
ts <- model.matrix(~.,ts[,-1])[,-1]

dtrain <- xgb.DMatrix(data=tr.x,label=tr.y)
dtest <- xgb.DMatrix(data=ts)

ps <- list(gamma = .1,
           min_child_weight = 1000,
           subsample = 1,
           colsample_bytree = .9, 
           max_depth = 6,
           booster = "gbtree",
           eta = .1,
           objective = "binary:logistic",
           eval_metric="auc") 

xgb <- xgb.train(params=ps,
                 data=dtrain,
                 nrounds=360)

save(xgb,file="workflow/models/xgboost/xgb_final.Rda")
load("workflow/models/xgboost/xgb_final.Rda")

## make predictions
pred.xgb <- predict(xgb,dtest)
submission <- data.frame(SK_ID_CURR=final.ts$SK_ID_CURR,TARGET=as.vector(pred.xgb))
submit(submission)

## get variable importance
importance_matrix <- xgb.importance(model=xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
