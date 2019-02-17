## http://philipppro.github.io/Random_Forest_in_R/
## https://arxiv.org/pdf/1508.04409v1.pdf
library(parallel)
library(pROC)
library(ranger)

source("workflow/functions.R")

load("app-clean.Rda")
ival15 <- sample(nrow(app.tr),.15*nrow(app.tr))
val <- app.tr[ival15,]
tr <- app.tr[-ival15,]
ts <- app.ts
#==============================  determine number of trees ==============================
## in general, the more trees trained the better; drawback is computation time
    ## goal: choose a reasonable number of trees for random forest that isn't too computationally expensive
    ## save some time during tuning... can increase number of trees later on 
m <- round(sqrt(ncol(app.tr[,-1])))
ncores <- detectCores()

num.trees <- c(10,25,50,75,100,125,150,200,300,400)
auc_score <- rep(NA,length(num.trees))
build_time <- rep(NA,length(num.trees))
for (i in 1:length(num.trees)) {
    n <- num.trees[i]
    build_time[i] <- system.time(rg1 <- ranger(factor(TARGET)~.,data=tr[,-1],
                                                num.trees=n,
                                                mtry = m,
                                                min.node.size=5,
                                                importance="impurity",
                                                probability=TRUE,
                                                replace=TRUE,
                                                num.threads=ncores-2,
                                                verbose=TRUE))[3]
    pred.rg1 <- predict(rg1,val[,-1])
    rg1_roc <- roc(val$TARGET,pred.rg1$predictions[,2])
    auc_score[i] <- as.numeric(rg1_roc$auc)
}

rg1.auc_curve <- plot(num.trees,auc_score,ylim=c(.6,.8),main="Learning curve for rf num.trees",xlab="Number of trees",ylab="Holdout AUC",pch=16) + 
    text(num.trees,auc_score,labels=round(auc_score,3),cex=.75,pos=3) + 
    lines(num.trees,auc_score,col="red")

rg1.time_curve <- plot(num.trees,build_time,main="Time curve for rf num.trees",xlab="Number of trees",ylab="Ranger build time (6 cores)",ylim=c(0,230)) +
    text(num.trees,build_time,labels=round(build_time,1),cex=.75,pos=3) + 
    lines(num.trees,build_time,col="blue")

#======================== random forest, parameters chosen by randomized search ========================
rg2 <- ranger(factor(TARGET)~.,data=tr[,-1],
              num.trees=125,
              mtry=15,
              min.node.size=100,
              importance="impurity",
              probability=TRUE,
              replace=TRUE,
              respect.unordered.factors=TRUE,
              num.threads=4,
              verbose=TRUE)
val.rg2 <- predict(rg2,val[,-1])
(rg2_roc <- roc(val$TARGET,val.rg2$predictions[,2]))

pred.rg2 <- predict(rg2,app.ts[,-1])
submission <- data.frame(SK_ID_CURR=app.ts$SK_ID_CURR,TARGET=as.vector(pred.rg2$predictions[,2]))
submit(submission)
