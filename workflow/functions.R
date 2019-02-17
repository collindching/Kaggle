## http://stcorp.nl/R_course/tutorial_code_organisation.html
## https://stackoverflow.com/questions/45676745/how-to-calculate-the-auc-value-for-a-ranger-rf-model
library(ggplot2)

is_binary = function(x) {
    if (all(x %in% 0:1)) return(TRUE)
    else return(FALSE)
}

impute_median <- function(x,colmed=NULL) {
    if (is.null(colmed))
        colmed <- median(x,na.rm=TRUE)
    
    if (any(is.na(x)))
        x[is.na(x)] <- colmed
    
    return(x)
}

## counts number of NAs
    ## input: vector
    ## output: total NAs in vector
count_na <- function(x) return(sum(is.na(x)))

bake_pie <- function(x,title="") pie(table(x),main=title)

submit <- function(x) {
    write.csv(x,file="submit.csv",row.names=FALSE)
}

target_prop <- function(x,y) {
    round(prop.table(table(x,y),margin=1),3)
}

auc <- function( scores, lbls ) {
    stopifnot( length(scores) == length(lbls) )
    jp <- which( lbls > 0 ); np <- length( jp )
    jn <- which( lbls <= 0); nn <- length( jn )
    s0 <- sum( rank(scores)[jp] )
    (s0 - np*(np+1) / 2) / (np*nn)
}   

cleanx <- theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

no_gridlines <- theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())

