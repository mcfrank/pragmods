## analysis of Experiment 1
# matrix is 
# 0 0 1
# 0 1 1
# 1 1 0
library(reshape)
library(plyr)

rm(list=ls())
raw.data <- read.csv("data/E1-salience.csv")

vars <- names(raw.data)
mvs <- grep("(Input)|(Answer)",vars)
mdata <- melt(data,id.vars="WorkerId",measure.vars=vars[mvs])

# extract the actual answers
targets <- mdata[grep("target|bet|level",mdata$variable),]
targets$trial <- 1
targets$trial[grep("Trial2",targets$variable)] <- 2
targets$trial[grep("Trial3",targets$variable)] <- 3
targets$variable <- sub("Trial[1-9]?","",targets$variable)
targets$variable <- sub("Answer.|Input.","",targets$variable)

# recast as what we want
data <- cast(targets,WorkerId + trial ~ variable)
clean.bets <- function(x) {
  x <- as.numeric(gsub("[^[:alnum:]_]","",as.character(x)))
  x[is.na(x)] <- 0
  return(x)
}
data$bet1 <- clean.bets(data$bet1)
data$bet2 <- clean.bets(data$bet2)
data$bet3 <- clean.bets(data$bet3)

target.bet <- function(x) {x[paste("bet",x$targetReferent,sep="")]}
data <- merge(data,ddply(data, .(WorkerId,trial), "target.bet"))

# aggregate
sem <- function(x) {sd(x) / sqrt(length(x))}
aggregate(target.bet ~ level, data, mean)
aggregate(target.bet ~ level, data, sem)