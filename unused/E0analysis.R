## analysis of Experiment 0
# matrix is 
# 0 0 0
# 0 1 0
# 1 1 0
library(reshape)
library(plyr)

# rm(list=ls())
raw.data <- read.csv("data/E0-listener.csv")
# filter repeats
raw.data <- raw.data[!duplicated(raw.data$WorkerId),]

vars <- names(raw.data)
mvs <- grep("(Input)|(Answer)",vars)
mdata <- melt(raw.data,id.vars="WorkerId",measure.vars=vars[mvs])

# extract the actual answers
targets <- mdata[grep("Referent|bet|level",mdata$variable),]
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

sum.100 <- (data$bet1 + data$bet2 + data$bet3) == 100
data <- data[sum.100,]

target.bet <- function(x) {x[paste("bet",x$targetReferent,sep="")]}
dist.bet <- function(x) {x[paste("bet",x$distractorReferent,sep="")]}
other.bet <- function(x) {x[paste("bet",x$otherReferent,sep="")]}

data <- merge(data,ddply(data, .(WorkerId,trial), "target.bet"))
data <- merge(data,ddply(data, .(WorkerId,trial), "dist.bet"))
data <- merge(data,ddply(data, .(WorkerId,trial), "other.bet"))

# aggregate
sem <- function(x) {sd(x) / sqrt(length(x))}
aggregate(target.bet ~ level, data, sem)

listener <- aggregate(cbind(target.bet,dist.bet,other.bet) ~ level, data, mean)

###### COMPARE WITH MODELS ######

source("agents.R")
expt = matrix(
  c(0,   0,
    0,   1,
    1,   1), byrow=T, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('m1', 'm2'))) # Column names; messages.

levels <- c(0,1)
target.referents <- c(3,2)
distractor.referents <- c(2,3) # referent with the feature, except for level 0
other.referents <- c(1,1) # referent without the feature, except for level 0
target.features <- c(1,2)

fg <- FG(expt)
fg.preds <- fg

for (i in 1:2) { 
  fg.preds[i,] <- c(fg[target.features[i],target.referents[i]],
                    fg[target.features[i],distractor.referents[i]],
                    fg[target.features[i],other.referents[i]])
}


t(apply(fg.preds * salience[,2:4],1,function(x) {x / sum(x,na.rm=T)}))
