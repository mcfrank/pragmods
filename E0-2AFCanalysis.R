## analysis of Experiment 1
# matrix is 
# 0 0 1
# 0 1 1
# 1 1 0
library(reshape)
library(plyr)

# rm(list=ls())
raw.data <- read.csv("data/E0-listener-2AFC.csv")
# filter repeats
raw.data <- raw.data[!duplicated(raw.data$WorkerId),]

vars <- names(raw.data)
mvs <- grep("(Input)|(Answer)",vars)
mdata <- melt(raw.data,id.vars="WorkerId",measure.vars=vars[mvs])

# extract the actual answers
targets <- mdata[grep("Referent|choice|level",mdata$variable),]
targets$trial <- 1
targets$trial[grep("Trial2",targets$variable)] <- 2
targets$trial[grep("Trial3",targets$variable)] <- 3
targets$variable <- sub("Trial[1-9]?","",targets$variable)
targets$variable <- sub("Answer.|Input.","",targets$variable)

# recast as what we want
data <- cast(targets,WorkerId + trial ~ variable)
data$target.choice <- data$targetReferent == data$choice
data$dist.choice <- data$distractorReferent == data$choice
data$other.choice <- data$otherReferent == data$choice

# aggregate
listener <- aggregate(cbind(target.choice,dist.choice,other.choice) ~ level, data, mean)

###### COMPARE WITH MODELS ######

source("agents.R")
expt = matrix(
  c(0,   0,   1,
    0,   1,   1,
    1,   1,   0), byrow=T, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('m1', 'm2', 'm3'))) # Column names; messages.

target.referents <- c(3,1,2)
distractor.referents <- c(1,2,3) # referent with the feature, except for level 0
other.referents <- c(2,3,1) # referent without the feature, except for level 0
target.features <- c(1,3,2)

source("agents.R")
fg <- FG(expt)
fg.preds <- fg

for (i in 1:3) { 
  fg.preds[i,] <- c(fg[target.features[i],target.referents[i]],
                    fg[target.features[i],distractor.referents[i]],
                    fg[target.features[i],other.referents[i]])
}


# t(apply(fg.preds * salience[,2:4],2,function(x) {x / sum(x,na.rm=T)}))