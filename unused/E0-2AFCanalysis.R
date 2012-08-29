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