## functions that should allow us to do general, model-based data analysis 

## non-general function that will add trial numbers to a melted array
addTrialNums <- function (x) {  
  x$trial <- 1
  x$trial[grep("Trial2",x$variable)] <- 2
  x$trial[grep("Trial3",x$variable)] <- 3
  x$variable <- sub("Trial[1-9]?","",x$variable)   
  return(x)
}

## non-general function that will move input and answer to a factor from melted 
## array
addInputAnswer <- function(x) {
  x$phase <- "input"
  x$phase[grep("Answer",x$variable)] <- "answer"
  x$phase <- factor(x$phase)
  x$variable <- sub("Input.|Answer.","",x$variable)
  return(x)
}

## non-general check number function
addCheckNum <- function(x) {
  x$checkNum <- 1
  x$checkNum[grep("check2",x$variable)] <- 2
  x$checkNum[grep("check3",x$variable)] <- 3
  x$variable <- sub("check[1-9]?","",x$variable)   
  return(x)
}

## clean numeric input that is currently factor or whatever
clean <- function(x) {
  x <- as.numeric(gsub("[^[:alnum:]_]","",as.character(x)))
  x[is.na(x)] <- 0
  return(x)
}

## do check trials from melted data and return whether all of them are correct 
## for a particular trial
checkTrials <- function(mdata) {
  # extract the check trials
  check <- mdata[grep("check.Count",mdata$variable),]
  check <- addTrialNums(check)
  check <- addInputAnswer(check)
  check <- addCheckNum(check)
  check$value <- clean(check$value)
  check.trials <- cast(check, WorkerId + trial + checkNum ~ phase)
  check.trials$correct <- check.trials$input == check.trials$answer
  check.data <- aggregate(correct ~ trial + WorkerId, check.trials, 
                          function(x) {sum(x) == length(x)})
  return(check.data)  
}


## semi-general function that should be updated to read whatever type of data
## we are getting
target <- function(x) {x[paste("bet",x$targetReferent,sep="")]/100}
dist <- function(x) {x[paste("bet",x$distractorReferent,sep="")]/100}
other <- function(x) {x[paste("bet",x$otherReferent,sep="")]/100}

exptAnalysis <- function (filename="E1-listener.csv", type="bet") {
  
  library(reshape)
  library(plyr)
  
  raw.data <- read.csv(paste("data/",filename,sep=""))
  
  # filter repeat subjects
  raw.data <- raw.data[!duplicated(raw.data$WorkerId),]
  
  # first extraction step
  vars <- names(raw.data)
  mvs <- grep("(Input)|(Answer)",vars)
  mdata <- melt(raw.data,id.vars="WorkerId",measure.vars=vars[mvs])
  
  # extract the actual answers
  targets <- mdata[grep("Referent|bet|choice|level",mdata$variable),]
  targets <- addTrialNums(targets)
  targets$variable <- sub("Answer.|Input.","",targets$variable)
  
  # recast as what we want
  data <- cast(targets,WorkerId + trial ~ variable)
  
  # split by whether this is 2AFC or betting
  if (type == "bet") {
    data$bet1 <- clean(data$bet1)
    data$bet2 <- clean(data$bet2)
    data$bet3 <- clean(data$bet3)
  
    # exclude data that do not sum to 100
    data <- data[data$bet1 + data$bet2 + data$bet3 == 100,]
    
    # reorder bets
    data <- merge(data,ddply(data, .(WorkerId,trial), "target"))
    data <- merge(data,ddply(data, .(WorkerId,trial), "dist"))
    data <- merge(data,ddply(data, .(WorkerId,trial), "other"))
  } else if (type == "2AFC") {
    # reorder choices
    data$target <- data$targetReferent == data$choice
    data$dist <- data$distractorReferent == data$choice
    data$other <- data$otherReferent == data$choice
  } 

  # merge in check trials and exclude failures
  check.data <- checkTrials(mdata)
  data <- merge(check.data,data)
  data <- data[data$correct,]
  
  # clean up fields for later merging
  data$worker <- data$WorkerId
  data$expt <- sub(".csv","",filename)
  data$measure <- type
    
  fields <- c("worker","trial","level","expt","measure","target","dist","other")
  small.data <- data[,fields]
  return(small.data)
}