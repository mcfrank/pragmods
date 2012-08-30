## general analysis code
library(reshape)
library(ggplot2)
library(bootstrap)
library(grid)

rm(list=ls())

source("util.R")
source("exptHelper.R")
source("agents.R")
source("IBR.R")

expts <- c("E0-listener-2AFC","E0-salience-2AFC")

data <- data.frame()
for (e in expts) {
  ename <- paste(e,".csv",sep="")
  
  # set type
  if (length(grep("2AFC",e))==1) {type="2AFC"} else {type="bet"}
  edata <- exptAnalysis(filename=ename,type=type)
  data <- rbind.fill(data,edata)
}

# aggregate and clean up for comparison
data[,c("expt","condition","measure")] <- colsplit(data$expt,"-",c("expt","condition","measure"))
agg.data <- aggregate(cbind(target,dist,other) ~ level + expt + condition,data,mean)

theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}

agg.data$target.cil <- aggregate(target ~ level + expt + condition,data,ci.low)$target
agg.data$target.cih <- aggregate(target ~ level + expt + condition,data,ci.high)$target

listener <- subset(agg.data,condition=="listener")
salience <- subset(agg.data,condition=="salience")