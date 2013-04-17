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
source("getExptPreds.R")
source("plottingHelper.R")

## consider using only first trial
trial1only <- T

########################################################
#### GATHER UP ALL EXPERIMENTAL DATA ####

expts <- c("E0-listener-bet","E0-salience-bet",
           "E1-listener-bet","E1-salience-bet",
           "E2-listener-bet","E2-salience-bet",           
           "E0S_targ-listener-bet","E0S_targ-salience-bet",
           "E0S_dist-listener-bet","E0S_dist-salience-bet",
           "E1S_targ-listener-bet","E1S_targ-salience-bet",
           "E1S_dist-listener-bet","E1S_dist-salience-bet",
           "E0S_targ_phrase-listener-bet","E0S_targ_phrase-salience-bet",
           "E0S_dist_phrase-listener-bet","E0S_dist_phrase-salience-bet")
# "E3-listener-bet","E3-salience-bet",
#            "E0S_dist_phrase_as-listener-bet",
#            "E0S_targ_phrase_as-listener-bet")
          # targ phrase as salience bet is fake

# excluded: "E3-listener-bet","E3-salience-bet"

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

theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}

if (trial1only) {
  agg.data <- aggregate(cbind(target,dist,other) ~ trial + level + expt + condition,data,mean)
  agg.data$target.cil <- aggregate(target ~ trial + level + expt + condition,data,ci.low)$target
  agg.data$target.cih <- aggregate(target ~ trial + level + expt + condition,data,ci.high)$target

  listener <- subset(agg.data,condition=="listener" & trial == 1)
  salience <- subset(agg.data,condition=="salience" & trial == 1)
} else {
  agg.data <- aggregate(cbind(target,dist,other) ~ level + expt + condition,data,mean)
  agg.data$target.cil <- aggregate(target ~ level + expt + condition,data,ci.low)$target
  agg.data$target.cih <- aggregate(target ~ level + expt + condition,data,ci.high)$target
  
  listener <- subset(agg.data,condition=="listener")
  salience <- subset(agg.data,condition=="salience")
}


## GET EXPERIMENT PREDICTIONS 
## (FACTORED FOR CLARITY)

models <- c("L0","LS","LSL","LSLS","LSLSLS")
preds <- getExptPreds("data/experiment_conditions_all.csv",salience,
                      models=models)

########################################################
#### VISUALIZE RESULTS VS. PREDICTIONS ###

# merge dataset
all.data <- merge(preds,listener,by.x=c("expt","level"),by.y=c("expt","level"))
all.data$model <- factor(all.data$model,levels=models)
all.data$level <- factor(all.data$level)

# merge in salience errors
salience$sal.cil <- salience$target.cil
salience$sal.cih <- salience$target.cih
salience.err <- salience[,c("level","expt","sal.cil","sal.cih")]
all.data <- merge(all.data,salience.err,by.x=c("expt","level"),by.y=c("expt","level"))
all.data[all.data$bayesian=="No salience","sal.cil"] <- 0  # zero these out for no salience conditions
all.data[all.data$bayesian=="No salience","sal.cih"] <- 0


# actually plot, plus more ggplot madness
q <- qplot(target.pred,target,ymin=target-target.cil,ymax=target+target.cih,
           data=all.data,colour=expt,geom="pointrange",shape=level,
           xlim=c(0,1),ylim=c(0,1),xlab="Model prediction",ylab="Experimental data") + 
             geom_abline(intercept=0,slope=1,lty=2) + 
             facet_grid(bayesian~model) +
             scale_colour_manual(name="Experiment",values=c("red","blue","green","orange","cyan","pink","yellow","gray","purple","black")) + 
             scale_shape_manual(name="Inference level",values=c(15,16,17,18)) +     
             geom_segment(aes(x=target.pred-sal.cil,y=target,xend=target.pred+sal.cih,yend=target)) +
        theme_bw() + 
        plot.style

quartz()
gt <- ggplot_gtable(ggplot_build(q))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

########################################################
#### STATISTICS AND SUMMARIES

# some stats for comparison

corr = function(x) {
	round(cor.test(x$target.pred,x$target)$estimate,3)}
rmse = function(x) {
  round(sqrt(mean((x$target.pred-x$target)^2)),2)}


corrs <- ddply(all.data, .(bayesian,model), "corr")
corrs.nozero <- ddply(all.data[all.data$level != 0,], .(bayesian,model), "corr")

rmses <- ddply(all.data, .(bayesian,model), "rmse")
rmses.nozero <- ddply(all.data[all.data$level != 0,], .(bayesian,model), "rmse")

quartz()
qplot(model,corr,colour=bayesian,data=corrs,geom=c("line"), group=bayesian) +
  theme_bw() + plot.style


########################################################
#### TRIAL EFFECT

agg.data.trial <- aggregate(cbind(target,dist,other) ~ trial + level + expt + condition,data,mean)
agg.data.trial$target.cil <- aggregate(target ~ trial + level + expt + condition,data,ci.low)$target
agg.data.trial$target.cih <- aggregate(target ~ trial + level + expt + condition,data,ci.high)$target

listener.trial <- subset(agg.data.trial,condition=="listener" & level==1)
listener.trial$trial <- factor(listener.trial$trial)

quartz()
qplot(trial,target,facets=.~expt,data=listener.trial,
      ymin=target-target.cil,ymax=target+target.cih,
      ylim=c(0,1),
      geom="pointrange") + 
  theme_bw() + plot.style