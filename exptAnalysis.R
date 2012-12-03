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

########################################################
#### GATHER UP ALL EXPERIMENTAL DATA ####

expts <- c("E0-listener-bet","E0-salience-bet","E1-listener-bet","E1-salience-bet",
           "E2-listener-bet","E2-salience-bet","E3-listener-bet","E3-salience-bet")

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

agg.data <- aggregate(cbind(target,dist,other) ~ level + expt + condition,data,mean)
agg.data$target.cil <- aggregate(target ~ level + expt + condition,data,ci.low)$target
agg.data$target.cih <- aggregate(target ~ level + expt + condition,data,ci.high)$target

listener <- subset(agg.data,condition=="listener")
salience <- subset(agg.data,condition=="salience")

agg.data.trial <- aggregate(cbind(target,dist,other) ~ trial + level + expt + condition,data,mean)

########################################################
#### GATHER TOGETHER PREDICTIONS FOR ALL EXPERIMENTS ###

es <- read.csv("data/experiment_conditions.csv")

# note I added these to agents.R
models <- c("L_S0","FG","L_S_L_S_L_S0","L_S_L_S_L_S_L_S0")
row.names <- c('r1','r2','r3','r4')
# big loop to run models on various datapoints of various experiments
preds <- data.frame()
for (b in 0:1) {
  for (m in 1:length(models)) {
    this.pred <- data.frame()
    
    for (i in 1:length(es$expt)) {
      e <- es[i,]
      
      # recover the experiment matrix from a string (messy)
      mat.str <- as.numeric(gsub("[^[:alnum:]_]","",
                             strsplit(as.character(e$matrix),"/")[[1]]))
      matrix <- matrix(mat.str,
                       nrow=e$nrows,byrow=T,dimname=list(row.names[1:e$nrows]))
      
      sal <- as.numeric(salience[salience$level==e$level & salience$expt==e$expt,c("target","dist","other")])
  
      # reorder these from tdo to matrix ordering
      if (e$nrows==4) {sal[4] <- 1 - sum(sal)}  # recover last if there are 4
      ord <- c(e$target.referent,e$distractor.referent,e$other.referent)
      if (e$nrows==4) {ord[4] <- setdiff(1:4,ord)}
      sal[ord] <- sal
      
      # unordered predictions
      if (b==0) {
        uop <- eval(parse(text=paste(models[m],"(matrix)",sep="")))
        this.pred[i,"bayesian"] <- "No salience"
      } else {
        uop <- eval(parse(text=paste(models[m],"(matrix,prior=sal)",sep="")))
        this.pred[i,"bayesian"] <- "With salience"
      }
      
      # reorder these to tdo ordering
      pred <- c(uop[e$target.feature,e$target.referent],
                uop[e$target.feature,e$distractor.referent],
                uop[e$target.feature,e$other.referent])
      
      this.pred[i,"model"] <- models[m]
      this.pred[i,"expt"] <- e$expt
      this.pred[i,"level"] <- e$level
      this.pred[i,"target.pred"] <- pred[1]
      this.pred[i,"dist.pred"] <- pred[2]
      this.pred[i,"other.pred"] <- pred[3]
    }  
    preds <- rbind.fill(preds,this.pred)
  }
}
########################################################
#### VISUALIZE RESULTS VS. PREDICTIONS ###

# merge dataset
all.data <- merge(preds,listener,by.x=c("expt","level"),by.y=c("expt","level"))

# merge in salience errors
salience$sal.cil <- salience$target.cil
salience$sal.cih <- salience$target.cih
salience.err <- salience[,c("level","expt","sal.cil","sal.cih")]
all.data <- merge(all.data,salience.err,by.x=c("expt","level"),by.y=c("expt","level"))
all.data[all.data$bayesian=="No salience","sal.cil"] <- 0  # zero these out for no salience conditions
all.data[all.data$bayesian=="No salience","sal.cih"] <- 0

# lots of ggplot madness
plot.style <- opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),
                   axis.line = theme_segment(colour="black",size=.5),
                   axis.ticks = theme_segment(size=.5),
                   axis.title.x = theme_text(vjust=-.5),
                   axis.title.y = theme_text(angle=90,vjust=0.25),
                   panel.margin = unit(1.5,"lines"))

all.data$model <- factor(all.data$model,levels=models)
all.data$level <- factor(all.data$level)

# actually plot, plus more ggplot madness
quartz()
q <- qplot(target.pred,target,ymin=target-target.cil,ymax=target+target.cih,
           data=all.data,colour=expt,geom="pointrange",shape=level,
           xlim=c(0,1),ylim=c(0,1),xlab="Model prediction",ylab="Experimental data") + 
             geom_abline(intercept=0,slope=1,lty=2) + 
             facet_grid(bayesian~model) +
             scale_colour_manual(name="Experiment",values=c("red","blue","green","orange")) + 
             scale_shape_manual(name="Inference level",values=c(15,16,17,18)) +     
             geom_segment(aes(x=target.pred-sal.cil,y=target,xend=target.pred+sal.cih,yend=target)) +
        theme_bw() + 
        plot.style + 
        expand_limits(x = 0, y = 0) + 
        scale_x_continuous(expand = c(0, 0)) + 
        scale_y_continuous(expand = c(0, 0))       

gt <- ggplot_gtable(ggplot_build(q))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

# some stats for comparison

corr = function(x){
	c(round(cor.test(x$target.pred,x$target)$estimate,3),
	  )}
corrs <- ddply(all.data, .(bayesian,model), "corr")

## just plot FG
qplot(target.pred,target,ymin=target-target.cil,ymax=target+target.cih,
           data=all.data[all.data$bayesian=="With salience" & all.data$model=="FG",],
           colour=expt,geom="pointrange",shape=level,
           xlim=c(0,1),ylim=c(0,1),xlab="Model prediction",ylab="Experimental data") + 
             geom_abline(intercept=0,slope=1,lty=2) + 
             scale_colour_manual(name="Experiment",values=c("red","blue","green","orange")) + 
             scale_shape_manual(name="Inference level",values=c(15,16,17,18)) +     
             geom_segment(aes(x=target.pred-sal.cil,y=target,xend=target.pred+sal.cih,yend=target)) +
        theme_bw() + 
        plot.style + 
        expand_limits(x = 0, y = 0) + 
        scale_x_continuous(expand = c(0, 0)) + 
        scale_y_continuous(expand = c(0, 0))  
gt <- ggplot_gtable(ggplot_build(q))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)


### plot correlations
plot(1:4,corrs$corr[5:8],xlab="Level of recursion",ylab="Correlation coefficient",
	ylim=c(.7,1),yaxp=c(.70,1,3),xaxp=c(1,4,3),type="b",bty="n")