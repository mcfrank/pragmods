library(binom)
library(ggplot2)
library(stringr)
library(lme4)

rm(list=ls())

ci.l <- function(x) {binom.confint(sum(x),length(x),method="bayes")$lower}
ci.h <- function(x) {binom.confint(sum(x),length(x),method="bayes")$upper}

## read turk data
d1 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v2.results.tsv",
           sep="\t",header=TRUE)
d1$level <- "L1"
d2 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v3.results.tsv",
                 sep="\t",header=TRUE)
d2$level <- "L2"
d <- rbind(d1,d2)
d$level <- factor(d$level)

## exclude check false
d <- subset(d, Answer.name_check_correct == "\"TRUE\"")
d$item <- factor(str_replace_all(as.character(d$Answer.item),"\"",""))
d$cond <- factor(str_replace_all(as.character(d$Answer.word_condition),"\"",""))
d$correct <-  d$Answer.choice_correct == "\"TRUE\""
d$freq <- d$Answer.target_freq    
d$pos <- factor(str_replace_all(as.character(d$Answer.target_position),"\"",""))



lmer(correct ~ cond * freq * level + (1|item), 
    data=d, family="binomial")

## plot and analyze
f <- correct ~ cond + freq + level #+ item

ms <- aggregate(f, d, mean)
ms$n <- aggregate(f, d, length)$correct
ms$cil <- aggregate(f, d, ci.l)$correct
ms$cih <- aggregate(f, d, ci.h)$correct

## add model
mod <- subset(ms,cond=="salience")
l1 <- mod$level == "L1"
l2 <- mod$level == "L2"
mod$cond <- "L0"
mods <- c("LS","LSL","LSLS")
l1a <- c(.66,.75,.8)
l2a <- c(.5,.6,.64)
  
all.mods <- mod
for (i in 1:length(mods)) {
  newmod <- subset(ms,cond=="salience")
  newmod$cond <- mods[i]
  
  l1 <- newmod$level == "L1"
  l2 <- newmod$level == "L2"
  newmod$correct[l1] <- (mod$correct[l1] * l1a[i]) / 
    ((mod$correct[l1] * l1a[i]) + ((1-mod$correct[l1]) * (1-l1a[i])))

  newmod$correct[l2] <- (mod$correct[l2] * l2a[i]) / 
    ((mod$correct[l2] * l2a[i]) + ((1-mod$correct[l2]) * (1-l2a[i])))
  all.mods <- rbind(all.mods,newmod)
}


all.mods <- subset(all.mods,cond!="L0")
all.mods$cond <- factor(all.mods$cond)

## PLOT IT
pd <- position_dodge(.02)

quartz()
qplot(freq, correct, colour = cond, facets=level~.,#.item,
      data=ms,position=pd,geom="pointrange",
      ylim=c(0,1),xlim=c(0,1),ymin=cil, ymax=cih,
      xlab="Target Frequency",ylab="Proportion Target Judgments") + 
  geom_linerange(aes(),position=pd) +
  geom_smooth(data=all.mods,aes(x=freq,y=correct,col=cond), lty=2, se=FALSE, span=2) + 
  geom_smooth(se=FALSE,span=2) + 
  theme_bw()