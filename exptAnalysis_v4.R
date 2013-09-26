library(binom)
library(ggplot2)
library(stringr)
library(lme4)

rm(list=ls())

############## PREAMBLE ############
source("tests.R")
source("~/Projects/R/mcf.useful.R")
source("agents.R")

get.preds <- function(m,d,eps=.001) {
  prior <- array()
  prior[1] <- mean(d$choice == "foil")
  prior[2] <- mean(d$choice == "target")
  prior[3] <- mean(d$choice == "logical")
  
  # smooth prior with eps
  prior <- (prior + eps) / (sum(prior) + (length(prior) * eps))
#   print(prior)
  #prior <- c(.333,.333,.333)
  
  preds <- array()
  preds[1] <- L0(m,prior)["glasses","r2"]
  preds[2] <- LS(m,prior)["glasses","r2"]
  preds[3] <- LSL(m,prior)["glasses","r2"]
  preds[4] <- LSLS(m,prior)["glasses","r2"]
  return(preds)
}

############## READ IN DATA AND CLEAN  ############
d1 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v4.results.tsv",
           sep="\t",header=TRUE)
d1$level <- "L1"
d2 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v5.results.tsv",
                 sep="\t",header=TRUE)
d2$level <- "L2"
d3 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v6.results.tsv",
                 sep="\t",header=TRUE)
d3$level <- "L1"
d4 <- read.table("~/Projects/Pragmatics/pragmods/pragmods_js/pragmods_v7.results.tsv",
                 sep="\t",header=TRUE)
d4$level <- "L2"
d <- rbind(d1,d2,d3,d4)
d$level <- factor(d$level)

## exclude check false
d$item <- factor(str_replace_all(as.character(d$Answer.item),"\"",""))
d$choice <- factor(str_replace_all(as.character(d$Answer.choice),"\"",""))
d$cond <- factor(str_replace_all(as.character(d$Answer.word_condition),"\"",""))
d$correct <-  d$Answer.choice_correct == "\"TRUE\""
d$freq <- d$Answer.target_freq    
d$pos <- factor(str_replace_all(as.character(d$Answer.target_position),"\"",""))
d$mc.targ <- as.numeric(str_replace_all(as.character(d$Answer.manip_check_target),"\"",""))
d$mc.dist <- as.numeric(str_replace_all(as.character(d$Answer.manip_check_dist),"\"",""))


############ EXCLUSIONS ############
d <- subset(d,Answer.name_check_correct == "\"TRUE\"" & 
              d$mc.targ == 2 & 
              ((d$mc.dist == 1 & d$level == "L1") |
               (d$mc.dist == 2 & d$level == "L2"))) # & 
              #d$choice != "null")# & 
              #!duplicated(d$workerid))

############ ADD MODEL AND PLOT ############
ci.l <- function(x) {binom.confint(sum(x),length(x),method="bayes",tol=1e-5)$lower}
ci.h <- function(x) {binom.confint(sum(x),length(x),method="bayes",tol=1e-5)$upper}

f <- correct ~ cond + freq + level #+ item

ms <- aggregate(f, d, mean)
ms$n <- aggregate(f, d, length)$correct
ms$cil <- aggregate(f, d, ci.l)$correct
ms$cih <- aggregate(f, d, ci.h)$correct

### ADD MODELS
models <- c("L0","LS","LSL","LSLS")
mats <- list()
mats[[1]] <- stiller.scales
mats[[2]] <- stiller.noscales.mod
levels <- c("L1","L2")
es <- seq(.01,1,.01)
all.mods <- list()

for (e in 1:length(es)) {
  all.mods[[e]] <- data.frame()
  for (m in 1:length(mats)) {
    for (f in unique(ms$freq)) {
      corr.preds <- get.preds(mats[[m]],
                              subset(d,freq==f & 
                                       level==levels[m] & 
                                       cond=="baserate"),
                              eps=es[e])
      preds <- data.frame(cond=models,
                               freq=f,
                               level=levels[m],
                               correct=corr.preds,
                               cil=NA,cih=NA,n=NA)
      
      all.mods[[e]] <- rbind(all.mods[[e]],preds)
    }
  }
}

### FIND BEST FITTING PARAMS
rs <- matrix(nrow=length(es),ncol=4)
ys <- subset(ms,cond=="listener")$correct # to predict
for (e in 1:length(es)) {
   for (m in 1:length(models)) {
     xs <- subset(all.mods[[e]],cond==models[m])$correct
     rs[e,m] <- cor.test(xs,ys)$estimate
   }
}

best.es <- array()
for (i in 1:4) {
  best.es[i] <- which(rs[,i]==max(rs[,i]))
}

### PLOT IT
quartz()
comb <- rbind(ms,all.mods[[best.es[4]]]) # 
comb[comb$level=="L2" & comb$cond=="L0","correct"] = 
  comb[comb$level=="L2" & comb$cond=="L0","correct"] - .01 # offset the L0 so you can see it
# comb <- subset(comb,cond!="salience")
comb$cond <- factor(comb$cond, levels=c("baserate","listener","salience",
                                        "L0","LS","LSL","LSLS"))
ggplot(comb, aes(x=freq,y=correct,colour=cond,ymin=cil,ymax=cih)) + 
  facet_grid(level~.) + 
  xlab("Target Baserate") + 
  ylab("Proportion Target Judgments") + 
  ylim(c(0,1)) +
  geom_pointrange(data=subset(comb,cond=="listener" | cond=="salience"), #| cond=="baserate"
                  position=position_dodge(.02)) +  
  #geom_line(data=subset(comb,cond=="listener"  | cond=="salience"), #| cond=="baserate"
#position=position_dodge(.02)) + 
  geom_smooth(data=subset(comb,cond=="listener" | cond=="salience"),
              method="loess", span=2, se=FALSE) + 
#   geom_line(data=subset(comb, cond != "listener" & cond != "baserate" & cond != "salience"),
#            lty=2, se=FALSE,span=1.5) +
  theme_bw()

# 
# +
#   scale_colour_manual("Condition",
#                       breaks = c("baserate","listener"),
#                       values = c("baserate" = "blue",
#                                            "listener" = "red")) + 
#   scale_linetype_manual("Model",values = c() +
#   theme_bw()
#                   
#   
# 

# ## item analysis
# f <- correct ~ cond + freq + level + item
# ms <- aggregate(f, d, mean)
# ms$n <- aggregate(f, d, length)$correct
# ms$cil <- aggregate(f, d, ci.l)$correct
# ms$cih <- aggregate(f, d, ci.h)$correct
# 
# ggplot(ms, aes(x=freq,y=correct,colour=cond,ymin=cil,ymax=cih)) + 
#   facet_grid(level~item) + 
#   geom_pointrange(position=position_dodge(.05)) + 
#   geom_smooth(span=2, se=FALSE) +
#   xlab("Target Baserate") + 
#   ylab("Proportion Target Judgments") + 
#   ylim(c(0,1))
# 
# 
# # lmer(correct ~ cond * freq * level + (1|item), 
# #     data=d, family="binomial")
# 
# [1] 0.2036613 0.3867277 0.4096110
# [1] 0.1968811 0.3723197 0.4307992
# [1] 0.2210526 0.4736842 0.3052632
# [1] 0.2036613 0.5469108 0.2494279
