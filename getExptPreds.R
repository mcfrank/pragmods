########################################################
#### GATHER TOGETHER PREDICTIONS FOR ALL EXPERIMENTS ###

getExptPreds <- function(filename="data/experiment_conditions.csv",salience,
                         models=c("L_S0","FG","L_S_L_S_L_S0","L_S_L_S_L_S_L_S0")) {
  
  es <- read.csv(filename)
  es$level <- factor(es$level)
  
  # note I added these to agents.R  
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
  
  return(preds)
}