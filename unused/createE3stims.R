## createE1stims.R
# creates stimuli for a first experiment, on level of inference
# the goal is to measure whether featureicipants can make a "level 2" inference 
# across randomized stimuli / positions
library(jpeg)

rm(list=ls())

source('viz_img.R')
source('ibr.R')
source('agents.R')
source('search.R')

randperm <- function (x) {sample(x,length(x))}

## Experimental matrix
expt = matrix(
  c(0,   0,   0,   1,
    0,   1,   1,   1,
    1,   0,   1,   1, 
    1,   1,   1,   0), byrow=T, nrow=4,
  dimname=list(
    c('r1', 'r2', 'r3', 'r4'), # Row names; referents.
    c('m1', 'm2', 'm3', 'm4'))) # Column names; messages.

levels <- c(1,2,3)
target.referents <- c(1,3,4)
distractor.referents <- c(2,4,1) 
other.referents <- c(3,2,2) 
target.features <- c(4,1,3)
num.with.features <- colSums(expt)

stims <- c("face","snowman","sundae")
stim.plurals <- c("faces","snowmen","sundaes")
stims.features <- list()
stims.features[[1]] <- c("hat","glasses","mustache","tie")
stims.features[[2]] <- c("beanie","scarf","gloves","belt")
stims.features[[3]] <- c("cherry","whipped cream","chocolate","banana")
stims.features.word <- list()
stims.features.word[[1]] <- c("a hat","glasses","a mustache","a tie")
stims.features.word[[2]] <- c("a beanie","a scarf","gloves","a belt")
stims.features.word[[3]] <- c("a cherry","whipped cream","chocolate sauce","a banana")

## variables
n.subs <- 50

## main loop
expt.data <- data.frame(hitNum = 1:n.subs)
for (s in 1:n.subs) {
  expt.data$originalMatrix[s] = paste(expt,collapse="")
	stim.perm <- randperm(1:length(stims))
	level.perm <- randperm(1:length(levels))
  
	for (l in 1:length(levels)) {
		level = levels[level.perm[l]]
      
		#sample permutations
		row.perm <- randperm(1:nrow(expt))
		col.perm <- randperm(1:ncol(expt))
		expt.perm <- expt[row.perm,col.perm]
    
    stim = stims[stim.perm[l]]
		stim.plural = stim.plurals[stim.perm[l]]
		stim.features = stims.features[[stim.perm[l]]]
		stim.features.word = stims.features.word[[stim.perm[l]]]
    
    filename = paste("S",as.character(s),"-trial",as.character(l),".jpg",sep="")
		jpeg(paste("E3-stims/",filename,sep=""),
         height=2,width=8,units="in",res=144)
		ImageViz(expt.perm,stim=stims[stim.perm[l]])
    dev.off()
        
    target.referent = match(target.referents[level.perm[l]],row.perm)
		distractor.referent = match(distractor.referents[level.perm[l]],row.perm)
    other.referent = match(other.referents[level.perm[l]],row.perm)
		target.feature = match(target.features[level.perm[l]],col.perm)

		expt.data[s,paste("filenameTrial",l,sep="")] = paste("E3/",filename,sep="")
    expt.data[s,paste("levelTrial",l,sep="")] = level
		expt.data[s,paste("stimSingularTrial",l,sep="")] = stim
		expt.data[s,paste("stimPluralTrial",l,sep="")] = stim.plural
		expt.data[s,paste("targetReferentTrial",l,sep="")] = target.referent
		expt.data[s,paste("distractorReferentTrial",l,sep="")] = distractor.referent		
		expt.data[s,paste("otherReferentTrial",l,sep="")] = other.referent
    expt.data[s,paste("targetFeatureTrial",l,sep="")] = stim.features[target.feature]
    
    for (i in 1:4) {
		  expt.data[s,paste("check",i,"NameTrial",l,sep="")] = stim.features.word[i]
		  expt.data[s,paste("check",i,"CountTrial",l,sep="")] = num.with.features[col.perm[i]]
		}
    
		expt.data[s,paste("targetReferentTrial",l,sep="")] = target.referent
    expt.data[s,paste("permutedMatrix",l,sep="")] = paste(expt.perm,collapse="")
	}	
}

write.csv(expt.data,"E3-stims/Experiment3_data.csv",row.names=FALSE)