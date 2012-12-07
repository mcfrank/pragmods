library(jpeg)

source('viz_img.R')
##source('ibr.R')
##source('agents.R')
##source('search.R')

######################################################################

CreateExperimentalData = function(
  expt=c(),
  row.names=paste('r', seq(1:nrow(expt)), sep=''),
  col.names=paste('m', seq(1:ncol(expt)), sep=''),
  outputDirname=NULL,
  outputFilename=NULL,
  n.subs=50,
  levels=c(),
  target.referents=c(),
  distractor.referents=c(),
  other.referents=c(),
  target.features=c(), 
  stims=c("face","snowman","sundae"),
  stim.plurals=c("faces","snowmen","sundaes"),
  stims.features=list(c("hat","glasses","mustache"),
                      c("beanie","scarf","gloves"),
                      c("cherry","whipped cream","chocolate")),
  stims.features.word=list(c("a hat","glasses","a mustache"),
                           c("a beanie","a scarf","gloves"),
                           c("a cherry","whipped cream","chocolate sauce")),
  salience="none",salience.output="color") {

  ## A bit of error-checking; more could be done to ensure sensible
  ## designs that will throw errors later.
  if (is.null(outputDirname)) {
    stop(paste('outputDirname must be specified'))
  }
  if (!file.exists(outputDirname)) {
    stop(paste('The specified output directory', outputDirname, 'does not exist.'))
  }
  if (is.null(outputFilename)) {
    stop('A filename argument is required.')
  }

  ## Just add intuitive row and column names for now:
  expt = PrepExperimentalMatrix(expt)
  
  ## The output data frame that we will store in CSV:
  expt.data = data.frame(hitNum=1:n.subs)

  ## Uniform column containing a flat string representation of the experimental matrix:
  expt.data$originalMatrix = paste(expt, collapse="")

  ## Row-iterator, filling out the rest of expt.data:
  for (s in 1:n.subs) {

    ## These shuffled indices are used to get random
    ## selections from the materials:
    stim.perm = randperm(1:length(stims))
    level.perm = randperm(1:length(levels))

    for (l in 1:length(levels)) {

      ## Randomly chosen indices into the materials:
      stim.index = stim.perm[l]
      level.index = level.perm[l]

      ## Current level:
      level = levels[level.index]
      
      ## Permute the matrix randomly:
      row.perm = randperm(1:nrow(expt))
      col.perm = randperm(1:ncol(expt))
      expt.perm = expt[row.perm, col.perm]
      
      ## Features of the current stimulus:      
      stim = stims[stim.index]
      stim.plural = stim.plurals[stim.index]
      stim.features = stims.features[[stim.index]]
      stim.features.word = stims.features.word[[stim.index]]
      
      ## Cell values particular to this trial:
      target.referent = match(target.referents[level.index], row.perm)
      distractor.referent = match(distractor.referents[level.index], row.perm)      
      other.referent = match(other.referents[level.index], row.perm)
      target.feature = match(target.features[level.index], col.perm)
      
      ## if we are manipulating salience
      salience.vals = rep(F,nrow(expt)) # by default all false
      
      if (salience == "target") {
        salience.vals = rep(T,nrow(expt))
        salience.vals[target.referent] <- F
      } else if (salience == "distractor") {
        salience.vals = rep(T,nrow(expt))
        salience.vals[distractor.referent] <- F
      }
      
      ## Create the image filename and the image itself:
      filename = StimFilename(s.index=s, level.index=l, outputDirname=outputDirname)
      if (salience.output == "color") {  # if we are using color to do salience
        CreateStimImage(expt.perm, stim=stim, filename, width=2*nrow(expt), bws=salience.vals)
      } else {
        CreateStimImage(expt.perm, stim=stim, filename, width=2*nrow(expt))
      }
      
      ## Add values for this row and level:
      expt.data[s, IndexedColumnName("filenameTrial", l)] = filename
      expt.data[s, IndexedColumnName("levelTrial", l)] = level
      expt.data[s, IndexedColumnName("stimSingularTrial", l)] = stim
      expt.data[s, IndexedColumnName("stimPluralTrial", l)] = stim.plural
      expt.data[s, IndexedColumnName("targetReferentTrial", l)] = target.referent
      expt.data[s, IndexedColumnName("distractorReferentTrial", l)] = distractor.referent		
      expt.data[s, IndexedColumnName("otherReferentTrial", l)] = other.referent
      expt.data[s, IndexedColumnName("targetFeatureTrial", l)] = stim.features[target.feature]

      ## Columns recording particular details of this row:
      num.with.features = colSums(expt)
      c = 1
      for (i in 1:ncol(expt)) {
        if (num.with.features[col.perm[i]] > 0) {
          expt.data[s, paste("check", c, "NameTrial", l, sep="")] = stim.features.word[i]
          expt.data[s, paste("check", c, "CountTrial", l, sep="")] = num.with.features[col.perm[i]]
          c = c + 1
        }
      }
      expt.data[s, IndexedColumnName("targetReferentTrial", l)] = target.referent
      expt.data[s, IndexedColumnName("permutedMatrix", l)] = paste(expt.perm,collapse="")
      
      ## If we are doing salience using target phrases
      positions = c("one on the left","one in the middle","one on the right")
      if (salience.output == "phrase" & salience == "target") {
        expt.data[s, IndexedColumnName("targetReferencePhraseTrial", l)] = positions[target.referent]
      } else if (salience.output == "phrase" & salience == "distractor") {
        expt.data[s, IndexedColumnName("targetReferencePhraseTrial", l)] = positions[distractor.referent]
      }
    }
  }

  ## Create the output CSV file:  
  write.csv(expt.data, file.path(outputDirname, outputFilename), row.names=FALSE)  
}

######################################################################
## Shuffle a vector or matrix, with no change in dimensions.

randperm <- function (x) {sample(x,length(x))}

######################################################################
## Add basic row and column labels to a matrix to help keep them
## distinguished. Default is  (r1, r2, ..., rM) and (m1, m2, ..., mN),
## but this can be changed with row.names and col.names keyword
## arguments.

PrepExperimentalMatrix = function(m,
  row.names=paste('r', seq(1:nrow(m)), sep=''),
  col.names=paste('m', seq(1:ncol(m)), sep='')) {
  rownames(m) = row.names
  colnames(m) = col.names
  return(m)
}

######################################################################
## Output image filename for stimuli:
  
StimFilename = function(s.index=NULL, level.index=NULL, outputDirname=NULL, extension=".jpg") {
  filename = paste("S", as.character(s.index), "-trial", as.character(level.index), extension, sep="")
  filename = file.path(outputDirname, filename)
  return(filename)
}

######################################################################
## Create an image file:

CreateStimImage = function(expt.perm=NULL, stim=NULL, filename=NULL, height=2, 
                           width=6, units="in", res=144, bws=rep(F,nrow(expt.perm))) {
  jpeg(filename, height=height, width=width, units=units,res=res)
  ImageViz(expt.perm, stim=stim, bws=bws)
  dev.off()
}

######################################################################
## Function for creating indexed column names in a uniform style:

IndexedColumnName = function(label, index) {
  return(paste(label, index, sep=""))
}

  


  
