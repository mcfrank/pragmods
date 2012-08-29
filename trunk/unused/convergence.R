## convergence.R
# a set of functions for describing the convergence properties of MMMs

source('IBR.R')
source('search.R')

SpeakerMatrix = function(m, seq) {
  return(rownames(m)[1] == rownames(seq[[1]])[1])
}

MessageConvergence = function(m) {
  vals = apply(m, 1, OneHotVector)
  return(vals)
}

FindConvergenceLevel = function(vals) {
  levels = apply(vals, 2, function(x) {match(T,x)})
  return(levels)
}

## FindMatrixConvergence
## 
## find which iteration each message converged on, returns a list of iteration depths

FindMatrixConvergence = function(mat, verbose=FALSE) {
  seq = IBR(mat)
  depth = length(seq)/2
  vals = matrix(nrow=depth*2,ncol=ncol(mat))
  c = 1
  for (m in seq) {
    if (!SpeakerMatrix(m,seq)) {
      vals[c,] = MessageConvergence(m)    
    } else {
      vals[c,] = MessageConvergence(t(m))
    }  
    c = c + 1  
  }
  colnames(vals) = colnames(mat)
  levels = FindConvergenceLevel(vals)/2
  
  if (verbose) {
    print("======================================================================")
    print(mat)
    print('Convergences')
    print(vals)
    print(paste('Messages converge at: ',paste(levels,collapse=', ')))
    print(paste('Total depth of system:', depth))    
    ImageViz(mat)
  }
  
  return(levels)
}


## FindAllConvergenceLevels
## 
## finds convergence levels for all non-empty binary matrices   

FindAllConvergenceLevels = function(nrow, ncol) {
  mats = AllBinaryMatrices(nrow, ncol, 
                           include.empty.rows=FALSE, include.empty.cols=FALSE)
  
  levels = matrix(nrow=length(mats),ncol=ncol)
  for (i in 1:length(mats)) {
    levels[i,] = FindMatrixConvergence(mats[[i]])
  }
  
  return(levels)
}

