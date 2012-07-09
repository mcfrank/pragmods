
library(sets)

source('ibr.R')

######################################################################
## Convert decimal to binary.
## From: http://blagrants.blogspot.com/2011/12/decimal-to-binary-in-r.html

binary<-function(p_number) {
  bsum<-0
  bexp<-1
  while (p_number > 0) {
     digit<-p_number %% 2
     p_number<-floor(p_number / 2)
     bsum<-bsum + digit * bexp
     bexp<-bexp * 10
  }
  return(bsum)
}


######################################################################
## Generate all of the possible binary vectors of length nrow. The
## result is a matrix in which, intuitively, the rows are entities
## and the columns are properties. For example,
##
## AllBinaryVectors(2)
##
## returns
##
##      [,1] [,2] [,3]
## [1,]    0    1    1
## [2,]    1    0    1
##
## in which each row is a property and all distinct properties are
## included -- excluding the empty property and the universal property.
##
## The resulting matrix is always (nrow x (2^nrow)-2) in dimension.

AllBinaryVectors = function(nrow)  {
  ncol = (2^nrow) - 2
  mat = matrix(rep(0, nrow*ncol), byrow=T, nrow=nrow)  
  for (i in 1:ncol) {
    fmt = paste("%0", nrow, "d", sep='')
    s = sprintf(fmt,  binary(i)  )
    vals = strsplit(s, '')[[1]]
    vals = as.numeric(vals)        
    mat[, i] = vals    
  }
  return(mat)
}

######################################################################
## Generate all binary matrices of dimension (nrow x ncol) that are
## distinguishable if we think of the columns as picking out sets of
## entities.

AllBinaryMatrices = function(nrow, ncol) {
  ## Intuitive row and column names:
  row.names = paste('t', seq(1,nrow), sep='')
  col.names = paste('m', seq(1,ncol), sep='')
  ## Output list of matrices:
  mats = list()
  ## The full set of possible vectors; we will draw
  ## subsets of length ncol:
  vecs = AllBinaryVectors(nrow)
  ## Column indices, which we turn into a set:
  indices = seq(1, ncol(vecs))
  indices = as.set(indices)
  ## Power-set of column indices:
  indexSets = 2^indices
  i = 1
  for (ind in indexSets) {
    ## Turn the set/list into a vector of indices:
    ind = unlist(ind)
    ## Where the number of indices is correct:
    if(length(ind) == ncol) {
      ## Get the corresponding columns from vecs:
      thismat = vecs[, ind]
      rownames(thismat) = row.names
      colnames(thismat) = col.names
      mats[[i]] = thismat
      i = i + 1
    }
  }
  return(mats)   
}

######################################################################
## Exhaustively search through a space of matrices of specified dimension.

IbrLengths = function(nrow, ncol) {
  df = data.frame('Matrix'=c(), 'Nrow'=c(), 'Ncol'=c(), 'Length'=c())
  mats = AllBinaryMatrices(nrow, ncol)
  for (mat in mats) {
    seqs = IBR(mat)
    str = paste(mat, collapse='')    
    len = length(seqs)
    thisdf = data.frame('Matrix'=str, 'Nrow'=nrow, 'Ncol'=ncol, 'Length'=len)
    df = rbind(df, thisdf)
  }
  return(df)
}




