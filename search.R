
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
  mats = list()
  vecs = AllBinaryVectors(nrow)
  indices = seq(1, ncol(vecs))
  indices = as.set(indices)
  indexSets = 2^indices
  i = 1
  for (ind in indexSets) {
    ind = unlist(ind)
    if(length(ind) == ncol) {
      mats[[i]] = vecs[, ind]
      i = i + 1
    }
  }
  return(mats)   
}

######################################################################
## Exhaustively search through a space of matrices to see how many
## steps they require for convergence.

IbrLengths = function(nrow, ncol) {
  lengths = c()
  mats = AllBinaryMatrices(nrow, ncol)
  for (mat in mats) {
    seqs = IBR(mat)
    lengths = c(lengths, length(seqs))    
  }
  return(lengths)
}


