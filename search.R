
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
## and the columns are properties.
##
## Arguments:
##
## nrow: the number of (worlds/referents) to have
## include.universal: whether to include columns with all 1s (default: FALSE)
##
## For example,
##
## AllBinaryVectors(2)
##
## returns
##
##      [,1] [,2]
## [1,]    0    1
## [2,]    1    0
##
## whereas
##
## AllBinaryVectors(2, include.universal=T)
##
## returns
##
##      [,1] [,2] [,3]
## [1,]    0    1    1
## [2,]    1    0    1

AllBinaryVectors = function(nrow, include.universal=FALSE)  {  
  val = 1
  ncol = (2^nrow) - 2
  ## include.empty is now always set to FALSE, because I think
  ## the model can't accommodate it -- it creates surprise rows 
  ## whose semantics is also surprising and hence there is
  ## no startegy to resort to.
  include.empty = FALSE
  ## If starting with the all 0s column:
  if (include.empty) {
    val = 0
    ncol = ncol + 1    
  }
  ## If finishing with the all 1s column:
  if (include.universal) {
    ncol = ncol + 1    
  }
  ## Output matrix:
  mat = matrix(rep(0, nrow*ncol), byrow=T, nrow=nrow)
  ## Formatting basis for binary vectors:
  fmt = paste("%0", nrow, "d", sep='')  
  for (i in 1:ncol) {
    ## Format as a binary number with the right padding:
    s = sprintf(fmt,  binary(val))
    ## Split the binary number into digits and convert to vector:
    vals = strsplit(s, '')[[1]]
    vals = as.numeric(vals)
    ## Add the resulting vector to the matrix:
    mat[, i] = vals
    ## Increment:
    val = val + 1
  }
  return(mat)
}

######################################################################
## Generate all binary matrices of dimension (nrow x ncol) that are
## distinguishable if we think of the columns as picking out sets of
## entities.
##
## Empty properties (all 0 columns) and universal properties (all 1 columns)
## are not included.
##
## Arguments:
##
## nrow: the number of (worlds/referents) to have
## ncol: number of columns (properties/messages) to have
## include.universal: whether to include columns with all 1s (default: FALSE)
## include.ineffable: whether to have any all 0s rows (default: FALSE)
##
## For example,
##
## AllBinaryMatrices(2, 2, include.universal=T)
##
## delivers
##
## [[1]]
##    m1 m2
## t1  0  1
## t2  1  0
##
## [[2]]
##    m1 m2
## t1  0  1
## t2  1  1
##
## [[3]]
##    m1 m2
## t1  1  1
## t2  0  1

AllBinaryMatrices = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
  ## Check for sensible values:
  if(ncol <= 1) {
    stop(paste("Apologies: nrow needs to be greater than 1."))
  }
  if (nrow < ncol) {
    stop(paste("Apologies: nrow needs to be at least as large as the ncol."))
  } 
  ## Intuitive row and column names:
  row.names = paste('t', seq(1,nrow), sep='')
  col.names = paste('m', seq(1,ncol), sep='')
  ## Output list of matrices:
  mats = list()
  ## The full set of possible vectors; we will draw
  ## subsets of length ncol:
  vecs = AllBinaryVectors(nrow, include.universal=include.universal)
  ## Column indices, which we turn into a set:
  indices = seq(1, ncol(vecs))
  indices = as.set(indices)
  ## Power-set of column indices:
  indexSets = 2^indices - set(set())  
  i = 1
  for (ind in indexSets) {
    ## Turn the set/list into a vector of indices:
    ind = unlist(ind)    
    ## Where the number of indices is correct:
    if(length(ind) == ncol) {
      thismat = vecs[, ind]
      ## Option to exclude matrices that contain all 0 rows:
      if (include.ineffable == TRUE | ContainsZeroVector(thismat) == FALSE) {       
        ## Get the corresponding columns from vecs:           
        rownames(thismat) = row.names
        colnames(thismat) = col.names
        mats[[i]] = thismat
        i = i + 1
      }
    }
  }
  return(mats)   
}

######################################################################
## Exhaustively search through a space of matrices of specified dimension.
##
## Arguments:
##
## nrow: number of rows (worlds/referents) to have
## ncol: number of columns (properties/messages) to have
## include.universal: whether to include columns with all 1s (default: FALSE)
## include.ineffable: whether to have any all 0s rows (default: FALSE)
##
## Value:
## A data.frame with columns
##
## Matrix Nrow Ncol Length
##
## where Matrix is a string representation of the matrix, Nrow and Ncol are
## the provided arguments, and Lenght is the number of steps required for
## convergence.

IbrLengths = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
  df = data.frame('Matrix'=c(), 'Nrow'=c(), 'Ncol'=c(), 'Length'=c())
  mats = AllBinaryMatrices(nrow, ncol, include.universal=include.universal, include.ineffable=include.ineffable)
  for (mat in mats) {
    seqs = IBR(mat)
    str = paste(mat, collapse='')    
    len = length(seqs)
    thisdf = data.frame('Matrix'=str, 'Nrow'=nrow, 'Ncol'=ncol, 'Length'=len)
    df = rbind(df, thisdf)
  }
  return(df)
}

######################################################################
## Return the maximum length for a given matrix space. The arguments
## are the same as those for IbrLengths. The value is a float.

IbrMaxLength = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
  df = IbrLengths(nrow, ncol, include.universal=include.universal, include.ineffable=include.ineffable)
  return(max(df$Length))
}

######################################################################
## PLot the distribution of lengths for a given matrix space. The arguments
## are the same as those for IbrLengths. A plot window is produced.

IbrLengthPlot = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
  df = IbrLengths(nrow, ncol, include.universal=include.universal, include.ineffable=include.ineffable)
  x = xtabs(~ df$Length)
  title = paste('(', nrow, ' x ', ncol, ') matrices; include.universal=', include.universal, '; include.ineffable=', include.ineffable, '; ', nrow(df), ' matrices', sep='')
  barplot(x, xlab='Length', ylab='Count', main=title, axes=F)
  axis(2, at=as.numeric(x), las=1)
}


