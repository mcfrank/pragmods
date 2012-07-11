
library(sets)

source('ibr.R')

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
  options(scipen=1000)
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
  ## Formatting basis for binary vectors; we use
  ## val rather than i for the counting so that we
  ## can optionally start at 0 even as we index into
  ## mat, where the lowest value allowed is 1.  
  for (i in 1:ncol) {
    ## Map this integer value to a binary value
    ## and use that to obtain a binary vector:
    vals = BinaryString2Vector(val, nrow)
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
  ## Number of matrices we need to consider:
  powerset.size = 2^(ncol(vecs))
  if (!include.universal) {
    powerset.size = powerset.size - 1
  }
  ## Indices for the output list:
  matind = 1
  ## Can't use a for loop because R actually tries to
  ## instantiate the full vector of integers 1:powerset.size!
  j = 1
  print(paste('Total number of matrices to generate and test:', powerset.size))  
  while (j <= powerset.size) {
    ## Get the appropriate binary vector:
    these.indices = BinaryString2Vector(j, ncol(vecs))
    ## We need to have ncol number of "on" indices:
    if (sum(these.indices) == ncol) {      
      ## Convert from 0/1 to column indices:
      col.indices = GetOneValuedIndices(these.indices)      
      ## Get the matrix:
      thismat = vecs[, col.indices]      
      ## Option to exclude matrices that contain all 0 rows:
      if (include.ineffable == TRUE | ContainsZeroVector(thismat) == FALSE) {
        ##print(sort(col.indices))
        ##print(thismat)
        ## Get the corresponding columns from vecs:           
        rownames(thismat) = row.names
        colnames(thismat) = col.names
        mats[[matind]] = thismat
        matind = matind + 1
      }      
    }
    if (j %% 1000000 == 0) {
      print(paste('Finished matrix:', j))
    }
    j = j + 1    
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
    ## Row-wise string representation (use t() to transpose because R defaults to column-wise):
    str = paste(t(mat), collapse='')    
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
  title = paste(nrow(df), ' (', nrow, ' x ', ncol, ') matrices; include.universal=', include.universal, '; include.ineffable=', include.ineffable, sep='')
  barplot(x, xlab='Length', ylab='Count', main=title, axes=F)
  axis(2, at=as.numeric(x), las=1)
}

