library(plyr)

source('ibr.R')

######################################################################
## Generate all binary matrices of a given size.
##
## Arguments:
##
## nrow: number of rows (worlds/referents) to have
## ncol: number of columns (properties/messages) to have
## include.row.repeats: allow identical entities (default: TRUE)
## include.col.repeats: allow identical messages/properties (default: TRUE)
## include.empty.cols: allow messages/properties true of no objects (defaut: TRUE, but this is incompatble with IBR and its variants)
## include.univ.cols: allow messages/properties true of all objects (defaut: TRUE)
## include.empty.rows: allow objects with no true messages/properties (default: TRUE)
## include.univ.rows:  allow objects with all messages/properties (default: TRUE)
##
## Value:
## mats: a list mapping integers to matrices.
##
## The function collapses matrices that are row and/or column variants of each other.
## Thus,
##
## AllBinaryMatrices(2,2, include.col.empty=F, include.row.empty=F)
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
## t2  1  1
##
## which does not contain, for example,
##
##    m1 m2
## t1  1  0
## t2  0  1
##
## because it is a permutation of [[1]] obtained by exchanging both rows and columns.

AllBinaryMatrices = function(nrow, ncol,
  include.permutation.variants=FALSE,
  include.row.repeats=TRUE, include.col.repeats=TRUE,
  include.empty.cols=TRUE, include.univ.cols=TRUE,
  include.empty.rows=TRUE, include.univ.rows=TRUE) {
  total = nrow*ncol
  matcount = 2^total
  ## This is used to filter out matrices that are row-permutations and/or
  ## column permutations of ones we've already seen.
  matlib = c()
  ## Output data structure
  mats = list()
  ## Increment the matrix counter:
  matind = 1
  ## Intuitive row and column names:
  row.names = paste('t', seq(1,nrow), sep='')
  col.names = paste('m', seq(1,ncol), sep='')
  ## Iterate with a while loop so that R doesn't try to build the
  ## whole 1:matcount vector:
  j = 1
  while (j <= matcount) {
    ## Get the nrow x ncol matrix associated with the binary version of j:
    vec = Integer2BinaryVector(j, total)
    thismat = matrix(vec, byrow=TRUE, nrow=nrow)
    ## Canonical (permutation invariant) string versions:
    matstr = Matrix2CanonicalStr(thismat)
    ## Iff we've never seen a row or column permutation variant of
    ## this matrix, we process it:
    if (include.permutation.variants == TRUE | (matstr %in% matlib)==FALSE) {
      ## Add this matrix to the library:
      matlib = c(matlib, matstr)
      ## Exclude matrices that contain 0s columns, since the model
      ## is not able to recover from such situations:
      if (include.empty.cols | ContainsZerosCol(thismat) == FALSE) {
        ## Option to exclude matrices in which a column has all 1s:
        if (include.univ.cols == TRUE | ContainsUniversalCol(thismat)==FALSE) {
          ## Option to exclude matrices that contain all 0 rows:
          if (include.empty.rows == TRUE | ContainsZerosRow(thismat) == FALSE) {
            ## Option to exclude matrices that contain all 1 rows:
            if (include.univ.rows == TRUE | ContainsUniversalRow(thismat) == FALSE) {
              ## Option to exclude matrices with repeated rows:
              if (include.row.repeats == TRUE | ContainsRowRepeats(thismat) == FALSE) {
                ## Option to exclude matrices with repeated rows:
                if (include.col.repeats == TRUE | ContainsColRepeats(thismat) == FALSE) {
                  ## Get the corresponding columns from vecs:           
                  rownames(thismat) = row.names
                  colnames(thismat) = col.names
                  mats[[matind]] = thismat
                  ## Increment the matrix counter:
                  matind = matind + 1
                }
              }
            }
          }
        }
      }
    }
    ## Increment while-loop counter:
    j = j + 1
    ## Progress report for very large runs:
    if (j %% 100000 == 0) {
      print(paste('Finished matrix:', j))
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

ModelLengths = function(nrow, ncol, models='IBR',
  include.permutation.variants=FALSE,
  include.row.repeats=TRUE, include.col.repeats=TRUE,
  include.univ.cols=TRUE,
  include.empty.rows=TRUE, include.univ.rows=TRUE) {
  ## Get all the matrices, as a list:
  mats = AllBinaryMatrices(nrow, ncol,
    include.row.repeats=include.row.repeats,
    include.col.repeats=include.col.repeats,
    include.empty.cols=FALSE,
    include.univ.cols=include.univ.cols,
    include.empty.rows=include.empty.rows,
    include.univ.rows=include.univ.rows)
  ## Apply all the models to al the elements of mats:
  df = ldply(.data=mats, .fun=ApplyAllModels, models) ##, .progress='text')
  
  colnames(df) = c('Matrix', models)
  ## Add these dimension columns so  that we can recreate the matrices from teh 
  df$Nrow = nrow
  df$Ncol = ncol
  ## More readable column order:
  df = df[ , c('Matrix', 'Nrow', 'Ncol', models)]  
  return(df)
}

ApplyAllModels = function(mat, models) {
  ## Row-wise string representation (use t() to transpose because R defaults to column-wise):
  str = paste(t(mat), collapse='')
  ## Output vector of values:
  vals = c(str)  
  for (i in 1:length(models)) {    
    model = get(models[i])
    seqs = model(mat)
    vals = c(vals, length(seqs))
  }
  return(vals)
}

######################################################################
## PLot the distribution of lengths for a given matrix space. The arguments
## are the same as those for IbrLengths. A plot window is produced.

ModelLengthPlot = function(nrow, ncol, model='IBR',
  include.permutation.variants=FALSE,
  include.row.repeats=TRUE, include.col.repeats=TRUE,
  include.univ.cols=TRUE,
  include.empty.rows=TRUE, include.univ.rows=TRUE) {
  df = ModelLengths(nrow, ncol, models=model,
    include.row.repeats=include.row.repeats,
    include.col.repeats=include.col.repeats,
    include.univ.cols=include.univ.cols,
    include.empty.rows=include.empty.rows,
    include.univ.rows=include.univ.rows)
  x = xtabs(~ df[, model])
  title = paste(nrow(df), ' (', nrow, ' x ', ncol, ') matrices; model=', model, sep='')
  barplot(x, xlab='Length', ylab='Count', main=title, axes=F, cex.main=1)
  axis(2, at=as.numeric(x), las=1)
}



