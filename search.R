library(plyr)

source('ibr.R')

######################################################################
##
##
## Arguments:
##
## nrow: number of rows (worlds/referents) to have
## ncol: number of columns (properties/messages) to have
## include.row.repeats: allow identical entities (default: TRUE)
## include.col.repeats: allow identical messages/properties (default: TRUE)
## include.empty: allow messages/properties true of no objects (defaut: TRUE, but this is incompatble with IBR and its variants)
## include.universal: whether to include columns with all 1s (default: FALSE)
## include.ineffable: whether to have any all 0s rows (default: FALSE)
##
## Value:
## mats: a list mapping integers to matrices.
##
## For example,
##
## AllBinaryMatrices(2,2, include.empty=FALSE, include.universal=TRUE)
##
## [[1]]
##    m1 m2
## t1  1  0
## t2  0  1
##
## [[2]]
##    m1 m2
## t1  1  1
## t2  1  0
##
## Notice that
##
##    m1 m2
## t1  0  1
## t2  1  0
##
## is excluded. This is because it is a column permutation variant of [[1]]. Similarly,
##
##    m1 m2
## t1  0  0
## t2  1  1
##
## is excluded because it is a row permutation variant of [[2]].

AllBinaryMatrices = function(nrow, ncol, include.row.repeats=TRUE, include.col.repeats=TRUE, include.empty=TRUE, include.universal=TRUE, include.ineffable=TRUE) {
  total = nrow*ncol
  matcount = 2^total
  ## These are used to filter out matrices that are row-permutations of
  ## ones we've already seen or column permutations of ones we've
  ## already seen:
  row.matlib = c()
  col.matlib = c()  
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
    matstr.row = Matrix2CanonicalStr(thismat)
    matstr.col = Matrix2CanonicalStr(t(thismat))
    ## If we've this matrix-type before, in terms of permutations on rows or columns,
    ## then we register its canonical form for both but we don't do anything else:    
    ##if (!matstr.row %in% row.matlib | !matstr.col %in% col.matlib) {
    ##  col.matlib = c(col.matlib, matstr.col)
    ## row.matlib = c(row.matlib, matstr.row)
    ##}
    ## If this is new in terms of permutations on both rows and columns:
    ##else {
      ## Add this matrix to the libraries:
      row.matlib = c(row.matlib, matstr.row)
      col.matlib = c(col.matlib, matstr.col)
      ## Exclude matrices that contain 0s columns, since the model
      ## is not able to recover from such situations:
      if (include.empty | ContainsZerosCol(thismat) == FALSE) {
        ## Option to exclude matrices in which a column has all 1s:
        if (include.universal == TRUE | ContainsUniversalCol(thismat)==FALSE) {
          ## Option to exclude matrices that contain all 0 rows:
          if (include.ineffable == TRUE | ContainsZeroVector(thismat) == FALSE) {
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
    ##}
    ## Increment while-loop counter:
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

ModelLengths = function(nrow, ncol, models=c('IBR'),
  include.row.repeats=TRUE, include.col.repeats=TRUE,
  include.universal=TRUE, include.ineffable=TRUE) {
  ## Get all the matrices, as a list:
  mats = AllBinaryMatrices(nrow, ncol,
    include.row.repeats=include.row.repeats,
    include.col.repeats=include.col.repeats,
    include.empty=FALSE,
    include.universal=include.universal,
    include.ineffable=include.ineffable)
  ## Apply all the models to al the elements of mats:
  df = ldply(.data=mats, .fun=ApplyAllModels, models) ##, .progress='text')
  colnames(df) = c('Matrix', names(models))
  ## Add these dimension columns so  that we can recreate the matrices from teh 
  df$Nrow = nrow
  df$Ncol = ncol
  ## More readable column order:
  df = df[ , c('Matrix', 'Nrow', 'Ncol', names(models))]  
  return(df)
}

ApplyAllModels = function(mat, models) {
  ## Row-wise string representation (use t() to transpose because R defaults to column-wise):
  str = paste(t(mat), collapse='')
  ## Output vector of values:
  vals = c(str)  
  for (modname in models) {    
    model = get(modname)
    seqs = model(mat)
    vals = c(vals, length(seqs))
  }
  return(vals)
}

######################################################################
## Return the maximum length for a given matrix space. The arguments
## are the same as those for IbrLengths. The value is a float.

ModelMaxLength = function(nrow, ncol, model='IBR',
  include.row.repeats=TRUE, include.col.repeats=TRUE, include.universal=TRUE, include.ineffable=TRUE) {
  df = ModelLengths(nrow, ncol, models=model,
    include.row.repeats=include.row.repeats,
    include.col.repeats=include.col.repeats,
    include.universal=include.universal,
    include.ineffable=include.ineffable)
  return(max(df$Length))
}

######################################################################
## PLot the distribution of lengths for a given matrix space. The arguments
## are the same as those for IbrLengths. A plot window is produced.

ModelLengthPlot = function(nrow, ncol, model=model,
  include.row.repeats=TRUE, include.col.repeats=TRUE, include.universal=TRUE, include.ineffable=TRUE) {
  df = ModelLengths(nrow, ncol, models=model,
    include.row.repeats=include.row.repeats,
    include.col.repeats=include.col.repeats,
    include.universal=include.universal,
    include.ineffable=include.ineffable)
  x = xtabs(~ df[, model])
  title = paste(nrow(df), ' (', nrow, ' x ', ncol, ') matrices; include.universal=', include.universal, '; include.ineffable=', include.ineffable, sep='')
  barplot(x, xlab='Length', ylab='Count', main=title, axes=F)
  axis(2, at=as.numeric(x), las=1)
}

######################################################################
## IBR-specific exploration functions:

## IbrMaxLength = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
##   return(ModelMaxLength(nrow, ncol, model=IBR, include.universal=include.universal, include.ineffable=include.ineffable))
## }

## IbrLengths = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
##   return(ModelLengths(nrow, ncol, model=IBR, include.universal=include.universal, include.ineffable=include.ineffable))
## }

## IbrLengthPlot = function(nrow, ncol, include.universal=FALSE, include.ineffable=FALSE) {
##   ModelLengthPlot(nrow, ncol, model=IBR, include.universal=include.universal, include.ineffable=include.ineffable)
## }



