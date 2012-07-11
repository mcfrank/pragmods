#!/usr/bin/env R

## Utilities for the other functions

######################################################################
## Turn a vector into a probability distribution, but return an even
## distribution if the row contains no positive elements:
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

VecNormalize = function(row){
  if (sum(row) > 0) {
    return(row/sum(row))
  }
  else {
    return(rep(0, length(row)))
  }
}

######################################################################
## Identify the max element in a row and zero out all non-maximal ones
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

## Zero-out non-maximal elements 
VecMax = function(row){  
  row = sapply(row, function(x){ifelse(x==max(row), x, 0)})
  return(row)
}

######################################################################
## Test whether two matrices are equivalent up to the level of
## precision given by digits.
##
## Arguments
## m1, m2: 2d matrices
## digits: the level of precision at which to compare the matrices (default: 20)

MatrixEquality = function(m1, m2, digits=100) {
  m1 = round(m1, digits)
  m2 = round(m2, digits)
  ## Reduce the matrices to vectors of booleans:
  cmp = unique(as.numeric(m1) == as.numeric(m2))
  ## If cmp contains FALSE, then the whole thing is false:
  if (FALSE %in% cmp)
    return(FALSE)
  else{   
    return(TRUE)
  }
}

######################################################################
## Determine whether the vector x contains all and only 0s.
##
## Argument:
## x: a numeric vector
## Value:
## boolean: TRUE if x contains only 0s, else FALSE

ZerosVector = function(x) {
  rowset = unique(as.numeric(x))
  ## Check for all zeros:
  if (length(rowset) == 1 & rowset[1] == 0) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

######################################################################
## Determine whether the matrix m contains a row of all 0s.

ContainsZeroVector = function(m) {
  vals = apply(m, 1, ZerosVector)
  if (TRUE %in% vals) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

######################################################################
## If the numeric vector argument row contains only 0s, map it to a
## uniform distribution:
##
## Argument:
## row: a numeric vector
## Value:
## a numeric vector of the same length as row

ZerosVector2UniformDistibution = function(row) {
  if (ZerosVector(row)) {
    return(rep(1/length(row), length(row)))
  }
  else {
    return(row)
  }
}

######################################################################
## Create a uniform distribution of length n:
##
## Argument:
## n: an integer
## Value:
## A numeric vector of length n.

UniformDistribution = function(n) {
  return(rep(1/n, n))
}

######################################################################
## Create an all 0s cost matrix.
##
## Argument:
## m: a 2d matrix
## Value:
## An all zeros matrix with the same dimensions as m

UniformCosts = function(m) {
  return(matrix(rep(0, length(m)), byrow=T, nrow=nrow(m), dimnames=list(rownames(m), colnames(m))))
}

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
## Given a binary vector x, return the indices of the 1 values:
##
## Argument:
## x: binary vector
##
## Value:
## An integer vector:
##
## For example, GetOneValuedIndices(c(0,1,0,1)) returns c(2,4).

GetOneValuedIndices = function(x){
  vals = c()
  for (i in 1:length(x)) {
    if (x[i] == 1){
      vals = c(vals, i)
    }
  }
  return(vals)
}

######################################################################
## Map an integer to a binary vector:
##
## Arguments:
## i: integer to convert
## length: length of the resulting vector
##
## Value:
## a binary vector
##
## Note: crucial to use "s" for formatting; "d" fails for large values

BinaryString2Vector = function(i, length) {
  ## Format the binary form of i with 0-padding to length:
  fmt = paste("%0", length, "s", sep='')
  s = sprintf(fmt,  binary(i))
  ## Split the binary number into digits:
  vals = strsplit(s, '')[[1]]
  ## Convert from string to vector:
  vals = as.numeric(vals)
  ## Return:
  return(vals)
}

######################################################################
## Map a string s of 1s and 0s to a matrix of dimension nrow. It is
## assumed that s is given row-wise, in the sense that a row is created
## after every nrow elements.
##
## Arguments:
##
## s: a string of 1s and 0s -- must be a length divisible by nrow
## nrow: number of rows in the resulting matrix
## row.names: optional row names to add to the output matrix
## col.names: optional column names to add to the output matrix
##
## Value:
## A binary matrix.
##
## Example
## Str2Matrix('010101110', 3, row.names=NULL, col.names=NULL)
##
##      [,1] [,2] [,3]
## [1,]    0    1    0
## [2,]    1    0    1
## [3,]    1    1    0

Str2Matrix = function(s, nrow) {
  vals = strsplit(s, '')[[1]]
  vals = as.numeric(vals)
  m = matrix(vals, nrow=nrow, byrow=TRUE)
  if (!is.null(row.names)) {
    rownames(m) = row.names
  }
  if (!is.null(col.names)) {
    colnames(m) = col.names
  }
  return(m)
}

