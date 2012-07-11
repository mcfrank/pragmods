#!/usr/bin/env R

source('ibr.R')
source('viz.R')

######################################################################
## SCENARIOS:

## A standard scalar implicature case:
scalars = matrix(
  c(1,   0,   0,
    0,   1,   0,
    0,   1,   1), byrow=T, nrow=3,
  dimname=list(
    c('w_no', 'w_somenotall', 'w_all'), # Row names; worlds.
    c('NO', 'SOME', 'ALL'))) # Column names; messages.

## Stiller no-scales, but with the ballcap left null:
stiller.noscales = matrix(
  c(0,   0,   1,
    0,   1,   1,
    1,   1,   0), byrow=T, nrow=3,
  dimname=list(
    c('r1', 'r2', 'r3'), # Row names; referents.
    c('hat', 'glasses', 'mustache'))) # Column names; messages.

## Science paper referential game:
fg = matrix(
  c(1,0,1,0,
    1,0,0,1,
    0,1,1,0), byrow=T, nrow=3,
  dimnames=list(
    c('r_bs','r_bc','r_gs'), # Row names; objects.
    c('blue','green','square','circle')) # Colum names; messages.
  )

## The Horn division of labor game (makes crucial ise of the costs)
hornnames = list(
  c('w_footbreak', 'w_ditch'),
  c('STOP', 'MAKE-STOP'))

## Utilities:
hornutil = matrix(
  c(1,1,
    1,1), byrow=T, nrow=2,
  dimnames=hornnames)

## Costs:
horncosts = list()
horncosts[[1]] = t(matrix(c(0,    0, 1/10, 1/10), byrow=T, nrow=2, dimnames=hornnames))
horncosts[[2]] = matrix(c(0, 1/10,    0, 1/10), byrow=T, nrow=2, dimnames=hornnames)

## A 4x4 case that requires 7 matrices in IBR:
m7 = matrix(
  c(0,0,1,1,
    1,1,0,0,
    0,1,0,1,
    1,0,0,1), byrow=T, nrow=4,
  dimnames=list(paste("t", seq(1,4),sep=''), ## Row names; refernets.
    c('hat', 'glasses', 'must', 'hair'))) ## Column names; messages.
  


######################################################################
## DEMOS:

## Example from p. 9ff of the Jaeger handbook article:
ScalarImplicature = function() {
  print("======================================================================")
  print('Scalars')    
  print(IBR(scalars))
}

## No Scales condition from the Stiller et al. paper:
StillerNoscales = function() {
  print("======================================================================")
  print('Stiller no scales.')    
  print(IBR(stiller.noscales))
  MatrixViz(stiller.noscales)  
}

## Science paper referential game:
FrankGoodman = function() {
  print("======================================================================")
  print('Frank-Goodman')    
  print(IBR(fg))
}

## Example from p. 16-17 of the Jaeger handbook article:
Division = function() {
  print("======================================================================")
  print('Division of pragmatic labor (costly messages)') 
  print(IBR(hornutil, costs=horncosts))

}

## Evaluate from a string:
FromString = function(s) {
  print("======================================================================")
  print(paste('Processing string', s) )
  return(eval(parse(text=s)))
}

## 4x4 matrix requring 7 iterations (the max):
Matrix7 = function() {
  print("======================================================================")
  print('4 x 4 matrix requiring 7 iterations in IBR')
  print(IBR(m7))
  MatrixViz(m7, print.matrix=TRUE)
}

## All demos:
Demos = function() {  
  ScalarImplicature()
  StillerNoscales()
  FrankGoodman()
  Division()
  print(FromString('Lstar(Sstar(Lstar(S0(fg), fg)), fg)'))
  Matrix7()
}

Demos()
  


