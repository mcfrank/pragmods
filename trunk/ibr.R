#!/usr/bin/env R

source('util.R')
source('agents.R')

######################################################################
## CLASSICAL IBR
##
## 
## m: the initial semantic matrix (0s and 1s only)
## costs: list with indices 1 (listener) and 2 (speaker); default: NULL, which imposes even costs
## prior: prior over worlds (rows in m); default is a uniform distribution
## maxiter: prevents an infinite loop (possible only if there is a bug; default: 100)
##
## Value:
##
## A list seq with numeric keys, where the values are strategies.
## length(seq) gives the depth of iteration

IBR = function(m, costs=NULL, prior=UniformDistribution(nrow(m)), maxiter=100) {
  seq = Iterator(m, costs=costs, prior=prior, argmax=TRUE, maxiter=maxiter, digits=20)
  return(seq)
}

######################################################################
## Arguments:
##
## m: initial 1/0 matrix capturing the underlying semantics
## costs: list with indices 1 (listener) and 2 (speaker); default: NULL, which imposes even costs
## prior: prior over worlds (rows in m); default is a uniform distribution
## maxiter: prevents an infinite loop (possible only if there is a bug; default: 100)
## digits: number of decimal places to consider when calculating equality of values
##
## Value:
##
## A list seq with numeric keys, where the values are strategies.
## length(seq) gives the depth of iteration

Iterator = function(m, costs=NULL, prior=UniformDistribution(nrow(m)), argmax=TRUE, maxiter=100, digits=100) {
  ## We will use this to access the right functions:
  funcs = list()
  funcs[[1]] = Listener
  funcs[[2]] = Speaker
  ## Uniform costs if no costs are supplied:
  if (is.null(costs)) {
    costs = list()
    costs[[1]] = t(UniformCosts(m))
    costs[[2]] = UniformCosts(m)
  }  
  ## Output data structure:
  seq = list()
  ## Get the base speaker:
  seq[[1]] = S0(m)
  ## We need at least one listener:
  seq[[2]] = Listener(seq[[1]], m, costs=costs[[1]], prior=prior, argmax=argmax)
  ## Now we can iterate:
  i = 3
  while (i <= maxiter) {
    ## R won't allow 0 indices in vectors or list keys, so we add 1 to obtain
    ## the value accessor index used to pick the right function and cost function:
    valueIndex = (i %% 2) + 1
    ## Get the function we need (S or L):
    func = funcs[[valueIndex]]
    ## Get the next strategy:    
    val = func(seq[[i-1]], m, costs=costs[[valueIndex]], prior=prior, argmax=argmax)
    ## If the next strategy is the same as the previous one of this type (2 back)
    ## then we have converged.
    if (MatrixEquality(val, seq[[i-2]], digits=digits)) {
      return(seq)
    }
    ## Where we haven't converged, we add the new strategy and increment the counter:
    else {      
      seq[[i]] = val
      i = i + 1
    }    
  }
  return(seq)
}
