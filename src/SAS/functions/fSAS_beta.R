fSAS_beta <- function(Ps, par, ...) {
  # function to compute fSAS function as a beta distribution
  # Om = fSAS_pl(Ps,par,wi)        
  
  # Synopsis
  # Om = the cumulative Omega function, evaluated over each element of Ps
  # Ps = cumulative storage age distribution (S_T/S)
  # par  = vector with parameters. 
  
  # assign the variables
  a <- par[1]  # affinity for young ages (the lower a, the higher the affinity)
  b <- par[2]  # affinity for old ages (the lower b, the higher the affinity)
  
  # do some error check
  if (length(par) != 2) {
    stop('wrong number of input parameters')
  }
  if (any(par < 0)) {
    stop('parameters of the beta function must be positive')
  }
  
  # compute omega
  Om <- pbeta(Ps, a, b)
  
  return(Om)
}
