fSAS_pl <- function(Ps, par, ...) {
  # function to compute a pl (power-law) fSAS function: Om = Ps^k
  # Om = fSAS_pl(Ps, par = c(k))
  # Ps = cumulative storage age distribution (S_T/S)
  # par = vector with parameter k.
  
  # do some error check
  if (length(par) != 1) {
    stop('wrong number of input parameters')
  }
  if (any(par) < 0) {
    stop('parameter of the power-law function must be positive')
  }
  
  # assign the variables
  k <- par[1]
  
  # compute omega
  Om <- Ps^k
  
  return(Om)
}
