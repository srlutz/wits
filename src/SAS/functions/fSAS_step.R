fSAS_step <- function(Ps, par) {
  # function to compute a step fSAS function 
  # note this is a "left" step, so the function is uniform up to a point Ps=u
  # Om = Ps/u if Ps<u, Om = 1 if Ps>=u
  # Om = fSAS_pl(Ps,par=c(u))    #u defines the interval where Om is linear
  
  # Synopsis
  # Om = the cumulative Omega function, evaluated over each element of Ps
  # Ps = cumulative storage age distribution (S_T/S)
  # par  = vector with parameter k. 
  
  # do some error check
  if (length(par) != 1) {
    stop("wrong number of input parameters")
  }
  if (any(par) <= 0 && any(par) > 1) {
    stop("parameter of the step function must be 0 < par <=1")
  }
  
  # assign the variables
  u <- par[1]
  
  # compute omega
  Om <- Ps / u
  Om[Ps >= u] <- 1
  
  return(Om)
}